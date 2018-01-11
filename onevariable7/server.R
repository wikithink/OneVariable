#单个日期时间变量的规律分析
#王文祥
#2018-1-5
#server.R

#获取老朱传来的SQL命令字符串
#直接模拟登录后获取sql脚本字符串
# library(RCurl)
# #通过验证发现，cookie一直有效
# ##利用chrome浏览器的开发模式，在network模块中找到header信息
# head <- c("Connection" = "keep-alive",
#           "Host" = "192.168.1.35:8083",
#           "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36",
#           "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
#           "Accept-Language"="zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3",
#           #"Authorization"= "Basic d2lraXRoaW5rQHFxLmNvbToxNTkwMDU4MjkyOQ==",
#           #"Content-Length" = 523,
#           #"Content-Type" = "application/x-www-form-urlencoded",
#           "Cookie" ="JSESSIONID=75C9788FCBA596BD469411725126C356; OL_session_id=A8078E8A0A66807102D49D3843389E8F; user-id=wangwenxiang|Fri%2C%2015%20Sep%202017%2009%3A09%3A32%20GMT|pYwjgjHTKE%2BWwxgrIHA5uLgyq%2FvEVvbNcW0i0RlipZA%3D; csrf-token=edf8a0fd-71c1-45b6-83ae-cd11e29836c7"
#           #"Cookie"="_ga=GA1.2.1476654134.1502955215; _gid=GA1.2.2098580144.1503048852; _gat=1"
#           #"Cookie" ="_gat=1;_ga=GA1.2.1436180817.1503049170;_gid=GA1.2.1058783361.1503049170"
# )
# d2 =debugGatherer()
# cHandle2<- getCurlHandle(httpheader=head, followlocation=TRUE,
#                          debugfunction=d2$update,verbose=TRUE
#                          #cookiefile= "G:/cookie.txt"
#                         )
# 
# txt1 <- getURL("http://192.168.1.35:8083/dmse/analysis/residentsconstitute/rateAgeGender", curl = cHandle2)
##暂时未加密，无需登录

shinyServer(function(input, output,session) {
withProgress({
    setProgress(message = "数据加载中......")
  #先从老朱那里取到themeid和一堆系统参数stat开头、数据筛选条件
  #observe里面支持全局赋值，否则后面output里面不认识，不同于shinyServer函数外面的变量可以被认识
  observe({
    urlkey <<- parseQueryString(session$clientData$url_search)
    if(length(urlkey)==0){
    urlkey <<- list(theme_id='940',
                   D_last_diag_date1='1958',
                   #report_district='金山区',
                   stat_side_type='双侧',stat_alpha0='0.05',stat_conf='0.95',
                   stat_max0='2017-12-31',stat_min0='1900-01-01',stat_pg='月')
    }
    ##检验的单双侧(全局)
    side_type <<- urlkey$stat_side_type
    ##显著性水平(全局)
    alpha0 <<- as.numeric(urlkey$stat_alpha0)
    ##置信水平(全局)
    conf <<- as.numeric(urlkey$stat_conf)
    ##参考值范围(全局)
    max0 <<- as.Date(urlkey$stat_max0)
    min0 <<- as.Date(urlkey$stat_min0)
    ##时间粒度(全局)
    unit0 <<- urlkey$stat_pg
  })
  #reactive里面不能使用全局赋值,但是外面的全局赋值可以传进去
  z <- reactive({
    #然后根据themeid从老丁那里取到该主题相关的数据集、数据元、设计类型、业务对象名称、数据元计量单位等
    theme <- jsonlite::fromJSON(paste0("http://192.168.1.35:8083/assetls/json/theme/fetchTheme/",urlkey$theme_id),flatten = TRUE)
    ##业务对象(全局)
    obj <- theme$hrolename
    ##数据设计类型(全局)
    design_type <- theme$designtype
    ##数据来源
    dskey <-theme$dskey
    dsname <- theme$dsname
    dmkey <- theme$dmkey
    dmname <- theme$dmname
    dmunit <- theme$dmunit
    #获取筛选器语义表
    thd <- rbind(theme$dims[[1]],theme$dims[[2]],theme$dims[[3]])
    
    #将where条件拼接成字符串，分成3个部分：时间维度（D_ OR I_），非时间维度，业务对象
    urlkey1 <- urlkey[names(urlkey)!='theme_id'& (!names(urlkey)%like% 'stat_*')]
    #筛选条件（不包括业务对象的算法条件）非空时，生成筛选条件表
    if(length(urlkey1)>=1){
    thdm <- data.frame(dmkey=names(urlkey1),dmvalue=unname(unlist(urlkey1)))
    thdm$dmkey <- as.vector(thdm$dmkey)
    thdm$dmvalue <- as.vector(thdm$dmvalue)
    ##ifelse函数已经向量化，而if-else没有，会出警告
    thdm$dmkey <- ifelse((thdm$dmkey%like% 'D_*')|(thdm$dmkey%like% 'I_*'),substr(thdm$dmkey,3,nchar(thdm$dmkey)),thdm$dmkey) 
    
    #filterT <- merge(thdm,thd,by='dmkey')#会导致顺序发生变化，改用left_join
    filterT <- dplyr::left_join(thdm,thd,by='dmkey')
    filterT <- rbind(data.frame(dmkey='',dmvalue='',dmname='筛选条件：'),filterT)
    }else{
      filterT <- data.frame(dmkey=character(0),dmvalue=character(0),dmname=character(0))
      filterT <- rbind(data.frame(dmkey='',dmvalue='无',dmname='筛选条件：'),filterT)
    }
    
    #生成拼接的sql语句
    #如果筛选器全为空则wherep1赋值为空
    ##如果变量名的前缀为D_则需要加上year()函数求年份，如果前缀为I_则表示为整数型，无需改变
    wherep <- character(0)
    wherep1 <- character(0)

    if(length(urlkey1)>=1){
      for(ii in 1:length(urlkey1)){
        if(names(urlkey1)[ii]%like% 'D_*'){
          names(urlkey1)[ii] <-paste0('year(',substr(names(urlkey1)[ii],3,nchar(names(urlkey1)[ii])),')') 
        }
        else if(names(urlkey1)[ii]%like% 'I_*'){
          names(urlkey1)[ii] <-substr(names(urlkey1)[ii],3,nchar(names(urlkey1)[ii]))
        }else{
          names(urlkey1)[ii] <-names(urlkey1)[ii]
        }
        wherep[ii] <- paste0(names(urlkey1)[ii],"='",urlkey1[[ii]],"'")
        wherep1 <- paste0(wherep1,' and ',wherep[ii])
      } 
    } else {
      wherep1 <- ''
      }
    withProgress({
      setProgress(message = "正在数据读取......")
    ##将SQL脚本传入dbGetQuery中获取mysql的数据
    conn <- dbConnect(MySQL(),
                      user="dmagic",
                      password="dmagic",
                      dbname="zyws",
                      host="192.168.1.35",
                      port=3306)
    #设置读取字符格式
    dbGetQuery(conn,"SET NAMES utf8")
    ##实际读数据时会将筛选器条件写在where语句中传进来，这里需要处理年份和日期的问题，否则取不到数据
    #date具体日期 nf年份 jd季度 yf月份 zhou一年的第几周 ri日  tian一年的第几天 xq星期几（1为星期一，7为星期天） xs小时（24小时）
    data <- dbGetQuery(conn,paste0('select ',dmkey,' date,year(',dmkey,') nf,quarter(',dmkey,') jd,month(',dmkey,') yf,WEEKOFYEAR(',dmkey,') zhou,day(',dmkey,') ri,DAYOFYEAR(',dmkey,') tian,WEEKDAY(',dmkey,')+1 xq,hour(time(',dmkey,')) xs from ',dskey,' where ',theme$hrolesql,wherep1,' order by ',dmkey,' desc')) 
    dbDisconnect(conn)
    #data0包含缺失值，出于性能的考虑，只取倒序最近的2000条(非缺失值)+缺失值
    data0 <<- rbind(head(data[!is.na(data[,1]),],2000),data[is.na(data[,1]),]) 
    data <-na.omit(data0)#data不包含缺失值
    })
    #基本统计表：计算各类指标值
    #unit0=c('年','季','月','周','日','小时','日夜')
    withProgress({
      setProgress(message = "正在准备分组数据......")
    if(nrow(data)>0){
    #--到年(h2y)
    h1y <- as.data.frame(table(data$nf),stringsAsFactors = F)
    h1y$Var1 <- as.numeric(h1y$Var1)
    names(h1y) <- c('ny','num')
    h2y <- data.frame(ny=seq(from=min(data$nf),to=max(data$nf),by=1),stringsAsFactors = F)
    h2y <- left_join(h2y,h1y,by='ny')#依赖dplyr包
    h2y$num[is.na(h2y$num)] <- 0
    #--到年季(h2q)
    data$njd <- paste0(data$nf,'-',data$jd)
    h1q <- as.data.frame(table(data$njd),stringsAsFactors = F)
    names(h1q) <- c('ny','num')
    h2q <- data.frame(jd=as.Date(seq(from=as.Date(paste0(min(data$nf),'-1-1')),to=as.Date(paste0(max(data$nf),'-12-31')),by='quarter')),stringsAsFactors = F)
    h2q$ny <- paste0(year(h2q$jd),'-',quarter(h2q$jd))
    h2q <- left_join(h2q,h1q,by='ny')#依赖dplyr包
    h2q$num[is.na(h2q$num)] <- 0
    h2q$jd <- NULL
    #--到季(h2q0)
    h1q0 <- as.data.frame(table(data$jd),stringsAsFactors = F)
    h1q0$Var1 <- as.numeric(h1q0$Var1)
    names(h1q0) <- c('ny','num')
    h2q0 <- data.frame(ny=seq(from=1,to=4,by=1),stringsAsFactors = F)
    h2q0 <- left_join(h2q0,h1q0,by='ny')#依赖dplyr包
    h2q0$num[is.na(h2q0$num)] <- 0
    
    #--到年月(h2m)
    data$ny <- paste0(data$nf,'-',data$yf)
    h1m <- as.data.frame(table(data$ny),stringsAsFactors = F)
    names(h1m) <- c('ny','num')
    #按顺序构建数据
    h2m <- data.frame(month=as.Date(seq(from=as.Date(paste0(min(data$nf),'-1-1')),to=as.Date(paste0(max(data$nf),'-12-31')),by='month')),stringsAsFactors = F)
    h2m$ny <- paste0(year(h2m$month),'-',month(h2m$month))
    h2m <- left_join(h2m,h1m,by='ny')#依赖dplyr包
    h2m$num[is.na(h2m$num)] <- 0
    h2m$month <- NULL
    #--到月(h2m0)
    h1m0 <- as.data.frame(table(data$yf),stringsAsFactors = F)
    h1m0$Var1 <- as.numeric(h1m0$Var1)
    names(h1m0) <- c('ny','num')
    h2m0 <- data.frame(ny=seq(from=1,to=12,by=1),stringsAsFactors = F)
    h2m0 <- left_join(h2m0,h1m0,by='ny')#依赖dplyr包
    h2m0$num[is.na(h2m0$num)] <- 0
    
    #--到年周(h2w) 比较麻烦，有的年份默认缺少第1周
    data$nzhou <- paste0(data$nf,'-',data$zhou)
    h1w <- as.data.frame(table(data$nzhou),stringsAsFactors = F)
    names(h1w) <- c('ny','num')
    #h2wp <- data.frame(year=seq(from=min(data$nf),to=max(data$nf),by=1),stringsAsFactors = F)
    #h2wp$week <- week(as.Date(paste0(h2wp$year,'-12-31')))#还有isoweek函数，涉及到一周的开始，还有闰年，所以不搞那么复杂了，取53周
    #h2w <- data.frame(zhou=as.Date(seq(from=as.Date(paste0(min(data$nf),'-01-01')),to=as.Date(paste0(max(data$nf),'-12-31')),by='week')),stringsAsFactors = F)#这种方法会导致某一年没有第一周
    h2w <- data.frame(year=sort(rep(seq(from=min(data$nf),to=max(data$nf),by=1),53)),stringsAsFactors = F)#每一年默认均取53周
    h2w$ny <- paste0(h2w$year,'-',c(1:53))
    h2w <- left_join(h2w,h1w,by='ny')#依赖dplyr包
    h2w$num[is.na(h2w$num)] <- 0
    h2w$year <- NULL
    #--到一年的第几周(h2w0)
    h1w0 <- as.data.frame(table(data$zhou),stringsAsFactors = F)
    h1w0$Var1 <- as.numeric(h1w0$Var1)
    names(h1w0) <- c('ny','num')
    h2w0 <- data.frame(ny=seq(from=1,to=53,by=1),stringsAsFactors = F)
    h2w0 <- left_join(h2w0,h1w0,by='ny')#依赖dplyr包
    h2w0$num[is.na(h2w0$num)] <- 0
    
    #--到天(h2d)
    h1d <- as.data.frame(table(as.Date(data$date)),stringsAsFactors = F)
    h1d$Var1 <- as.Date(h1d$Var1)
    names(h1d) <- c('ny','num')
    h2d <- data.frame(ny=seq(from=min(h1d$ny),to=max(h1d$ny),by='day'),stringsAsFactors = F)
    h2d <- left_join(h2d,h1d,by='ny')#依赖dplyr包
    h2d$num[is.na(h2d$num)] <- 0
    h2d$ny <- as.character(h2d$ny)
    #--到一年的第几天(h2d0)
    h1d0 <- as.data.frame(table(data$tian),stringsAsFactors = F)
    h1d0$Var1 <- as.numeric(h1d0$Var1)
    names(h1d0) <- c('ny','num')
    h2d0 <- data.frame(ny=seq(from=1,to=366,by=1),stringsAsFactors = F)
    h2d0 <- left_join(h2d0,h1d0,by='ny')#依赖dplyr包
    h2d0$num[is.na(h2d0$num)] <- 0
    
    #--到星期几(h2wd)
    h1wd <- as.data.frame(table(data$xq),stringsAsFactors = F)
    h1wd$Var1 <- as.numeric(h1wd$Var1)
    names(h1wd) <- c('ny','num')
    h2wd <- data.frame(ny=seq(from=1,to=7,by=1),stringsAsFactors = F)
    h2wd <- left_join(h2wd,h1wd,by='ny')#依赖dplyr包
    h2wd$num[is.na(h2wd$num)] <- 0
    
    #--到小时(h2h)
    h1h <- as.data.frame(table(data$xs),stringsAsFactors = F)
    h1h$Var1 <- as.numeric(h1h$Var1)
    names(h1h) <- c('ny','num')
    h2h <- data.frame(ny=seq(from=0,to=23,by=1),stringsAsFactors = F)
    h2h <- left_join(h2h,h1h,by='ny')#依赖dplyr包
    h2h$num[is.na(h2h$num)] <- 0
    h2h$ny <- paste0(h2h$ny,'h')
    }else{
    h1m <- data.frame(ny=character(0),num=numeric(0),stringsAsFactors = F)
    h2m <- data.frame(ny=character(0),num=numeric(0),stringsAsFactors = F)
    h1y <- h1q <- h1q0 <- h1m0 <- h1w <- h1w0 <- h1wd <- h1d <- h1d0 <- h1h <- h1m
    h2y <- h2q <- h2q0 <- h2m0 <- h2w <- h2w0 <- h2wd <- h2d <- h2d0 <- h2h <- h2m
    }
    })
    
    #1.1 频率分布表(h5)
    #ny=分组,num=频数,e3=频率(%),e4=累计频数,e5=累计频率 e6 e7去除缺失值后的累计频数和累计频率
    #注意：频率分布表，只要有1个有效数据即可
    #日夜当小时处理
    h1 <- switch(unit0, '月'= h1m, '年'=h1y, '季'=h1q,'周'=h1w,'日'=h1d,'小时'=h1h,'日夜'=h1h)
    h2 <- switch(unit0, '月'= h2m, '年'=h2y, '季'=h2q,'周'=h2w,'日'=h2d,'小时'=h2h,'日夜'=h2h)
    
    if(nrow(data)>0){
    h51 <- as.data.frame(table(data0$yf,useNA = 'ifany'))
    if(isempty(h51$Freq[is.na(h51$Var1)])){
      num0 <- 0
    }else{
      num0 <- h51$Freq[is.na(h51$Var1)]
    }
    }else{
      h51 <- data.frame(Var1=character(0),Freq=numeric(0))
      num0 <- 0
    }
    
    h5 <- subset(h2,h2$num!=0,select = c('ny','num'))
    if(nrow(h5)>0||(nrow(h5)==0&&num0>0)){
    h5 <- rbind(h5,data.frame(ny='缺失值',num=num0,stringsAsFactors = F))
    h5$e3 <- h5$num/sum(h5$num)*100
    h5$e4 <- cumsum(h5$num)
    h5$e5 <- round(cumsum(h5$e3),2)
    #e6 e7去除缺失值后的累计频数和累计频率
    h5$e6 <- h5$e4
    h5$e7 <- round(cumsum(h5$num/(sum(h5$num)-num0)*100),2)
    h5$e6[nrow(h5)] <- NA
    h5$e7[nrow(h5)] <- NA

    h5 <- rbind(h5,
                data.frame(ny='合计',num=sum(h5$num),e3=sum(h5$e3),
                           e4=NA,e5=NA,e6=NA,e7=NA,stringsAsFactors = F))
    h5$e3 <- round(h5$e3,2)
    }else{
      h5 <- data.frame(ny=character(0),num=numeric(0),e3=numeric(0),e4=numeric(0),e5=numeric(0),e6=numeric(0),e7=numeric(0))
    }
    #1.2 疑似异常值分布表（dat2）
    ##非正态分布中采用箱式图来判断异常值，标准为<Q1-1.5*IQR 或 >Q3+1.5*IQR
    ##如符合正态分布，则考虑使用均数+-3个标准差来判断
    ##如果可以读入该数据元的参考值的限值范围，则使用该限值范围，目前未考虑这个代码
    if(nrow(data)>0){
      #将日期转为数字，按天或者秒计算到1900-1-1的值
      data$dt <- as.numeric(difftime(strptime(data$date,format='%Y-%m-%d %H:%M:%S'),as.Date('1900-01-01'),units='days'))
      b71 <- psych::describe(data$dt,quant=c(0.05,0.25,0.75,0.95),IQR=T,skew = F)
      #按照分布确定
      data$yc1 <- data$dt<(b71$Q0.25-1.5*b71$IQR)|data$dt>(b71$Q0.75+1.5*b71$IQR)
      #按照标准差确定
      data$yc2 <- data$dt<(b71$mean-3*b71$sd)|data$dt>(b71$mean+3*b71$sd)
      #按照总体参数确定
      data$yc3 <- difftime(as.Date(data$date),max0,units='days')>0|difftime(as.Date(data$date),min0,units='days')<0
      #取异常总数
      data$yc <- data$yc1|data$yc2|data$yc3
      ycnum <- sum(data$yc,na.rm = T)#异常总数
      dat2 <- data.frame(f1=c('正常值','疑似异常值','缺失值','合计'),
                         f2=c((nrow(data0)-num0-ycnum),ycnum,num0,nrow(data0)))
      dat2$f3 <- round(dat2$f2/(nrow(data0))*100,2)#频率
    }else{
      dat2 <- data.frame(f1=character(0),f2=numeric(0),f3=numeric(0))
    }

    #这里需要解决tfoot中td换行的问题，非常难解决
    sketch2  <-  htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-left',colspan = 1, '值类型'),
          th(class = 'dt-left',colspan = 1, '频数'),
          th(class = 'dt-left',colspan = 1, '频率(%)')
        )
      ),
      tfoot(
        tr(td(class = 'dt-left',colspan = 3,'**1.异常值判断方法：百分位数间距法+标准差法+总体参数值域法；'))
      )
    ))
    
    #2.3 规律性分析(长期趋势、季节、周期)
    # 季节性分解
    # 曲线相似度：Frechet distance
    # 总体趋势比较（长期趋势）
    # 总体斜率比较
    #要求至少3个元素
    if(nrow(data)>0){
      #转为时间序列格式，用ts函数而不是as.ts函数
      #后续可以考虑将月份限制在实际起始月份
      withProgress({
        setProgress(message = "正在进行季节性分解......")
      kts_y <- ts(h2y$num,start=min(data$nf),end=max(data$nf),frequency=1,names='year_series')#按照年份
      kts_yq <- ts(h2q$num,start=c(min(data$nf),1),end=c(max(data$nf),4),frequency=4,names='quarter_series')#按照年季
      kts_ym <- ts(h2m$num,start=c(min(data$nf),1),end=c(max(data$nf),12),frequency=12,names='month_series')#按照年月
      kts_yw <- ts(zoo(h2w$num, order.by=h2w$ny))#按照年周
      kts_yd <- ts(zoo(h2d$num, order.by=as.Date(h2d$ny)))#按照天
      #kts <- ts(h2d$num,start=min(h2d$ny),end=max(h2d$ny),frequency=1)#按照天
      kts_h <- ts(zoo(h2h$num, order.by=h2h$ny))#按照小时
      kts_q <- ts(zoo(h2q0$num, order.by=h2q0$ny))#按照季
      kts_m <- ts(zoo(h2m0$num, order.by=h2m0$ny))#按照月
      kts_w <- ts(zoo(h2w0$num, order.by=h2w0$ny))#按照第几周
      kts_d <- ts(zoo(h2d0$num, order.by=h2d0$ny))#按照第几天
      kts_xq <- ts(zoo(h2wd$num, order.by=h2wd$ny))#按照星期几
      
      kts <- switch(unit0, '月'= kts_ym, '年'=kts_y, '季'=kts_yq,'周'=kts_yw,'日'=kts_yd,'小时'=kts_h,'日夜'=kts_h)
      
      #lkts <- log(kts_ym)#有0不能取对数,取对数的目的是为了从相加模型转成相乘模型
      #季节性分解只能是月和季度数据时才行，且必须大于等于3年
      if((unit0=='月'||unit0=='季')&&(end(kts)[1]-start(kts)[1]>=3)){
      fit <- stl(kts,s.window = 'period')#利用LOESS原理
      #matrix(fit$time.series,nrow=(end(kts)[1]-start(kts)[1]+1)*4,ncol=3)
      }else{
       fit <- list() 
      }
      })
      #总体趋势
      #Mann-Kendall Trend Test  tau=Kendall's tau statistic S=Kendall Score
      # 当Z大于1.96则说明为有明显上升趋势
      # 当小于-1.96则说明有明显下降趋势
      # 大于0且小于1.96说明有上升但不明显
      # 小于0且大于-1.96则说明有下降趋势但不明显.
      #要求至少有3个元素
      withProgress({
        setProgress(message = "正在计算总体趋势......")
      if(nrow(h2y)>=3){
      mktr_y <- trend::mk.test(h2y$num)
      }else{
        mktr_y <- list()
        mktr_y$statistic <- NA
        mktr_y$p.value <- NA
      }
      if(nrow(h2q)>=3){
      mktr_yq <- trend::mk.test(h2q$num)
      }else{
        mktr_yq <- list()
        mktr_yq$statistic <- NA
        mktr_yq$p.value <- NA
      }
      if(nrow(h2q0)>=3){
      mktr_q <- trend::mk.test(h2q0$num)
      }else{
        mktr_q <- list()
        mktr_q$statistic <- NA
        mktr_q$p.value <- NA
      }
      if(nrow(h2m)>=3){
      mktr_ym <- trend::mk.test(h2m$num)
      }else{
        mktr_ym <- list()
        mktr_ym$statistic <- NA
        mktr_ym$p.value <- NA
      }
      if(nrow(h2m0)>=3){
      mktr_m <- trend::mk.test(h2m0$num)
      }else{
        mktr_m <- list()
        mktr_m$statistic <- NA
        mktr_m$p.value <- NA
      }
      if(nrow(h2w)>=3){
      mktr_yw <- trend::mk.test(h2w$num)
      }else{
        mktr_yw <- list()
        mktr_yw$statistic <- NA
        mktr_yw$p.value <- NA
      }
      if(nrow(h2w0)>=3){
      mktr_w <- trend::mk.test(h2w0$num)
      }else{
        mktr_w <- list()
        mktr_w$statistic <- NA
        mktr_w$p.value <- NA
      }
      if(nrow(h2wd)>=3){
      mktr_xq <- trend::mk.test(h2wd$num)
      }else{
        mktr_xq <- list()
        mktr_xq$statistic <- NA
        mktr_xq$p.value <- NA
      }
      if(nrow(h2d)>=3){
      mktr_yd <- trend::mk.test(h2d$num)
      }else{
        mktr_yd <- list()
        mktr_yd$statistic <- NA
        mktr_yd$p.value <- NA
      }
      if(nrow(h2d0)>=3){
      mktr_d <- trend::mk.test(h2d0$num)
      }else{
        mktr_d <- list()
        mktr_d$statistic <- NA
        mktr_d$p.value <- NA
      }
      if(nrow(h2h)>=3){
      mktr_h <- trend::mk.test(h2h$num)
      }else{
        mktr_h <- list()
        mktr_h$statistic <- NA
        mktr_h$p.value <- NA
      }
      total_trend <- data.frame(method='Mann-Kendall',
                                lidu=c('年份','年季','季','年月','月','年周','周','星期','日','年天','小时'),
                                size=c(nrow(h2y),nrow(h2q),nrow(h2q0),nrow(h2m),nrow(h2m0),nrow(h2w),nrow(h2w0),nrow(h2wd),nrow(h2d),nrow(h2d0),nrow(h2h)),
                                tjl='z',
                                tjz=c(unname(mktr_y$statistic),unname(mktr_yq$statistic),unname(mktr_q$statistic),unname(mktr_ym$statistic),unname(mktr_m$statistic),
                                      unname(mktr_yw$statistic),unname(mktr_w$statistic),unname(mktr_xq$statistic),unname(mktr_yd$statistic),unname(mktr_d$statistic),
                                      unname(mktr_h$statistic)),
                                p=c(mktr_y$p.value,mktr_yq$p.value,mktr_q$p.value,mktr_ym$p.value,mktr_m$p.value,mktr_yw$p.value,mktr_w$p.value,
                                    mktr_xq$p.value,mktr_yd$p.value,mktr_d$p.value,mktr_h$p.value),
                                jd=NA,stringsAsFactors = F)
      
     for(i in 1:nrow(total_trend)){
       if(is.na(total_trend$p[i])&&is.na(total_trend$tjz[i])){
         total_trend$jd[i] <- '无法计算'
       }else if(total_trend$p[i]<0.05&&total_trend$tjz[i]< -1.96){
        total_trend$jd[i] <- '存在明显下降趋势'
      }else if(total_trend$p[i]<0.05&&total_trend$tjz[i]>1.96){
        total_trend$jd[i] <- '存在明显上升趋势'
      }else if(total_trend$p[i]<0.05&&total_trend$tjz[i]>=-1.96&&total_trend$tjz[i]<0){
        total_trend$jd[i] <- '存在下降趋势但不明显'
      }else if(total_trend$p[i]<0.05&&total_trend$tjz[i]<=1.96&&total_trend$tjz[i]>0){
        total_trend$jd[i] <- '存在上升趋势但不明显'
      }else{
        total_trend$jd[i] <- '总体趋势不显著'
      }
     }
      total_trend$tjz <- round(total_trend$tjz,2)
      total_trend$p <- round(total_trend$p,4)
      })
      #总体斜率和季节斜率
      #计算总体斜率
      withProgress({
        setProgress(message = "正在计算斜率......")
      if(nrow(h2y)>=2){
      xl_y <- trend::sens.slope(h2y$num, conf.level = conf)#至少2个
      }else{
        xl_y <- list()
        xl_y$estimates <- NA
      }
      if(nrow(h2q)>=2){
      xl_yq <- trend::sens.slope(h2q$num, conf.level = conf)#至少2个
      }else{
        xl_yq <- list()
        xl_yq$estimates <- NA
      }
      if(nrow(h2q0)>=2){
      xl_q <- trend::sens.slope(h2q0$num, conf.level = conf)#至少2个
      }else{
        xl_q <- list()
        xl_q$estimates <- NA
      }
      if(nrow(h2m)>=2){
      xl_ym <- trend::sens.slope(h2m$num, conf.level = conf)#至少2个
      }else{
        xl_ym <- list()
        xl_ym$estimates <- NA
      }
      if(nrow(h2m0)>=2){
      xl_m <- trend::sens.slope(h2m0$num, conf.level = conf)#至少2个
      }else{
        xl_m <- list()
        xl_m$estimates <- NA
      }
      #数据量太大导致计算非常耗时，只计算3650个周
      if(nrow(h2w)>=2&&nrow(h2w)<=3650){
      xl_yw <- trend::sens.slope(h2w$num, conf.level = conf)#至少2个
      }else{
        xl_yw <- list()
        xl_yw$estimates <- NA
      }
      if(nrow(h2w0)>=2){
      xl_w <- trend::sens.slope(h2w0$num, conf.level = conf)#至少2个
      }else{
        xl_w <- list()
        xl_w$estimates <- NA
      }
      if(nrow(h2wd)>=2){
      xl_xq <- trend::sens.slope(h2wd$num, conf.level = conf)#至少2个
      }else{
        xl_xq <- list()
        xl_xq$estimates <- NA
      }
      if(nrow(h2d)>=2&&nrow(h2d)<=3650){
      xl_yd <- trend::sens.slope(h2d$num, conf.level = conf)#至少2个
      }else{
        xl_yd <- list()
        xl_yd$estimates <- NA
      }
      if(nrow(h2d0)>=2){
      xl_d <- trend::sens.slope(h2d0$num, conf.level = conf)#至少2个
      }else{
        xl_d <- list()
        xl_d$estimates <- NA
      }
      
      if(nrow(h2h)>=2){
      xl_h <- trend::sens.slope(h2h$num, conf.level = conf)#至少2个
      }else{
        xl_h <- list()
        xl_h$estimates <- NA
      }
      #计算季节斜率(要求至少两个年份)
      #kts_yq <- ts(c(1,2,3,4),start=c(1957,1),end=c(1957,4),frequency=4,names='quarter_series')#按照年季
      if((end(kts_yq)[1]-start(kts_yq)[1])>=1){
      xl_yq1 <- sea.sens.slope(kts_yq)
      }else{
        xl_yq1 <- '无法计算' 
      }
      if((end(kts_ym)[1]-start(kts_ym)[1])>=1){
      xl_ym1 <- sea.sens.slope(kts_ym)
      }else{
        xl_ym1 <- '无法计算'
      }
      total_slope <- data.frame(method="Sen's",
                                lidu=c('年份','年季','季','年月','月','年周','周','星期','日','年天','小时'),
                                size=c(nrow(h2y),nrow(h2q),nrow(h2q0),nrow(h2m),nrow(h2m0),nrow(h2w),nrow(h2w0),nrow(h2wd),nrow(h2d),nrow(h2d0),nrow(h2h)),
                                slope1=c(xl_y$estimates,xl_yq$estimates,xl_q$estimates,xl_ym$estimates,xl_m$estimates,
                                         xl_yw$estimates,xl_w$estimates,xl_xq$estimates,xl_yd$estimates,
                                         xl_d$estimates,xl_h$estimates),
                                slope2=c(NA,xl_yq1,NA,xl_ym1,NA,NA,NA,NA,NA,NA,NA),
                                jd=NA,stringsAsFactors = F)
      
      for(i in 1:nrow(total_slope)){
        if(is.na(total_slope$slope1[i])){
          total_slope$jd[i] <- '无法计算'
        }else if(total_slope$slope1[i] <0){
          total_slope$jd[i] <- '总体存在下降趋势'
        }else if(total_slope$slope1[i]==0){
          total_slope$jd[i] <- '无显著趋势'
        }else{
          total_slope$jd[i] <- '总体存在上升趋势'
        }
      }
      
      total_slope$slope1 <- round(total_slope$slope1,4)
      if(total_slope$slope2[2]!='无法计算'){total_slope$slope2 <- round(total_slope$slope2,4)}
  
      })
      #相关系数（适用于不同聚合方式，而非不同时间粒度）
      #比较：年、季度、月份、周、年天、小时
      #要求x和y有相同的长度
      #cor(h2y$num,h2q0$num,method = 'pearson')

      #曲线比较hausdorff distance/DTW/EDIT_distance/LCSS 和 Frechet distance
      withProgress({
        setProgress(message = "正在计算曲线相似度......")
      m_h2y <- as.matrix(h2y$num,ncol=1)
      m_h2q <- as.matrix(h2q$num,ncol=1)
      m_h2q0 <- as.matrix(h2q0$num,ncol=1)
      #dimnames(m_h2q0) <- list(c('Q1','Q2','Q3','Q4'),c('NUM'))
      m_h2m <- as.matrix(h2m$num,ncol=1)
      m_h2m0 <- as.matrix(h2m0$num,ncol=1)
      m_h2w <- as.matrix(h2w$num,ncol=1)
      m_h2w0 <- as.matrix(h2w0$num,ncol=1)
      m_h2wd <- as.matrix(h2wd$num,ncol=1)
      m_h2d <- as.matrix(h2d$num,ncol=1)
      m_h2d0 <- as.matrix(h2d0$num,ncol=1)
      m_h2h <- as.matrix(h2h$num,ncol=1)
      m_name <- c('m_h2y','m_h2q','m_h2q0','m_h2m','m_h2m0','m_h2w','m_h2w0','m_h2wd','m_h2d','m_h2d0','m_h2h')
      # #比较耗时
      # hausdorff1 <- pracma::hausdorff_dist(m_h2m,m_h2w)
      # dtw1 <- SimilarityMeasures::DTW(m_h2y, m_h2w, pointSpacing=-1)
      # edit1 <- SimilarityMeasures::EditDist(m_h2y, m_h2w, pointDistance=20)
      # lcss1 <- SimilarityMeasures::LCSS(m_h2y, m_h2y, pointSpacing=-1, pointDistance=20, errorMarg=2, returnTrans=F)#耗时2分钟
      # frechet1 <- SimilarityMeasures::Frechet(m_h2y,m_h2q,testLeash=-1) 
      dist <- matrix(rep(NA,11*11),11,11,dimnames = list(m_name,m_name))#构建一个矩阵用于存储5种不同类型的距离，依次为hausdorff,DTW,Edit,LCSS,Frechet
      for(i in 1:11){
        for(j in 1:11){
          dist[i,j] <-paste(eval(parse(text=paste0("ifelse(dim(",m_name[i],")[1]+dim(",m_name[j],")[1]<=4000,hausdorff_dist(",m_name[i],",",m_name[j],"),NA)"))),
          eval(parse(text=paste0("ifelse(dim(",m_name[i],")[1]+dim(",m_name[j],")[1]<=1000,DTW(",m_name[i],",",m_name[j],",pointSpacing=-1),NA)"))),
          eval(parse(text=paste0("ifelse(dim(",m_name[i],")[1]+dim(",m_name[j],")[1]<=400,EditDist(",m_name[i],",",m_name[j],",pointDistance=20),NA)"))),
          eval(parse(text=paste0("ifelse(dim(",m_name[i],")[1]+dim(",m_name[j],")[1]<=30,LCSS(",m_name[i],",",m_name[j],",pointSpacing=-1,pointDistance=20,errorMarg=2,returnTrans=F),NA)"))),
          eval(parse(text=paste0("ifelse(dim(",m_name[i],")[1]+dim(",m_name[j],")[1]<=30,Frechet(",m_name[i],",",m_name[j],",testLeash=-1),NA)"))),sep=',')
        }
      }
      dist <- gsub('NA,NA,NA,NA,NA','耗时过长',dist)
      dist <- gsub('NA','Nd',dist)
      })
      
      #卷积神经网络LeNet-5(待续...) 可以使用R中的keras深度学习框架
      #https://www.analyticsvidhya.com/blog/2017/06/getting-started-with-deep-learning-using-keras-in-r/
      # devtools::install_github("rstudio/keras")
      # library(keras)
      # install_tensorflow()
      # install_tensorflow(gpu=TRUE)

    }else{
      #时间序列
      kts_y <- NULL
      kts_yq <- NULL
      kts_ym <- NULL
      kts_yw <- NULL
      kts_yd <- NULL
      kts_q <- NULL
      kts_m <- NULL
      kts_w <- NULL
      kts_d <- NULL
      kts_xq <- NULL
      kts_h <- NULL
      kts <- NULL
      #季节性分解
      fit <- list()
      #1 总体趋势
      total_trend <- data.frame(method=character(0),lidu=character(0),size=numeric(0),tjl=character(0),tjz=numeric(0),p=numeric(0),jd=character(0),stringsAsFactors = F)
      #2 总体斜率
      total_slope <- data.frame(method=character(0),lidu=character(0),size=numeric(0),slope1=numeric(0),slope2=numeric(0),jd=character(0),stringsAsFactors = F)
      #3 曲线相似度
      m_name <- c('m_h2y','m_h2q','m_h2q0','m_h2m','m_h2m0','m_h2w','m_h2w0','m_h2wd','m_h2d','m_h2d0','m_h2h')
      dist <- matrix(rep(NA,11*11),11,11,dimnames = list(m_name,m_name))
    }
    
    #3.1参数估计
    #包括3个部分：总体趋势、总体斜率、曲线相似度


    ##解读的文字段落
    if(nrow(data)>0){
      tt1 <- nrow(total_trend[total_trend$jd=='存在明显下降趋势',])
      tt2 <- nrow(total_trend[total_trend$jd=='存在明显上升趋势',])
      tt3 <- nrow(total_trend[total_trend$jd=='存在下降趋势但不明显',])
      tt4 <- nrow(total_trend[total_trend$jd=='存在上升趋势但不明显',])
      ts1 <- nrow(total_slope[total_slope$jd=='总体存在下降趋势',])
      ts2 <- nrow(total_slope[total_slope$jd=='总体存在上升趋势',])
      
    p1 <- paste0('1.',dmname,'数据的总频数为',nrow(data0),'，有效频数为',nrow(data0)-num0,'，占比',round((nrow(data0)-num0)/nrow(data0)*100,1),'%，缺失频数为',num0,'，占比',round(num0/nrow(data0)*100,1),'%；')
    p2 <- paste0('2.在非缺失值中，疑似异常值个数为',dat2$f2[2],'个，占比',dat2$f3[2],'%；\n（异常值判断方法：百分位数间距法+标准差法+总体参数值域法）')
    p3 <- paste0('3.在11个时间粒度的趋势中，其中有',tt1,'个存在明显下降趋势，',tt2,'个存在明显上升趋势，',tt3,'个存在下降趋势但不明显，',tt4,'个存在上升趋势但不明显；')
    p4 <- paste0('4.将超过3年的季度数据或者月度数据进行季节性分解，以获取季节性的规律；未超过3年的数据不能进行季节性分解；')
    p5 <- paste0('5.从11个时间粒度计算其Sen氏总体斜率和季节斜率，其中有',ts1,'个斜率为负，存在总体下降趋势，',ts2,'个斜率为正，存在总体上升趋势；')
    p6 <- paste0("6.利用5种曲线间的距离，分别是Hausdorff，DTW，EDR，LCSS，Frechet，两两估计11个时间粒度间的曲线相似度，\n距离越小，曲线的形状和走势越相似；")
    }else if(nrow(data)==0&&nrow(data0)>0){
      p1 <- paste0('1.',dmname,'数据中总频数为',nrow(data0),'，有效频数为',nrow(data0)-num0,'，占比',round((nrow(data0)-num0)/nrow(data0)*100,1),'%，缺失频数为',num0,'，占比',round(num0/nrow(data0)*100,1),'%；')
      p2 <- paste0('2.无法计算疑似异常值！')
      p3 <- paste0('3.数据缺失，无法进行时间趋势检验！')
      p4 <- paste0('4.数据缺失，无法进行季节性分解！')
      p5 <- paste0('5.数据缺失，无法计算总体斜率和季节斜率！')
      p6 <- paste0('6.数据缺失，无法计算曲线相似度！')
    }else{
     p1 <- '无数据'
     p2 <- ''
     p3 <- ''
     p4 <- ''
     p5 <- ''
     p6 <- ''
    }
    z <- list(obj=obj,dmname=dmname,dmunit=dmunit,filterT=filterT,
              #data=data,data0=data0,
              h1=h1,h1d=h1d,
              h2=h2,h5=h5,dat2=dat2,sketch2=sketch2,
              kts_y=kts_y,kts_yq=kts_yq,kts_ym=kts_ym,kts_yw=kts_yw,kts_yd=kts_yd,kts_q=kts_q,kts_m=kts_m,kts_w=kts_w,
              kts_d=kts_d,kts_xq=kts_xq,kts_h=kts_h,
              kts=kts,fit=fit,
              total_trend=total_trend,total_slope=total_slope,dist=dist,
              p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6)
              
    
    return(z)
  })
})
  
#筛选条件表
output$filter1 <- renderTable({
    b <- z()
    filterT <- b$filterT
    #names(filterT) <- c('dmkey','筛选字段','筛选值')
    filterT[,c(3,2)]
  },colnames = F)
output$filter2 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)
output$filter3 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)
output$filter4 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)
output$filter5 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)
output$filter6 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)
output$filter7 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)

#1.1 频率分布表(h5)
output$table1 <- DT::renderDataTable({
  b <- z()
  h5 <- b$h5
  names(h5) <- c('时序','频数','频率(%)','累计频数','累计频率(%)','累计频数*','累计频率(%)*')
  DT::datatable(h5,
                #container = sketch1,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',unit0,'）的频率分布表'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom =ifelse(nrow(h5)<=50,'t','tip')))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(1,target = 'row',
                backgroundColor = styleEqual('缺失值',  '#FFFF99'))
})

#1.2疑似异常值分布表(dat2 sketch2)
output$table2 <- DT::renderDataTable({
  b <- z()
  dat2 <- b$dat2
  sketch2 <- b$sketch2
  #names(dat2) <- c('值类型','频数','频率(%)')
  DT::datatable(dat2,
                container = sketch2,
                escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',unit0,'）的异常值分布表'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom = "t"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(1,target = 'row',
                backgroundColor = styleEqual('缺失值',  '#FFFF99'))
})
# output$text21 <- renderUI({
#   h4("单个日期时间变量无法进行异常值检验！",style="color:red;font-weight:bold;",align='center')
# })

#2.2 季节性分解
output$table3 <- DT::renderDataTable({
  b <- z()
  kts <- b$kts
  fit <- b$fit
  if(!is.null(kts)&&!is.null(fit)&&(unit0=='月'||unit0=='季')&&(end(kts)[1]-start(kts)[1]>=3)){
  dimnames(fit$time.series) <- list(paste0(sort(rep(seq(start(fit$time.series)[1],end(fit$time.series)[1],1),frequency(fit$time.series))),'-Q',c(1:frequency(fit$time.series))),
                         c('季节效应','趋势效应','随机效应'))
  }else{
    fit <- list()
    fit$time.series <- data.frame(a1=numeric(0),a2=numeric(0),a3=numeric(0),stringsAsFactors = F)
    names(fit$time.series) <- c('季节效应','趋势效应','随机效应')
  }
  DT::datatable(round(fit$time.series,2),
                #container = sketch2,
                #escape = FALSE,
                rownames=T,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的季节性分解结果表'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = '_all')),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom = ifelse(class(fit$time.series)[1]=='data.frame','t','tip')))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1,backgroundColor = '#FFFF99')%>%
    formatStyle(3,backgroundColor = '#FFFF99')
})
output$plot2 <- renderPlot({
  b <- z()
  kts <- b$kts
  fit <- b$fit
  if(!is.null(kts)&&!is.null(fit)&&(unit0=='月'||unit0=='季')&&(end(kts)[1]-start(kts)[1]>=3)){
    show("plot2")
  } else {
    hide("plot2")
  }
  if(!is.null(kts)&&!is.null(fit)&&(unit0=='月'||unit0=='季')&&(end(kts)[1]-start(kts)[1]>=3)){
    #STL的作用是更换横轴的mtext为空，原来默认为time
    STL <- proto(plot.stl = stats:::plot.stl,
                 mtext = function(text, ...)
                   graphics::mtext(if (text == "time") xlab else text, ...),
                 xlab = "")
    with(STL, plot.stl(fit,labels=c('总体','季节','趋势','随机'),
         col='darkgreen',col.range = "orange",
         main=paste0(b$obj,b$dmname,'的季节性分解图'))
         )
    # plot(fit,labels=c('总体','季节','趋势','随机'),
    #          col='darkgreen',col.range = "orange",
    #          main=paste0(b$obj,b$dmname,'的季节性分解图'))
  }else{
    return(NULL)
  }
})

#3.1 总体趋势表
output$table4 <- DT::renderDataTable({
  b <- z()
  total_trend <- b$total_trend
  total_trend <- total_trend[order(total_trend$tjz),]
  names(total_trend) <- c('计算方法','时间粒度','时序数','统计量','统计值','P值','意义')
  DT::datatable(total_trend,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的总体趋势表'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom = "t"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')
})
output$plot1 <- renderDygraph({
  b <- z()
  h1d <- b$h1d
  #只有1个点的话，时序图不好看，故屏蔽掉
  if (nrow(h1d)>1) {
    show("plot1")
  } else {
    hide("plot1")
  }
  if (nrow(h1d)>1) {
    xtsd <- xts(h1d$num, as.Date(h1d$ny, format='%Y-%m-%d'))
    names(xtsd) <- 'all'#重命名时间序列
    dygraph(xtsd,main = paste0(b$obj,b$dmname,'的时序图')) %>%
      dyAxis("y", label = "频数",valueRange = c(0, max(h1d$num))) %>%
      dySeries("all", label = "全部",axis='y', color = "green") %>%
      dyOptions(stackedGraph = FALSE,drawGrid = TRUE,axisLineColor = "navy", 
                gridLineColor = "lightblue") %>%
      dyRangeSelector(height = 20,dateWindow = c(min(h1d$ny), max(h1d$ny)))%>%
      dyLegend(width=400,hideOnMouseOut = FALSE)
  }else{
    return(NULL)
  }
})
#3.2 总体斜率表
output$table5 <- DT::renderDataTable({
  b <- z()
  total_slope <- b$total_slope
  #total_slope <- total_slope[order(total_slope$lidu,decreasing = T),]
  names(total_slope) <- c('计算方法','时间粒度','时序数','总体斜率','季节斜率','意义')
  DT::datatable(total_slope,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的总体斜率表'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom = "t"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')
})
#3.3 曲线相似度
output$table6 <- DT::renderDataTable({
  b <- z()
  dist <- b$dist
  dimnames(dist) <- list(c('年份','年季','季','年月','月','年周','周','星期','日','年天','小时'),
                         c('年份','年季','季','年月','月','年周','周','星期','日','年天','小时'))
  DT::datatable(dist,
                #container = sketch2,
                #escape = FALSE,
                rownames=T,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的曲线相似度估计表(Hausdorff/DTW/EDR/LCSS/Frechet)'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = '_all')),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom = "t"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1,backgroundColor = '#FFFF99')%>%
    formatStyle(3,backgroundColor = '#FFFF99')%>%
    formatStyle(5,backgroundColor = '#FFFF99')%>%
    formatStyle(7,backgroundColor = '#FFFF99')%>%
    formatStyle(9,backgroundColor = '#FFFF99')%>%
    formatStyle(11,backgroundColor = '#FFFF99')
})
output$plot3 <- renderPlot({
  b <- z()
  #data <- b$data
  kts_y <- b$kts_y
  kts_yq <- b$kts_yq
  kts_ym <- b$kts_ym
  kts_yw <- b$kts_yw
  kts_yd <- b$kts_yd
  kts_q <- b$kts_q
  kts_m <- b$kts_m
  kts_w <- b$kts_w
  kts_d <- b$kts_d
  kts_xq <- b$kts_xq
  kts_h <- b$kts_h
  if (!is.null(kts_y)){
    show("plot3")
  } else {
    hide("plot3")
  }
  if (!is.null(kts_y)){
    #依次为时间序列图、一阶差分序列图、ACF图、PACF图,2行2列
    opar <- par(no.readonly = TRUE)
    par(mfrow=c(4,3),mar=c(2,4,3,2))#mar的顺序依次为下、左、上、右
    plot(kts_y,ylab='频数（年份）',main='',xlab='',col='red')
    plot(kts_yq,ylab='频数（年季）',main='',xlab='',col='red')
    plot(kts_ym,ylab='频数（年月）',main='',xlab='',col='red')
    plot(kts_yw,ylab='频数（年周）',main='',xlab='',col='darkgreen')
    plot(kts_yd,ylab='频数（日）',main='',xlab='',col='orange')
    plot(kts_q,ylab='频数（季）',main='',xlab='',col='orange')
    plot(kts_m,ylab='频数（月）',main='',xlab='',col='darkgreen')
    plot(kts_w,ylab='频数（周）',main='',xlab='',col='darkgreen')
    plot(kts_d,ylab='频数（年天）',main='',xlab='',col='darkgreen')
    plot(kts_xq,ylab='频数（星期）',main='',xlab='',col='darkgreen')
    plot(kts_h,ylab='频数（小时）',main='',xlab='',col='darkgreen')
    mtext(paste0(b$obj,b$dmname,'各个时间粒度的时序图'),side=3,outer=T,line=-2,cex=1.2,col='grey23')#为组合图形增加标题
    par(opar)
  }else{
    return(NULL)
  }
})

#总体解读
output$text1 <- renderPrint({
  b <- z()
  #print(urlkey)
  #cat(b$p1,b$p2,sep='\n\n')
  cat(b$p1,b$p2,b$p3,b$p4,b$p5,b$p6,sep='\n\n')
})

})
