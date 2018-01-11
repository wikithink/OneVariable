#单个日期时间变量的时间趋势分析
#王文祥
#2018-1-2
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
    #urlkey <<- list()
    if(length(urlkey)==0){
    urlkey <<- list(theme_id='943',
                   D_last_diag_date1='2008',
                   #report_district='金山区',
                   stat_side_type='双侧',stat_alpha0='0.05',stat_conf='0.95',
                   stat_max0='2017-12-31',stat_min0='1900-01-01',stat_pg='月',stat_indicator='计数')
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
    ##统计指标(全局)
    indicator0 <<- urlkey$stat_indicator
   
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
      setProgress(message = "数据加载中......")
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
    #nf年份 jd季度 yf月份 zhou一年的第几周 ri日  tian一年的第几天 xq星期几（1为星期一，7为星期天） xs小时（24小时）
    data <- dbGetQuery(conn,paste0('select ',dmkey,',year(',dmkey,') nf,quarter(',dmkey,') jd,month(',dmkey,') yf,WEEKOFYEAR(',dmkey,') zhou,day(',dmkey,') ri,DAYOFYEAR(',dmkey,') tian,WEEKDAY(',dmkey,')+1 xq,hour(time(',dmkey,')) xs from ',dskey,' where ',theme$hrolesql,wherep1,' order by ',dmkey,' desc')) 
    dbDisconnect(conn)
    #data0包含缺失值，处于性能的考虑，只取倒序最近的2000条(非缺失值)+缺失值
    data0 <<- rbind(head(data[!is.na(data[,1]),],2000),data[is.na(data[,1]),]) 
    data <-na.omit(data0)#data不包含缺失值
    })
    #1.1 计算各类指标值（h2）
    withProgress({
      setProgress(message = "正在计算各类指标......")
    if(nrow(data)>0){
      if(unit0=='月'){
        data$ny <- paste0(data$nf,'-',data$yf)
        h1 <- as.data.frame(table(data$ny),stringsAsFactors = F)
        names(h1) <- c('ny','num')
        #按顺序构建数据
        h2 <- data.frame(month=as.Date(seq(from=as.Date(paste0(min(data$nf),'-1-1')),to=as.Date(paste0(max(data$nf),'-12-31')),by='month')),stringsAsFactors = F)
        h2$ny <- paste0(year(h2$month),'-',month(h2$month))
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        h2$month <- NULL
      }else if(unit0=='年'){
        h1 <- as.data.frame(table(data$nf),stringsAsFactors = F)
        h1$Var1 <- as.numeric(h1$Var1)
        names(h1) <- c('ny','num')
        h2 <- data.frame(ny=seq(from=min(data$nf),to=max(data$nf),by=1),stringsAsFactors = F)
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        
      }else if(unit0=='季'){
        data$njd <- paste0(data$nf,'-',data$jd)
        h1 <- as.data.frame(table(data$njd),stringsAsFactors = F)
        names(h1) <- c('ny','num')
        h2 <- data.frame(jd=as.Date(seq(from=as.Date(paste0(min(data$nf),'-1-1')),to=as.Date(paste0(max(data$nf),'-12-31')),by='quarter')),stringsAsFactors = F)
        h2$ny <- paste0(year(h2$jd),'-',quarter(h2$jd))
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        h2$jd <- NULL
      }else if(unit0=='周'){
        data$nzhou <- paste0(data$nf,'-',data$zhou)
        h1 <- as.data.frame(table(data$nzhou),stringsAsFactors = F)
        names(h1) <- c('ny','num')
        h2 <- data.frame(year=sort(rep(seq(from=min(data$nf),to=max(data$nf),by=1),53)),stringsAsFactors = F)#每一年默认均取53周
        h2$ny <- paste0(h2$year,'-',c(1:53))
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        h2$year <- NULL
      }else if(unit0=='日'){
        h1 <- as.data.frame(table(as.Date(data$report_date)),stringsAsFactors = F)
        h1$Var1 <- as.Date(h1$Var1)
        names(h1) <- c('ny','num')
        h2 <- data.frame(ny=seq(from=min(h1$ny),to=max(h1$ny),by='day'),stringsAsFactors = F)
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        h2$ny <- as.character(h2$ny)
      }else if(unit0=='小时'){
        h1 <- as.data.frame(table(data$xs),stringsAsFactors = F)
        h1$Var1 <- as.numeric(h1$Var1)
        names(h1) <- c('ny','num')
        h2 <- data.frame(ny=seq(from=0,to=23,by=1),stringsAsFactors = F)
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        h2$ny <- paste0(h2$ny,'h')
      }else{
        #默认取月份
        data$ny <- paste0(data$nf,'-',data$yf)
        h1 <- as.data.frame(table(data$ny),stringsAsFactors = F)
        names(h1) <- c('ny','num')
        #按顺序构建数据
        h2 <- data.frame(month=as.Date(seq(from=as.Date(paste0(min(data$nf),'-1-1')),to=as.Date(paste0(max(data$nf),'-12-31')),by='month')),stringsAsFactors = F)
        h2$ny <- paste0(year(h2$month),'-',month(h2$month))
        h2 <- left_join(h2,h1,by='ny')#依赖dplyr包
        h2$num[is.na(h2$num)] <- 0
        h2$month <- NULL
      }
    
    #生成累加数据
    h2$cnum <- cumsum(h2$num)
    #计算其它指标：	累计增长量 环比增长量（按时间粒度选择确定名称）发展速度 (定基，环比) 增长速度(定基，环比) 平均发展速度 平均增长速度
    if(nrow(h2)>=2){
      h2$ljzzl <- NA
      h2$hbzzl <- NA
      h2$ljzzl[1] <- 0
      h2$hbzzl[1] <- 0
     for(i in 2:nrow(h2)){
       h2$ljzzl[i] <- h2$num[i]-h2$num[1]
       h2$hbzzl[i] <- h2$num[i]-h2$num[i-1]
     }

    }else{
      h2$ljzzl <- NA
      h2$hbzzl <- NA
    }
    #发展速度：定基和环比，要确保第一个不为0，环比还要处理分母为0的情况
    #平均发展速度和平均增长速度
    h3 <- subset(h2,h2$cnum==0)#前几个为0的情况
    h4 <- subset(h2,h2$cnum>0)
    if(nrow(h4)>=2){
      h4$djfzsd <- NA
      h4$hbfzsd <- NA
      h4$djzzsd <- NA
      h4$hbzzsd <- NA
      h4$pjfzsd <- NA
      h4$pjzzsd <- NA
      h4$djfzsd[1] <- 100
      h4$hbfzsd[1] <- 100
      for(i in 2:nrow(h4)){
        h4$djfzsd[i] <- h4$num[i]/h4$num[1]*100
        h4$hbfzsd[i] <- h4$num[i]/h4$num[i-1]*100
        h4$djzzsd[i] <- h4$djfzsd[i]-100
        h4$hbzzsd[i] <- h4$hbfzsd[i]-100
        h4$pjfzsd[i] <- (h4$djfzsd[i]/100)^(1/(i-1))*100
        h4$pjzzsd[i] <- h4$pjfzsd[i]-100
      }

      h4$djzzsd[h4$djzzsd==-100] <- NA
      h4$hbzzsd[h4$hbzzsd==-100] <- NA
      h4$pjzzsd[h4$pjzzsd==-100] <- NA

    }else{
      h4$djfzsd <- NA
      h4$hbfzsd <- NA
      h4$djzzsd <- NA
      h4$hbzzsd <- NA
      h4$pjfzsd <- NA
      h4$pjzzsd <- NA
    }

    #合并h3和h4,最终输出h2
    if(nrow(h3)>0){
      h3$djfzsd <- NA
      h3$hbfzsd <- NA
      h3$djzzsd <- NA
      h3$hbzzsd <- NA
      h3$pjfzsd <- NA
      h3$pjzzsd <- NA
      h2 <- rbind(h3,h4)
    }else{
      h2 <- h4
    }
    
    h2$hbfzsd <- round(h2$hbfzsd,2)
    h2$hbzzsd <- round(h2$hbzzsd,2)
    h2$pjfzsd <- round(h2$pjfzsd,2)
    h2$pjzzsd <- round(h2$pjzzsd,2)
    h2$cnum <- NULL#去掉该字段
    }else{
    h1 <- data.frame(ny=character(0),num=numeric(0),stringsAsFactors = F)
    h2 <- data.frame(ny=character(0),num=numeric(0),ljzzl=numeric(0),hbzzl=numeric(0),
                   djfzsd=numeric(0),hbfzsd=numeric(0),djzzsd=numeric(0),hbzzsd=numeric(0),pjfzsd=numeric(0),pjzzsd=numeric(0),
                   stringsAsFactors = F)
    }
    }) 
    
    #1.2 频率分布表(h5)
    #ny=分组,num=频数,e3=频率(%),e4=累计频数,e5=累计频率
    #注意：频率分布表，只要有1个有效数据即可
    h5 <- subset(h2,h2$num!=0,select = c('ny','num'))
    h51 <- as.data.frame(table(data0$yf,useNA = 'ifany'))
    if(isempty(h51$Freq[is.na(h51$Var1)])){
      num0 <- 0
    }else{
      num0 <- h51$Freq[is.na(h51$Var1)]
    }
    
    if(nrow(h5)>0||(nrow(h5)==0&&num0>0)){
    h5 <- rbind(h5,data.frame(ny='缺失值',num=num0,stringsAsFactors = F))
    h5$e3 <- h5$num/sum(h5$num)*100
    h5$e4 <- cumsum(h5$num)
    h5$e5 <- round(cumsum(h5$e3),2)
    #去除缺失值后的累计频数和累计频率
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
    #1.3 疑似异常值分布表（dat2）
    ##非正态分布中采用箱式图来判断异常值，标准为<Q1-1.5*IQR 或 >Q3+1.5*IQR
    ##如符合正态分布，则考虑使用均数+-3个标准差来判断
    ##如果可以读入该数据元的参考值的限值范围，则使用该限值范围，目前未考虑这个代码
    if(nrow(data)>0){
      #将日期转为数字，按天或者秒计算到1900-1-1的值
      data$dt <- as.numeric(difftime(strptime(data$report_date,format='%Y-%m-%d %H:%M:%S'),as.Date('1900-01-01'),units='days'))
      b71 <- psych::describe(data$dt,quant=c(0.05,0.25,0.75,0.95),IQR=T,skew = F)
      #按照分布确定
      data$yc1 <- data$dt<(b71$Q0.25-1.5*b71$IQR)|data$dt>(b71$Q0.75+1.5*b71$IQR)
      #按照标准差确定
      data$yc2 <- data$dt<(b71$mean-3*b71$sd)|data$dt>(b71$mean+3*b71$sd)
      #按照总体参数确定
      data$yc3 <- difftime(as.Date(data$report_date),max0,units='days')>0|difftime(as.Date(data$report_date),min0,units='days')<0
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
    
    #2.1离群点检验
    #单个变量不能做异常值检验，绘制离群箱线图替代

    #多个连续变量的相关性可以用到，图形比较漂亮
    #psych::outlier(x, plot = TRUE, bad = 5,na.rm = TRUE, xlab, ylab, ...)
    #pairs.panels(sat.d2,bg=c("yellow","blue")[(d3 > 25)+1],pch=21)

    #2.2 时间平稳性检验(pwjy)
    #一阶差分后的平稳性检验
    withProgress({
      setProgress(message = "正在进行平稳性检验......")
    if(nrow(data)>0){
      #转为时间序列格式，用ts函数而不是as.ts函数
      #后续可以考虑将月份限制在实际起始月份
      if(unit0=='月'){
      kts <- ts(h2$num,start=c(min(data$nf),1),end=c(max(data$nf),12),frequency=12,names='month_series')#按照月份
      }else if(unit0=='季'){
      kts <- ts(h2$num,start=c(min(data$nf),1),end=c(max(data$nf),4),frequency=4,names='quarter_series')#按照季度
      }else if(unit0=='年'){
      kts <- ts(h2$num,start=min(data$nf),end=max(data$nf),frequency=1,names='year_series')#按照年份
      }else if(unit0=='周'){
        kts <- ts(zoo(h2$num, order.by=h2$ny))#按照周
      }else if(unit0=='日'){
        kts <- ts(zoo(h2$num, order.by=as.Date(h2$ny)))#按照天
      }else if(unit0=='小时'){
        kts <- ts(zoo(h2$num, order.by=h2$ny))#按照小时
      }else{
        kts <- ts(h2$num,start=c(min(data$nf),1),end=c(max(data$nf),12),frequency=12,names='month_series')#按照月份
      }
      #ADF检验，p<0.05即可认为趋于平稳(支持向量和时间序列)
      adf_kts <- adf.test(diff(kts))
      #a0检验方法	a1统计量	a2统计值	a3P值	a4滞后阶数	a5备择假设	a6解读
      pwjy <- data.frame(a0=adf_kts$method,a1=names(adf_kts$statistic),a2=unname(adf_kts$statistic),a3=adf_kts$p.value,
                         a4=unname(adf_kts$parameter),a5=adf_kts$alternative,a6=NA,stringsAsFactors = F)
      if(adf_kts$p.value<0.05){
        pwjy$a6[1] <- '差分序列平稳'
      }else{
        pwjy$a6[1] <- '差分序列不平稳'
      }
      pwjy$a2 <- round(pwjy$a2,2)
      pwjy$a3 <- round(pwjy$a3,2)

    }else{
      kts <- NULL
      pwjy <- data.frame(a0=character(0),a1=character(0),a2=numeric(0),a3=numeric(0),
                         a4=numeric(0),a5=character(0),a6=character(0),stringsAsFactors = F)
    }
    })
    
    #2.3 时间趋势性检验
    #升降趋势检验Cox and Stuart Trend Test  p<0.05认为存在上升或者下降趋势
    #要求至少3个元素
    withProgress({
      setProgress(message = "正在进行时间趋势检验......")
    if(nrow(h2)>=3){
    cstr <- trend::cs.test(h2$num)
    #Mann-Kendall Trend Test  tau=Kendall's tau statistic S=Kendall Score
    # 当Z大于1.96则说明为有明显上升趋势
    # 当小于-1.96则说明有明显下降趋势
    # 大于0且小于1.96说明有上升但不明显
    # 小于0且大于-1.96则说明有下降趋势但不明显.
    mktr <- trend::mk.test(h2$num)
    #随机性检验：要求至少2个元素
    # trend::bartels.test(h2$num)#至少10个
    # trend::wm.test(h2$num)#至少2个
    # trend::ww.test(h2$num)#至少2个
    withProgress({
      setProgress(message = "正在进行折点检测......")
    #折点检测
      #730=365*2,，默认只计算两年内的数据
      if(nrow(h2)<=730){
    zd1 <- trend::br.test(h2$num, m = 20000)#至少2个
     #trend::bu.test(h2$num, m = 20000)#至少2个
     #trend::snh.test(h2$num, m = 20000)#至少2个
    zd2 <- trend::pettitt.test(h2$num)#至少2个
    #计算斜率
    xl <- trend::sens.slope(h2$num, conf.level = conf)#至少2个
      }else{
        zd1 <- list()
        zd2 <- list()
        xl <- list()
        zd1$estimate <- '耗时过长'
        zd2$estimate <- '耗时过长'
        xl$estimates <- NA
      }
    
    })

    #a0检验方法	a1统计量	a2统计值	a3P值	a4Kendall Score a41varS a42tau a43折点 a44估计斜率 	a5零假设	a6解读
    qsjy1 <- data.frame(a0='Cox-Stuart趋势检验',a1=names(cstr$statistic),a2=unname(cstr$statistic),a3=cstr$p.value,
                       a4=NA,a41=NA,a42=NA,a43=paste0(unname(zd1$estimate),'(Buishand)'),
                       a44=NA,a5='随时间无升降趋势',a6=NA,a7=NA,stringsAsFactors = F)
    if(qsjy1$a3<0.05){
      qsjy1$a6 <- '存在上升或者下降趋势'
    }else{
      qsjy1$a6 <- '不能拒绝零假设'
    }
    qsjy2 <- data.frame(a0='Mann-Kendall趋势检验',a1=names(mktr$statistic),a2=unname(mktr$statistic),a3=mktr$p.value,
                        a4=unname(mktr$estimates[1]),a41=unname(mktr$estimates[2]),a42=unname(mktr$estimates[3]),
                        a43=paste0(unname(zd2$estimate),'(Pettitt)'),
                        a44=ifelse(is.na(xl$estimates),"耗时过长(Sen's斜率)",paste0(round(unname(xl$estimates),4),"(Sen's slope)")),
                        a5='随时间无升降趋势',a6=NA,a7='推荐',stringsAsFactors = F)
    if(qsjy2$a3<0.05&&qsjy2$a2<0){
      qsjy2$a6 <- '存在下降趋势'
    }else if(qsjy2$a3<0.05&&qsjy2$a2>0){
      qsjy2$a6 <- '存在上升趋势'
    }else{
      qsjy2$a6 <- '不能拒绝零假设'
    }
    qsjy <- rbind(qsjy1,qsjy2)
    
    qsjy$a2 <- round(qsjy$a2,2)
    qsjy$a3 <- round(qsjy$a3,2)
    qsjy$a41 <- round(qsjy$a41,2)
    qsjy$a42 <- round(qsjy$a42,2)

    }else{
      qsjy <- data.frame(a0=character(0),a1=character(0),a2=numeric(0),a3=numeric(0),
                         a4=numeric(0),a41=numeric(0),a42=numeric(0),
                         a43=character(0),a44=character(0),
                         a5=character(0),a6=character(0),a7=character(0),stringsAsFactors = F)
    }
      
    })
    
    #3.1参数估计
    #针对单个的时间变量，转为数值型，做参数估计没有什么意义
    #根据具体的图形获取ARIMA模型的参数，比较有意义(ARIMA模型要求30个有效时间样本才有效)
    withProgress({
      setProgress(message = "正在进行ARIMA参数估计......")
    if(nrow(data)>0&&nrow(h1)>=30){
      #kts <- ts(h2$num,start=c(min(data$nf),1),end=c(max(data$nf),12),frequency=12,names='month_series')#按照月份
      fits <- auto.arima(kts,allowdrift=TRUE)
      pdq <- arimaorder(fits)
      #a0有效样本量 a1模型名称 a11有效时序数
      csgj <- data.frame(a0=nrow(data),a1='ARIMA',a11=nrow(h1),p=pdq[1],d=pdq[2],q=pdq[3],
                         AIC=round(fits$aic,2),AICC=round(fits$aicc,2),BIC=round(fits$bic,2),stringsAsFactors = F)
    }else{
      csgj <- data.frame(a0=numeric(0),a1=character(0),a11=numeric(0),p=numeric(0),d=numeric(0),q=numeric(0),
                         AIC=numeric(0),AICC=numeric(0),BIC=numeric(0),stringsAsFactors = F)
    }
    })

    ##解读的文字段落
    if(nrow(data)>0){
    p1 <- paste0('1.',dmname,'数据的总频数为',nrow(data0),'，有效频数为',nrow(data0)-num0,'，占比',round((nrow(data0)-num0)/nrow(data0)*100,1),'%，缺失频数为',num0,'，占比',round(num0/nrow(data0)*100,1),'%；')
    p2 <- paste0('2.在非缺失值中，疑似异常值个数为',dat2$f2[2],'个，占比',dat2$f3[2],'%；\n（异常值判断方法：百分位数间距法+标准差法+总体参数值域法）')
    p3 <- paste0('3.单个日期时间变量无法进行异常值检验！')
    p4 <- paste0('4.数据经',pwjy$a0[1],'，认为其',pwjy$a6[1],'；')
    p5 <- paste0('5.数据经',qsjy$a0[2],'，认为其',qsjy$a6[2],'；')
    if(nrow(h1)>=30){
    p6 <- paste0('6.数据利用',csgj$a1[1],'模型进行评估，获得其pdq参数分别为：',paste(csgj$p,csgj$d,csgj$q,sep=' '),'；')
    }else{
      p6 <- paste0('6.有效时序数少于30个，无法有效进行ARIMA参数估计！')
    }
    }else if(nrow(data)==0&&nrow(data0)>0){
      p1 <- paste0('1.',dmname,'数据中总频数为',nrow(data0),'，有效频数为',nrow(data0)-num0,'，占比',round((nrow(data0)-num0)/nrow(data0)*100,1),'%，缺失频数为',num0,'，占比',round(num0/nrow(data0)*100,1),'%；')
      p2 <- paste0('2.无法计算疑似异常值！')
      p3 <- paste0('3.单个日期时间变量无法进行异常值检验！')
      p4 <- paste0('4.数据缺失，无法进行时序平稳性检验！')
      p5 <- paste0('5.数据缺失，无法进行时间趋势检验！')
      p6 <- paste0('6.数据缺失，无法进行ARIMA参数估计！')
    }else{
     p1 <- '无数据'
     p2 <- ''
     p3 <- ''
     p4 <- ''
     p5 <- ''
     p6 <- ''
    }
    z <- list(obj=obj,dmname=dmname,dmunit=dmunit,filterT=filterT,
              data=data,data0=data0,h1=h1,
              h2=h2,h5=h5,dat2=dat2,sketch2=sketch2,
              pwjy=pwjy,kts=kts,
              qsjy=qsjy,csgj=csgj,
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
output$filter8 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)

#1.1基本统计表
output$table1 <- DT::renderDataTable({
  b <- z()
  h2 <- b$h2
  names(h2) <- c('时序','频数','累计增长量','环比增长量','定基发展速度(%)','环比发展速度(%)','定基增长速度(%)','环比增长速度(%)','平均发展速度(%)','平均增长速度(%)')
  DT::datatable(h2,
                #container = b$sketch2,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',unit0,'）的统计描述表'))
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
                  dom = "tip"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')
})

#1.2 频率分布表
output$table2 <- DT::renderDataTable({
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
                  dom = "tip"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(1,target = 'row',
                backgroundColor = styleEqual('缺失值',  '#FFFF99'))
})

#1.3疑似异常值分布表
output$table3 <- DT::renderDataTable({
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

#2.1 离群点检验
output$plot1 <- renderPlot({
  b <- z()
  data <- b$data
  if (nrow(data)>=3) {
    show("plot1")
  } else {
    hide("plot1")
  }
  if (nrow(data)>=3) {
    Boxplot(data$dt,labels=as.Date(data$report_date),axes=T,ylab='',yaxt='n',
            main=paste0(b$obj,b$dmname,'（',unit0,'）的离群箱式图'))
  }else{
    return(NULL)
  }
})
output$text21 <- renderUI({
  h4("单个日期时间变量无法进行异常值检验！",style="color:red;font-weight:bold;",align='center')
})

#2.2 时序平稳性检验表
output$table4 <- DT::renderDataTable({
  b <- z()
  pwjy <- b$pwjy
  names(pwjy) <- c('检验方法','统计量','统计值','P值','滞后阶数','备择假设','意义')
  DT::datatable(pwjy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',unit0,'）的时序平稳性检验表'))
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
#2.3 时间趋势检验
output$table5 <- DT::renderDataTable({
  b <- z()
  qsjy <- b$qsjy
  qsjy <- qsjy[order(qsjy$a7,decreasing = T),]
  names(qsjy) <- c('检验方法','统计量','统计值','P值','KendallScore值','varS值','tau值','疑似折点','估计斜率','零假设','意义','是否推荐')
  DT::datatable(qsjy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',unit0,'）的时间趋势检验表'))
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
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(12, fontWeight = styleEqual('推荐',  'bold'),
                color = styleEqual('推荐',  'green'))%>%
    formatStyle(12,target = 'row',
                backgroundColor = styleEqual('推荐',  '#FFFF99'))
})

#3.1参数估计（ARIMA）
output$table6 <- DT::renderDataTable({
  b <- z()
  csgj <- b$csgj
  names(csgj) <- c('有效样本含量','模型名称','有效时序数','p','d','q','AIC','AICC','BIC')
  DT::datatable(csgj,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',unit0,'）的ARIMA参数估计表'))
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
    formatStyle(1, fontWeight = 'bold')
})
output$plot2 <- renderPlot({
  b <- z()
  data <- b$data
  kts <- b$kts
  if (!is.null(kts)){
    show("plot2")
  } else {
    hide("plot2")
  }
  if (!is.null(kts)){
    #依次为时间序列图、一阶差分序列图、ACF图、PACF图,2行2列
    opar <- par(no.readonly = TRUE)
    par(mfrow=c(2,2),mar=c(2,4,2,2))
    plot(kts,ylab='频数',main='',xlab='',col.main='darkgreen')
    plot(diff(kts,differences=1),ylab='一阶差分',xlab='',main='',col.main='darkgreen')
    Acf(diff(kts),xlab='滞后阶数',ylab='自相关ACF',main='')
    Pacf(diff(kts),xlab='滞后阶数',ylab='偏自相关PACF',main='')
    par(opar)
  }else{
    return(NULL)
  }
})

#总体解读
output$text1 <- renderPrint({
  b <- z()
  #print(urlkey)
  cat(b$p1,b$p2,b$p3,b$p4,b$p5,b$p6,sep='\n\n')
})

})
