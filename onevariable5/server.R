#单个有序多分类变量的描述推断分析
#王文祥
#2017-12-29
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

#构建计算构成比可信区间的函数
binom.agresti<- function(x, n, conf=0.95,alter) {
  # agresti-coull
  if(alter==1){
    tails<- alter
    alter1 <- '单侧' 
  }else if(alter==2){
    tails<- alter
    alter1 <- '双侧'
  }else{
    tails<- 2
    alter1 <- '双侧'
  }
  z.conf<- stats::qnorm(1 - (1 - conf)/tails, 0, 1)
  n.ac<- n + z.conf^2
  x.ac<- x + z.conf^2/2
  p.ac<- x.ac/n.ac
  q.ac<- 1 - p.ac
  lc<- p.ac - z.conf*(p.ac*q.ac)^0.5 * n.ac^-0.5
  uc<- p.ac + z.conf*(p.ac*q.ac)^0.5 * n.ac^-0.5
  
  return(data.frame(x=x, n=n, proportion=x/n, lower=lc,
                    upper=uc, conf.level=conf,alter=alter1, method="agresti-coull"))
}
binom.jeffreys<- function(x, n, conf=0.95,alter) {
  # jeffreys interval
  if(alter==1){
    tails<- alter
    alter1 <- '单侧' 
  }else if(alter==2){
    tails<- alter
    alter1 <- '双侧'
  }else{
    tails<- 2
    alter1 <- '双侧'
  }
  lc<- stats::qbeta((1 - conf)/tails, x+0.5, n-x+0.5)
  uc<- stats::qbeta(1 - (1 - conf)/tails, x+0.5, n-x+0.5)
  p<- x/n
  return(data.frame(x=x, n=n, proportion=p,
                    lower=lc, upper=uc, conf.level=conf,alter=alter1, method="jeffreys"))
}
binom.cp<- function(x, n, conf=0.95,alter) {
  # clopper-pearson exact interval
  if(alter==1){
    tails<- alter
    alter1 <- '单侧' 
  }else if(alter==2){
    tails<- alter
    alter1 <- '双侧'
  }else{
    tails<- 2
    alter1 <- '双侧'
  }
  lc<- stats::qbeta((1-conf)/tails, x, n-x+1)
  uc<- stats::qbeta(1 - (1-conf)/tails, x+1, n-x)
  p<- x/n
  return(data.frame(x=x, n=n, proportion=p, lower=lc,
                    upper=uc, conf.level=conf,alter=alter1, method="clopper-pearson"))
}
binom.5basic<- function(x, n, conf=0.95,alter){
  ##Wald
  ##适用条件：n足够大（通常要大于100），n*p和n*q均大于5
  if(alter==1){
    tails<- alter
    alter1 <- '单侧' 
  }else if(alter==2){
    tails<- alter
    alter1 <- '双侧'
  }else{
    tails<- 2
    alter1 <- '双侧'
  }
  
  zalpha <- qnorm(1-((1-conf)/tails))
  
  p <- x/n
  q  <-  1 - p
  stder <-  sqrt(p*q/n)
  Wald_lcl  <-  p - zalpha * stder
  Wald_ucl  <-  p + zalpha * stder
  ##wald cc 样本量小进行连续性校正
  #http://support.sas.com/documentation/cdl/en/procstat/67528/HTML/default/viewer.htm#procstat_freq_details37.htm
  Waldcc_lcl  <-  p - (zalpha * stder+1/(2*n))
  Waldcc_ucl  <-  p + (zalpha * stder+1/(2*n))
  #另外一种小样本校正方法:Adjusted-Wald Binomial Confidence Interval
  #https://www.douban.com/note/327902632/
  p1 <- (x+zalpha)/(n+2*zalpha)
  q1 <- 1-p1
  stder1 <-  sqrt(p1*q1/n)
  Waldcc_lcl1  <-  p1 - zalpha * stder1
  Waldcc_ucl1  <-  p1 + zalpha * stder1
  
  ##Wilson Score
  part1  <-  p + ((zalpha^2)/(2*n))
  part2  <-  sqrt( (p*q/n) + ((zalpha^2)/(4*n^2)) )
  part3  <-  1 + (zalpha^2)/n
  Wilson_lcl  <-  (part1 - (zalpha * part2))/ part3
  Wilson_ucl  <-  (part1 + (zalpha * part2))/ part3
  
  ##Wilson Score with continuity correction(Newcombe-Wilson Score)
  #http://www.ucl.ac.uk/english-usage/staff/sean/resources/binomialpoisson.pdf
  #However, if p = 0, wl must be taken as 0; if p = 1, wu is then 1.
  #通过这个计算机验证通过 http://www.vassarstats.net/prop1.html
  #为了支持向量的运算，极值的校正放到后面进行
  wl=max(0,(2*n*p+zalpha^2-(zalpha*sqrt(zalpha^2-1/n+4*n*p*(1-p)+(4*p-2))+1))/(2*(n+zalpha^2)))
  wu=min(1,(2*n*p+zalpha^2+(zalpha*sqrt(zalpha^2-1/n+4*n*p*(1-p)-(4*p-2))+1))/(2*(n+zalpha^2))) 
  ##Agresti-Coull
  d1 <- binom.agresti(x,n,conf,alter)
  ##Jeffreys
  d2 <- binom.jeffreys(x,n,conf,alter)
  ##Exact Clopper Pearson
  d3 <- binom.cp(x,n,conf,alter)
  
  d <- rbind(
    data.frame(x=x, n=n, proportion=x/n, lower=Wald_lcl,
               upper=Wald_ucl, conf.level=conf,alter=alter1, method="Wald"),
    data.frame(x=x, n=n, proportion=x/n, lower=Waldcc_lcl,
               upper=Waldcc_ucl, conf.level=conf,alter=alter1, method="Waldcc"),
    data.frame(x=x, n=n, proportion=x/n, lower=Waldcc_lcl1,
               upper=Waldcc_ucl1, conf.level=conf,alter=alter1, method="Adjusted-Wald"),
    data.frame(x=x, n=n, proportion=x/n, lower=Wilson_lcl,
               upper=Wilson_ucl, conf.level=conf,alter=alter1, method="Wilson Score"),
    data.frame(x=x, n=n, proportion=x/n, lower=wl,
               upper=wu, conf.level=conf,alter=alter1, method="Newcombe-Wilson Score"),
    d1,d2,d3
  )
  for(i in 1:nrow(d)){
    if(d$proportion[i]==1){
      d$upper[i] <- 1
    }
    if(d$proportion[i]==0){
      d$lower[i] <- 0
    }
  }
  
  return(d)
}

shinyServer(function(input, output,session) {
  
  withProgress({
    setProgress(message = "数据加载中......")
  #先从老朱那里取到themeid和一堆系统参数stat开头、数据筛选条件
  #observe里面支持全局赋值，否则后面output里面不认识，不同于shinyServer函数外面的变量可以被认识
  observe({
    urlkey <<- parseQueryString(session$clientData$url_search)
    if(length(urlkey)==0){
    urlkey <<- list(theme_id='1209',
                   I_report_year='2011',
                   report_district='杨浦区',
                   stat_side_type='双侧',stat_alpha0='0.05',stat_conf='0.95')
    }
    ##检验的单双侧(全局)
    side_type <<- urlkey$stat_side_type
    ##显著性水平(全局)
    alpha0 <<- as.numeric(urlkey$stat_alpha0)
    ##置信水平(全局)
    conf <<- as.numeric(urlkey$stat_conf)
    ##参考值范围(全局)
    #max0 <<- as.numeric(urlkey$stat_max0)
    #min0 <<- as.numeric(urlkey$stat_min0)

    
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
    dsid <- theme$dsid
    dskey <-theme$dskey
    dsname <- theme$dsname
    dmkey <- theme$dmkey
    dmname <- theme$dmname
    #dmunit <- theme$dmunit
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
    
    #filterT <- merge(thdm,thd,by='dmkey',sort = T)
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
    data <- dbGetQuery(conn,paste0('select ',dmkey,' from ',dskey,' where ',theme$hrolesql,wherep1)) 
    dbDisconnect(conn)
    
    #取对应的代码
    # conn1 <- dbConnect(MySQL(),
    #                   user="dmagic",
    #                   password="dmagic",
    #                   dbname="assetls",
    #                   host="192.168.1.35",
    #                   port=3306)
    # #设置读取字符格式
    # dbGetQuery(conn1,"SET NAMES utf8")
    # ##实际读数据时会将筛选器条件写在where语句中传进来，这里需要处理年份和日期的问题，否则取不到数据
    # dic <- dbGetQuery(conn1,paste0('select vkey dickey,name dicname from t_b_dic_value where dicid=(select dicid from t_e_datameta where datasetid=',dsid," and dmkey='",dmkey,"' and removeflag=0)")) 
    # dbDisconnect(conn1)
    #只有在字典不为空的情况下才取出字典
    if(length(theme$dics)>0){
      dic <- as.data.frame(theme$dics$dv)
      names(dic) <- c('dicname','dickey')
    }else{
      dic <- data.frame(dicname=character(0),dickey=character(0))
    }
    
    # #1.1 构成分析表
    withProgress({
      setProgress(message = "正在进行构成分析......")
    freq <- as.data.frame(table(data,useNA='ifany'),stringsAsFactors = F)
    freq <- freq[order(freq$data,decreasing = F),]#按照属性排序
    freq0 <- freq
    #注意利用isempty函数进行判断是否为空
    if(nrow(data)>0){
      NAnum <- freq$Freq[is.na(freq$data)]
      if(pracma::isempty(NAnum)){
        NAnum <- 0
      }
      freq$prop <- round(freq$Freq/sum(freq$Freq)*100,2)#包含缺失值
      freq$prop1 <- round(freq$Freq/(sum(freq$Freq)-NAnum)*100,2)#不包含缺失值
      if(nrow(dic)>0){
        freq <- left_join(freq,dic,by=c('data'='dickey'))#根据代码翻译成中文，不在代码内的保持原样
      }else{
        freq$dicname <- NA
      }
      if(nrow(freq)==1&is.na(freq$data[1])){
        freq <- rbind(freq,data.frame(data='合计',Freq=sum(freq$Freq),prop=100,prop1=NA,dicname=NA)) #有且仅有缺失值时
      }else{
      freq <- rbind(freq,data.frame(data='合计',Freq=sum(freq$Freq),prop=100,prop1=100,dicname=NA))
      }
      freq$prop1[is.na(freq$data)] <- NA
      freq$data[is.na(freq$data)] <- '缺失值'
      freq$dicname[is.na(freq$dicname)] <- freq$data[is.na(freq$dicname)]#前台展示用freq[,c(5,2:4)],第一列用于异常值的判断
    }else{
      freq <- data.frame(data=character(0),Freq=numeric(0),prop=numeric(0),prop1=numeric(0),dicname=character(0),stringsAsFactors = F)
      freq0 <- freq
    }

    })

    #1.2疑似异常值分布表
    ##分类字段的异常值以代码映射为准，不在代码属性内的为异常
    ##本次没有区分异常值和缺失值，因为一般来说不在代码范围内的就是这两类
    if(nrow(dic)>0){
    if(nrow(freq)>0){
    freq1 <- freq[-nrow(freq),]
    freq3 <- subset(freq1,freq1$data%in%dic$dickey,select = c(1:3))#freq3为非异常值
    freq1 <- subset(freq1,!freq1$data%in%dic$dickey,select = c(5,2:3))
    names(freq1)[1] <- 'data'
    freq4 <- freq1#freq4用于计算异常值（排除缺失值和合计）占比
    if(nrow(freq1)>0){
    freq1 <- rbind(freq1,data.frame(data='合计',Freq=sum(freq1$Freq),prop=sum(freq1$prop)))
    }
    }else{
      freq1 <- data.frame(data=character(0),Freq=numeric(0),prop=numeric(0))
      freq3 <- data.frame(data=character(0),Freq=numeric(0),prop=numeric(0))
    }
    }else{
      freq1 <- data.frame(data=NA,Freq='无代码映射，无法判断',prop=NA)#无代码时的输出
      freq3 <- data.frame(data=character(0),Freq=numeric(0),prop=numeric(0))
    }

    #这里可能需要解决跨行的表格设计问题，暂时不做，以后再弄
    # sketch2  <-  htmltools::withTags(table(
    #   class = 'display',
    #   thead(
    #     tr(
    #       th(class = 'dt-left',colspan = 1, '值类型'),
    #       th(class = 'dt-left',colspan = 1, '频数'),
    #       th(class = 'dt-left',colspan = 1, '频率(%)')
    #     )
    #   ),
    #   tfoot(
    #     tr(td(class = 'dt-left',colspan = 3,tft))
    #   )
    # ))

 
    #2. 假设检验
    #2.1 比例检验
    #只做双侧检验
    if(nrow(freq0)>1){
    pr <- prop.test(freq0$Freq,rep(sum(freq0$Freq),nrow(freq0)),alternative = 'two.sided',conf.level = conf,correct = T)
    bljy <- data.frame(a0='比例检验',a1='X-squared',a2='双侧',a3=round(unname(pr$statistic),2),a4=round(pr$p.value,4),
                       a5=unname(pr$parameter),a6='各个分类的比例相同',a7=NA,stringsAsFactors = F)
    
    if(is.na(bljy$a4[1])){
      bljy$a7[1] <- NA
    }else if(bljy$a4[1]<0.05){
      bljy$a7[1] <- '拒绝零假设'
    }else if(bljy$a4[1]>=0.05){
      bljy$a7[1] <- '不能拒绝零假设'
    }else{
      bljy$a7[1] <- NA
    }
    }else{
      bljy <- data.frame(a0='比例检验',a1='X-squared',a2='双侧',a3=NA,a4=NA,
                         a5=NA,a6='各个分类的比例相同',a7='不能进行比例检验',stringsAsFactors = F)
    }
    
    #2.2 多项式分布的拟合优度检验
    #http://www.r-tutor.com/elementary-statistics/goodness-fit/multinomial-goodness-fit
    #需要根据提供的总体构成比进行卡方检验，而总体构成比如果分类比较多的情况下，总体构成比也比较多，估计不好提供，所以暂时不弄


    #3.1参数估计（二项分布法和多项分布法，共计17种方法）
    withProgress({
      setProgress(message = "正在进行参数估计......")
    if(side_type=='双侧'){
      alter1 <- 2
    }else if(side_type=='单侧'){
      alter1 <- 1
    }else{
      alter1 <- 2
    }
    
    if(nrow(freq)>0){
    freq2 <- freq[-nrow(freq),c(5,2,3)]#在freq的基础上去除了合计
    names(freq2)[1] <- 'data'
    cl1 <- binom.5basic(freq2$Freq,sum(freq2$Freq),conf,alter1)
    cl1$vname <- freq2$data
    
    m1 <- WALD(freq2$Freq,1-conf)
    m1$waldcl$alter <- '双侧'
    m1$waldcl$vname <- freq2$data
    
    m2 <- WALDCC(freq2$Freq,1-conf)
    m2$waldcccl$alter <- '双侧'
    m2$waldcccl$vname <- freq2$data
    
    m3 <- WS(freq2$Freq,1-conf)
    m3$wscl$alter <- '双侧'
    m3$wscl$vname <- freq2$data
    
    m4 <- GM(freq2$Freq,1-conf)
    m4$gmcl$alter <- '双侧'
    m4$gmcl$vname <- freq2$data
    
    m5 <- SG(freq2$Freq,1-conf)
    m5$sgcl$alter <- '双侧'
    m5$sgcl$vname <- freq2$data
    
    m6 <- FS(freq2$Freq,1-conf,alter1)
    m6$fscl$alter <- side_type
    m6$fscl$vname <- freq2$data
    
    m7 <- QH(freq2$Freq,1-conf)
    m7$qhcl$alter <- '双侧'
    m7$qhcl$vname <- freq2$data
    
    m8 <- BMDE(freq2$Freq,1)
    m8$bmdecl$alter <- '双侧'
    m8$bmdecl$conf <- 0.95
    m8$bmdecl$vname <- freq2$data
    
    #du的数值：[1,length(freq2$Freq)]
    if(length(freq2$Freq)<=1){
      du <- 1
    }else{
      du <- 2
    }
    m9 <- BMDU(freq2$Freq,du)
    m9$bmducl$alter <- '双侧'
    m9$bmducl$conf <- 0.95
    m9$bmducl$vname <- freq2$data
    
    mcl1 <- rbind(m1$waldcl[,c(11,1,2,4,7:10)],m2$waldcccl[,c(11,1,2,4,7:10)],m3$wscl[,c(11,1,2,4,7:10)],m4$gmcl[,c(11,1,2,4,7:10)],m5$sgcl[,c(11,1,2,4,7:10)],
          m6$fscl[,c(11,1,2,4,7:10)],m7$qhcl[,c(11,1,2,4,7:10)],m8$bmdecl[,c(9,1,2,8,4:7)],m9$bmducl[,c(9,1,2,8,4:7)])
    
    mcl1$n <- sum(freq2$Freq)
    mcl1$method <- paste0('multinomial:',mcl1$method)
    names(mcl1) <- c("vname","x","proportion","conf.level","lower","upper","method","alter","n")
    
    cl <- rbind(cl1[,c(9,1:8)],mcl1[,c(1,2,9,3,5,6,4,8,7)])
    cl$lower[cl$lower<0] <- 0#极小值处理
    cl$upper[cl$upper>1] <- 1#极大值处理
    cl$proportion <- round(as.numeric(cl$proportion)*100,2)
    cl$lower <- round(as.numeric(cl$lower)*100,3)
    cl$upper <- round(as.numeric(cl$upper)*100,3)
    cl$isok <- NA
    if(sum(freq2$Freq)>1000){
      cl$isok[cl$method=='Wald'] <- '推荐'
    }else if(sum(freq2$Freq)>100){
      cl$isok[cl$method=='Newcombe-Wilson Score'] <- '推荐'
    }else{
      cl$isok[cl$method=='clopper-pearson'] <- '推荐'
    }
    
    }else{
      cl <- data.frame(vname=character(0),x=numeric(0),n=numeric(0),proportion=numeric(0),
                       lower=numeric(0),upper=numeric(0),conf.level=numeric(0),
                       alter=character(0),method=character(0),isok=character(0),stringsAsFactors = F)
    }
    })
    
    ##解读的文字段落
    if(nrow(data)>0){
    if(nrow(dic)>0){
      p1 <- paste0('1.',dmname,'数据中总计数为',sum(freq2$Freq),'，符合代码值的计数为',sum(freq3$Freq),'，占比',round(sum(freq3$Freq)/sum(freq2$Freq),2)*100,'%，缺失的计数为',ifelse(isempty(freq$Freq[freq$data=='缺失值']),0,freq$Freq[freq$data=='缺失值']),'，占比',ifelse(isempty(freq$prop[freq$data=='缺失值']),0,freq$prop[freq$data=='缺失值']),'%；')
      p2 <- paste0('2.在非缺失值中，疑似异常值个数为',sum(freq4$Freq),'个，占比',sum(freq4$prop),'%；（疑似异常值判断方法：代码值域）')
    }else{
      p1 <- paste0('1.',dmname,'数据中总计数为',sum(freq2$Freq),'，缺失的计数为',ifelse(isempty(freq$Freq[freq$data=='缺失值']),0,freq$Freq[freq$data=='缺失值']),'，占比',ifelse(isempty(freq$prop[freq$data=='缺失值']),0,freq$prop[freq$data=='缺失值']),'%；')
      p2 <- "2.无代码映射，无法判断异常值！"
    }
    p3 <- paste0('3.根据',dmname,'数据的实际情况进行比例等同检验，其零假设为',bljy$a6[1],'，结果为：',bljy$a7[1],'；')
    p4 <- paste0('4.根据样本量和构成比的实际情况采用',unique(cl$method[which(cl$isok=='推荐')]),'方法，确定各分类的构成比的',as.numeric(unique(cl$conf.level[which(cl$isok=='推荐')]))*100,'%的置信区间；')
    }else{
     p1 <- '无数据'
     p2 <- ''
     p3 <- ''
     p4 <- ''
    }
    z <- list(obj=obj,dmname=dmname,data=data,
              #dic=dic,
              freq=freq,freq1=freq1,bljy=bljy,cl=cl,
              filterT=filterT,
              #sketch2=sketch2,
              p1=p1,p2=p2,p3=p3,p4=p4)
    
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


#1.1 构成分析表
output$table1 <- DT::renderDataTable({
  b <- z()
  freq <- b$freq[,c(5,2,3,4)]
  names(freq) <- c('类别','计数','构成比(%)','构成比(%)*')
  DT::datatable(freq,
                #container = sketch1,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,'中不同',b$dmname,'的构成分析表'))
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
                  dom = "t"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')
})

#1.2疑似异常值分布表
output$table2 <- DT::renderDataTable({
  b <- z()
  freq1 <- b$freq1
  #sketch2 <- b$sketch2
  names(freq1) <- c('类别','计数','构成比(%)')
  DT::datatable(freq1,
                #container = sketch2,
                escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,'中不同',b$dmname,'的异常值构成表'))
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
                  dom = "t"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')
})

#2.1 比例检验
output$table3 <- DT::renderDataTable({
  b <- z()
  bljy <- b$bljy
  names(bljy) <- c('检验方法','统计量','单双侧','统计值','P值','自由度','零假设','解读')
  DT::datatable(bljy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,'中不同',b$dmname,'的比例检验表'))
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

#3.1参数估计（17种方法）
output$table4 <- DT::renderDataTable({
  b <- z()
  cl <- b$cl
  cl <- cl[order(cl$isok,decreasing = T),]
  names(cl) <- c('类别','计数','总数','构成比(%)','CI下限','CI上限','置信水平','单双侧','估计方法','是否推荐')
  DT::datatable(cl,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,'中不同',b$dmname,'的构成比估计表'))
                ),
                options = list(
                  ordering= T,
                  columnDefs = list(list(className = 'dt-center', targets = '_all')),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'mintcream', 'color': '#000'});",
                    "}"),
                  lengthMenu = c(50, 100,150),
                  searchHighlight = TRUE,
                  deferRender = TRUE,
                  dom = "fltip"))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(10, fontWeight = styleEqual('推荐',  'bold'),
                color = styleEqual('推荐',  'green'))%>%
    formatStyle(10,target = 'row',
                backgroundColor = styleEqual('推荐',  '#FFFF99'))
})
#增加一幅置信区间图p205
output$plot1 <- renderPlotly({
  b <- z()
  cl <- b$cl
  #在使用shinyjs的基础上，隐藏空白图片的输出
  if (nrow(na.omit(cl))>0) {
    show("plot1")
  } else {
    hide("plot1")
  }
  
  if(nrow(na.omit(cl))>0){
    cl$proportion <- as.numeric(cl$proportion)
    cl$lower <- as.numeric(cl$lower)
    cl$upper <- as.numeric(cl$upper)
    s1 <-cl[which(cl$isok=='推荐'),c(9,1,4,5,6,7)] 
    meth <- unique(s1$method)
    clf <- paste0(as.numeric(unique(s1$conf.level))*100,'%')    
    s1$vname <- factor(s1$vname,levels = s1$vname[order(s1$vname,decreasing = T)],ordered = T)#保证纵轴按变量属性排列
    p <- plot_ly(s1, color = I("gray80")) %>%
      add_segments(x = ~lower, xend = ~upper, y = ~vname, yend = ~vname, showlegend = FALSE) %>%
      add_markers(x = ~lower, y = ~vname, name = "CI下限", color = I("darkgreen")) %>%
      add_markers(x = ~proportion, y = ~vname, name = "均值", color = I("black")) %>%
      add_markers(x = ~upper, y = ~vname, name = "CI上限", color = I("red")) %>%
      layout(font=list(family="Microsoft YaHei"),
        #title = paste0('<b>',b$obj,'中不同',b$dmname,'的构成比',clf,'置信区间图(',meth,')</b>'),
        xaxis = list(title = "构成比（%）",showline=F,zeroline = FALSE),
        yaxis = list(title = "",showline=F,domain=list(0.4,1)),
        margin = list(l = 65,t=30)
      )%>%
      config(p = ., staticPlot = FALSE, 
             displayModeBar = FALSE, 
             workspace = TRUE, 
             editable = FALSE,
             showLink = FALSE,
             sendData = FALSE, 
             displaylogo = FALSE)
    p
  }else{
    return(NULL)
  }

})

#总体解读
output$text1 <- renderPrint({
  b <- z()
  #print(b$filterT)
  cat(b$p1,b$p2,b$p3,b$p4,sep='\n\n')

})

})
