# 单个二分类变量的描述推断分析
# 王文祥
# 2017-12-25
# server.R

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
  #参考文后代码：http://mwsug.org/proceedings/2008/pharma/MWSUG-2008-P08.pdf
  #https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
  #http://stats.org.uk/statistical-inference/Newcombe1998.pdf
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
#写一个函数binom.p，涵盖二项分布概率计算(与总体率的比较)的直接法和正态近似法
binom.p<- function(x, size,prob,alpha=0.05) {
  #x为阳性事件发生次数；size为试验总次数；prob为总体发生率；alpha为显著水平，一般为0.05
  if(x<0||size<=0)
  {cat('Warning: x arguments must be non-negative integers')
  }
  if(prob<0||prob>1)
  {cat('Warning: prob arguments must in [0,1]')
  }
  #c('≥','>','≤','<','!=')
  if(x>=1){
    ge <- pbinom(x-1,size,prob,lower.tail = F)#great equal
    gt <- pbinom(x,size,prob,lower.tail = F)#great than
    le <- pbinom(x,size,prob,lower.tail = T)#less equal
    lt <- pbinom(x-1,size,prob,lower.tail = T)#less than
    eq <- ge-gt#equal
    bp <- data.frame(method='二项分布精确计算法',x=x,size=size,prob=prob,lambda=NA,type=c('great equal','great than','less equal','less than','equal'),stat=NA,v=NA,p=c(ge,gt,le,lt,eq),alpha=alpha)
  }else if(x==0){
    ge0 <- 1#great equal
    gt0 <- pbinom(x,size,prob,lower.tail = F)#great than
    le0 <- pbinom(x,size,prob,lower.tail = T)#less equal or equal
    lt0 <- 0#less than
    eq0 <- le0#equal
    bp <- data.frame(method='二项分布精确计算法',x=x,size=size,prob=prob,lambda=NA,type=c('great equal','great than','less equal','less than','equal'),stat=NA,v=NA,p=c(ge0,gt0,le0,lt0,eq0),alpha=alpha)
  }else{
    bp <- data.frame(method='二项分布精确计算法',x=x,size=size,prob=prob,lambda=NA,type=NA,stat=NA,v=NA,p=NA,alpha=alpha)
  }
  ##正态近似法
  if(x>5&&size>100&&(size-x)>5){
    p <- x/size
    u <- (p-prob)/sqrt(prob*(1-prob)/size)
    #双侧
    pt <- 2*pnorm(abs(u),lower.tail=F)
    #单侧
    ps <- pnorm(abs(u),lower.tail=F)
    bn <- data.frame(method='二项分布正态近似法',x=x,size=size,prob=prob,lambda=NA,type=c('双侧','单侧'),stat=abs(u),v=Inf,p=c(pt,ps),alpha=alpha)
  }else{
    bn <- data.frame(method='二项分布正态近似法',x=x,size=size,prob=prob,lambda=NA,type=NA,stat=NA,v=NA,p=NA,alpha=alpha)
  }
  
  bdata <- rbind(bp,bn)#stat为统计量 v为自由度
  #根据alpha水平，判断是否显著
  # #H0
  # bdata$h0 <- paste0('样本率=',prob)
  # #H1
  # bdata$h1[bdata$method=='二项分布精确计算法'] <- paste0('样本率',bdata$type[bdata$method=='二项分布精确计算法'],prob)
  # bdata$h1[bdata$type=='双侧'] <- paste0('样本率!=',prob)
  # bdata$h1[bdata$type=='单侧'] <- paste0('样本率大于[或小于]',prob)
  bdata$value[bdata$p<alpha] <- '显著'
  bdata$value[bdata$p>=alpha] <- '不显著'
  
  bdata$recommend[bdata$x>5&bdata$size>100&(bdata$size-bdata$x)>5&bdata$method=='二项分布正态近似法'] <- '推荐'
  bdata$recommend[bdata$x>=0&bdata$size<=100&bdata$method=='二项分布精确计算法'] <- '推荐'
  bdata$p <- round(bdata$p,5)
  return(bdata)
}
#写另外一个函数poisson.p，涵盖Poisson分布概率计算(与总体率的比较)的直接法和正态近似法
poisson.p<- function(x,size,prob,alpha=0.05) {
  #x为阳性事件发生次数；size为试验总次数；prob为总体发生率；alpha为显著水平，一般为0.05
  if(x<0||size<=0)
  {cat('Warning: arguments must be non-negative integers')
  }
  if(prob<0||prob>1)
  {cat('Warning: lambda arguments must in [0,1]')
  }
  lambda <- size*prob#计算lambda的数值
  #c('≥','>','≤','<','!=')
  if(x>=1&&lambda>0){
    ge <- ppois(x-1,lambda,lower.tail = F)#great equal
    gt <- ppois(x,lambda,lower.tail = F)#great than
    le <- ppois(x,lambda,lower.tail = T)#less equal
    lt <- ppois(x-1,lambda,lower.tail = T)#less than
    eq <- ge-gt#equal
    pp <- data.frame(method='Poisson分布精确计算法',x=x,size=size,prob=prob,lambda=lambda,type=c('great equal','great than','less equal','less than','equal'),stat=NA,v=NA,p=c(ge,gt,le,lt,eq),alpha=alpha)
  }else if(x==0&&lambda>0){
    ge0 <- 1#great equal
    gt0 <- ppois(x,lambda,lower.tail = F)#great than
    le0 <- ppois(x,lambda,lower.tail = T)#less equal or equal
    lt0 <- 0#less than
    eq0 <- le0#equal
    pp <- data.frame(method='Poisson分布精确计算法',x=x,size=size,prob=prob,lambda=lambda,type=c('great equal','great than','less equal','less than','equal'),stat=NA,v=NA,p=c(ge0,gt0,le0,lt0,eq0),alpha=alpha)
  }else{
    pp <- data.frame(method='Poisson分布精确计算法',x=x,size=size,prob=prob,lambda=lambda,type=NA,stat=NA,v=NA,p=NA,alpha=alpha)
  }
  
  if(lambda>=20&&x>=0){
    #正态近似法 总体均数lambda>=20
    u <- (x-lambda)/sqrt(lambda)
    #双侧
    pt <- 2*pnorm(abs(u),lower.tail=F)
    #单侧
    ps <- pnorm(abs(u),lower.tail=F)
    pn <- data.frame(method='Poisson分布正态近似法',x=x,size=size,prob=prob,lambda=lambda,type=c('双侧','单侧'),stat=abs(u),v=Inf,p=c(pt,ps),alpha=alpha)
  }else{
    pn <- data.frame(method='Poisson分布正态近似法',x=x,size=size,prob=prob,lambda=lambda,type=NA,stat=NA,v=NA,p=NA,alpha=alpha)
  }
  
  pdata <- rbind(pp,pn)
  #根据alpha水平，判断是否显著
  pdata$value[pdata$p<alpha] <- '显著'
  pdata$value[pdata$p>=alpha] <- '不显著'
  
  pdata$recommend[pdata$lambda>=20&pdata$method=='Poisson分布正态近似法'] <- '推荐'
  pdata$recommend[pdata$lambda>0&pdata$lambda<20&pdata$method=='Poisson分布精确计算法'] <- '推荐'
  pdata$p <- round(pdata$p,5)
  return(pdata) 
  
}

shinyServer(function(input, output,session) {
  
  withProgress({
    setProgress(message = "数据加载中......")
  #先从老朱那里取到themeid和一堆系统参数stat开头、数据筛选条件
  #observe里面支持全局赋值，否则后面output里面不认识，不同于shinyServer函数外面的变量可以被认识
  observe({
    urlkey <<- parseQueryString(session$clientData$url_search)
    if(length(urlkey)==0){
    urlkey <<- list(theme_id='1293',
                   I_report_year='1987',
                   #report_district='杨浦区',
                   stat_side_type='双侧',stat_alpha0='0.05',stat_conf='0.95',stat_total_rate='50',stat_rare_rate='0.05')
    }
    ##检验的单双侧(全局)
    side_type <<- urlkey$stat_side_type
    ##显著性水平(全局)
    alpha0 <<- as.numeric(urlkey$stat_alpha0)
    ##置信水平(全局)
    conf <<- as.numeric(urlkey$stat_conf)
    #总体的阳性率%（默认为阳性的率，二项分布检验用）
    total_rate <<- as.numeric(urlkey$stat_total_rate)
    #罕见水平界定
    rare_rate <<- as.numeric(urlkey$stat_rare_rate)

    #阳性事件的总体均数(poisson分布检验用) 可以带单位如每年，每日，可以通过n*total_rate获取

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
      # names(dic) <- c('dicname','dickey')
      # dic$pclass <- c('+','-')#增加一个阳性阴性的标识符字段，后续由老丁自动从代码里传给我
      names(dic) <- c('dicname','dickey','pclass')
      if(is.na(unique(dic$pclass))&&length(unique(dic$pclass))==1){dic$pclass <- c('+','-')}#如果pclass全缺失则给个默认值，老丁那边已经做了控制
    }else{
      dic <- data.frame(dicname=character(0),dickey=character(0),pclass=character(0))
    }
    
    # #1.1 构成分析表
    freq <- as.data.frame(table(data,useNA='ifany'),stringsAsFactors = F)
    freq <- freq[order(freq$Freq,decreasing = T),]
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
        freq <- left_join(freq,dic,by=c('data'='dickey'))[,1:5]#根据代码翻译成中文，不在代码内的保持原样
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
      freq0 <- freq[,1:2]
    }

    

    #1.2疑似异常值分布表freq1
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
    bljy <- data.frame(a0='比例检验',a1='X-squared',a2='双侧',a21=conf,a3=round(unname(pr$statistic),2),a4=round(pr$p.value,4),
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
      bljy <- data.frame(a0='比例检验',a1='X-squared',a2='双侧',a21=conf,a3=NA,a4=NA,
                         a5=NA,a6='各个分类的比例相同',a7='不能进行比例检验',stringsAsFactors = F)
    }
    
    #2.2二项分布检验与poisson分布检验
    #二项分布检验:利用总体参数传入的阳性率total_rate进行判断
    withProgress({
      setProgress(message = "正在进行二项分布检验......")
    if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)>0){
      datatest <- left_join(freq0,dic,by=c('data'='dickey'))
      size0 <- sum(freq0$Freq)#试验总次数
      pnum <- datatest$Freq[datatest$pclass=='+']#阳性次数
      if(isempty(pnum)){pnum <- 0}
      pbi <- ks.test(x=pnum,y="pbinom",size=size0,prob=total_rate/100,alternative = 'two.sided')
      bijy <- data.frame(a0='二项分布检验',a1='D',a2=total_rate,a21=round(pnum/size0*100,2),
                         a3=round(unname(pbi$statistic),2),a4=round(pbi$p.value,4),
                         a5=NA,a6='符合二项分布',a7=NA,a8=size0,a9=pnum,stringsAsFactors = F)
      
      if(is.na(bijy$a4[1])){
        bijy$a7[1] <- NA
      }else if(bijy$a4[1]<0.05){
        bijy$a7[1] <- '拒绝零假设'
      }else if(bijy$a4[1]>=0.05){
        bijy$a7[1] <- '不能拒绝零假设'
      }else{
        bijy$a7[1] <- NA
      }
      
    }else if(nrow(freq0)>0&&is.na(total_rate)&&nrow(dic)>0){
      datatest <- left_join(freq0,dic,by=c('data'='dickey'))
      size0 <- sum(freq0$Freq)#试验总次数
      pnum <- datatest$Freq[datatest$pclass=='+']#阳性次数
      if(isempty(pnum)){pnum <- 0}
      bijy <- data.frame(a0='二项分布检验',a1='D',a2=NA,a21=round(pnum/size0*100,2),a3=NA,a4=NA,
                         a5=NA,a6='符合二项分布',a7='总体率为空，不能进行二项分布检验',a8=size0,a9=pnum,stringsAsFactors = F) 
    }else if(nrow(freq0)<=0&&!is.na(total_rate)&&nrow(dic)>0){
      bijy <- data.frame(a0='二项分布检验',a1='D',a2=total_rate,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='符合二项分布',a7='无数据，不能进行二项分布检验',a8=0,a9=NA,stringsAsFactors = F)
    }else if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)==0){
      size0 <- sum(freq0$Freq)#试验总次数
      bijy <- data.frame(a0='二项分布检验',a1='D',a2=total_rate,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='符合二项分布',a7='代码为空，不能识别阳性类别',a8=size0,a9=NA,stringsAsFactors = F) 
    }else{
      bijy <- data.frame(a0='二项分布检验',a1='D',a2=NA,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='符合二项分布',a7='不能进行二项分布检验',a8=NA,a9=NA,stringsAsFactors = F)
    }
    })
    #poisson分布检验
    #poisson分布的4个特点：比例、相等（事件发生的概率不变）、独立（各个观察间相互独立）、罕见
    # 一小段时间内事件发生的概率与该段时间的长短呈比例(proportional)
    # 在一个极小时间段内事件发生两次以上的概率可以忽略(negligible)
    # 无论时间先后，事件在一段给定时间内发生的概率稳定(stable)
    # 非重叠时间段内事件发生的概率相互独立(independent)
    #poisson分布检验:利用总体参数传入的阳性均数lambda进行判断
    #如何定义罕见？目前默认为5%以下
    withProgress({
      setProgress(message = "正在进行poisson分布检验......")
    if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)>0){
      datatest1 <- left_join(freq0,dic,by=c('data'='dickey'))
      pnum <- datatest1$Freq[datatest1$pclass=='+']#阳性次数（只能有一个值，多个值需要提前处理）
      if(isempty(pnum)){pnum <- 0}
      lambda0 <- sum(freq0$Freq)*total_rate/100
      ppo <- ks.test(x=pnum,y="ppois",lambda=lambda0,alternative = 'two.sided')
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=lambda0,a21=pnum,a22=total_rate,
                         a3=round(unname(ppo$statistic),2),a4=round(ppo$p.value,4),
                         a5=NA,a6='符合Poisson分布',a7=NA,a8=sum(freq0$Freq),stringsAsFactors = F)
      
      if(is.na(psjy$a4[1])){
        psjy$a7[1] <- NA
      }else if(psjy$a4[1]<0.05){
        psjy$a7[1] <- '拒绝零假设'
      }else if(psjy$a4[1]>=0.05){
        psjy$a7[1] <- '不能拒绝零假设'
      }else{
        psjy$a7[1] <- NA
      }
      
    }else if(nrow(freq0)>0&&is.na(total_rate)&&nrow(dic)>0){
      datatest1 <- left_join(freq0,dic,by=c('data'='dickey'))
      pnum <- datatest1$Freq[datatest1$pclass=='+']#阳性次数（只能有一个值，多个值需要提前处理）
      if(isempty(pnum)){pnum <- 0}
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=NA,a21=pnum,a22=total_rate,a3=NA,a4=NA,
                         a5=NA,a6='符合Poisson分布',a7='总体均数为空，不能进行Poisson分布检验',a8=sum(freq0$Freq),stringsAsFactors = F) 
    }else if(nrow(freq0)<=0&&!is.na(total_rate)&&nrow(dic)>0){
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=0,a21=NA,a22=total_rate,a3=NA,a4=NA,
                         a5=NA,a6='符合Poisson分布',a7='无数据，不能进行Poisson分布检验',a8=0,stringsAsFactors = F)
    }else if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)==0){
      lambda0 <- sum(freq0$Freq)*total_rate/100
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=lambda0,a21=NA,a22=total_rate,a3=NA,a4=NA,
                         a5=NA,a6='符合Poisson分布',a7='代码为空，不能识别阳性类别',a8=sum(freq0$Freq),stringsAsFactors = F) 
    }else{
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=NA,a21=NA,a22=total_rate,a3=NA,a4=NA,
                         a5=NA,a6='符合Poisson分布',a7='不能进行Poisson分布检验',a8=NA,stringsAsFactors = F)
    }
    
    #以5%为分界，作为罕见的约束条件
    psjy$a6[psjy$a22/100>rare_rate] <- paste0('发生率高于',rare_rate*100,'%，不符合Poisson分布的罕见条件')
    psjy$a7[psjy$a22/100>rare_rate] <- '拒绝零假设'
    
    #a8为试验总数，a7为解读，a2为lambda，a21为阳性次数，a22总体率，a5为自由度
    })

    #2.3 与总体率比较
    #参数方法
    #符合二项分布（包括精确法和正态近似法）
    #符合poisson分布（包括精确法和正态近似法）
    #不符合以上两个分布的采用拟合优度方法
    if(bijy$a7[1]=='不能拒绝零假设'){
      #精确法和正态近似法
      bpbj1 <- binom.p(bijy$a9,bijy$a8,total_rate/100,alpha0)
      
    }else{
      #下一步需要区分有无数据进行拟合优度检验，可以同时存在3种方法
      bpbj1 <- data.frame(method=character(0),x=numeric(0),size=numeric(0),prob=numeric(0),lambda=numeric(0),
                         type=character(0),stat=numeric(0),v=numeric(0),p=numeric(0),alpha=numeric(0),
                         value=character(0),recommend=character(0))
    } 
    
    if(psjy$a7[1]=='不能拒绝零假设'){
      bpbj2 <- poisson.p(psjy$a21,psjy$a8,total_rate/100,alpha0)
      
    }else{
      #下一步需要区分有无数据进行拟合优度检验，可以同时存在3种方法
      bpbj2 <- data.frame(method=character(0),x=numeric(0),size=numeric(0),prob=numeric(0),lambda=numeric(0),
                         type=character(0),stat=numeric(0),v=numeric(0),p=numeric(0),alpha=numeric(0),
                         value=character(0),recommend=character(0))
    }
    
    bpbj <- rbind(bpbj1,bpbj2)
    
    # #精确二项分布检验(已经涵盖在上述bpbj中了)
    # if(nrow(freq0)>0&&!is.na(total_rate)){
    #   pr1 <- binom.test(freq0$Freq[1],sum(freq0$Freq),p=total_rate/100,alternative = 'two.sided',conf.level = conf)
    #   exjy <- data.frame(a0='精确二项分布检验',a1='Num',a2='双侧',a21=conf,a3=round(unname(pr1$statistic),2),a4=round(pr1$p.value,4),
    #                      a5=unname(pr1$parameter),a6='样本率与总体率相同',a7=NA,stringsAsFactors = F)
    #   
    #   if(is.na(exjy$a4[1])){
    #     exjy$a7[1] <- NA
    #   }else if(exjy$a4[1]<0.05){
    #     exjy$a7[1] <- '拒绝零假设'
    #   }else if(exjy$a4[1]>=0.05){
    #     exjy$a7[1] <- '不能拒绝零假设'
    #   }else{
    #     exjy$a7[1] <- NA
    #   }
    # }else{
    #   exjy <- data.frame(a0='二项分布检验',a1='Num',a2='双侧',a21=conf,a3=NA,a4=NA,
    #                      a5=NA,a6='样本率与总体率相同',a7='不能进行二项分布检验',stringsAsFactors = F)
    # }
    
    #与总体率比较：
    #非参数方法：多项式分布的拟合优度检验
    #http://www.r-tutor.com/elementary-statistics/goodness-fit/multinomial-goodness-fit
    if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)>0){
      size0 <- sum(freq0$Freq)#试验总次数
      datatest <- left_join(freq0,dic,by=c('data'='dickey'))
      pnum <- datatest$Freq[datatest$pclass=='+']#阳性次数,目前只支持1个
      if(isempty(pnum)){pnum <- 0}
      pr2 <- chisq.test(x=c(pnum,size0-pnum),p=c(total_rate/100,1-total_rate/100),correct = T)
      nhjy <- data.frame(a0='拟合优度检验',a1='X-squared',a2=total_rate,a21=round(freq0$Freq[1]/sum(freq0$Freq)*100,2),
                         a3=round(unname(pr2$statistic),2),a4=round(pr2$p.value,4),
                         a5=unname(pr2$parameter),a6='样本率与总体率相同',a7=NA,stringsAsFactors = F)
      
      if(is.na(nhjy$a4[1])){
        nhjy$a7[1] <- NA
      }else if(nhjy$a4[1]<0.05){
        nhjy$a7[1] <- '拒绝零假设'
      }else if(nhjy$a4[1]>=0.05){
        nhjy$a7[1] <- '不能拒绝零假设'
      }else{
        nhjy$a7[1] <- NA
      }
    }else if(is.na(total_rate)&&nrow(freq0)<=0){
      nhjy <- data.frame(a0='拟合优度检验',a1='X-squared',a2=total_rate,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='样本率与总体率相同',a7='总体率为空且数据为空，不能进行拟合优度检验',stringsAsFactors = F) 
    }else if(is.na(total_rate)){
      nhjy <- data.frame(a0='拟合优度检验',a1='X-squared',a2=total_rate,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='样本率与总体率相同',a7='总体率为空，不能进行拟合优度检验',stringsAsFactors = F) 
    }else if(nrow(freq0)<=0){
      nhjy <- data.frame(a0='拟合优度检验',a1='X-squared',a2=total_rate,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='样本率与总体率相同',a7='无数据',stringsAsFactors = F)
    }else if(nrow(dic)==0){
      nhjy <- data.frame(a0='拟合优度检验',a1='X-squared',a2=total_rate,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='样本率与总体率相同',a7='无法识别阳性数',stringsAsFactors = F)
    }else{
      nhjy <- data.frame(a0='拟合优度检验',a1='X-squared',a2=NA,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='样本率与总体率相同',a7='不能进行拟合优度检验',stringsAsFactors = F)
    }


    
    
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
    if(nrow(data)>0&&!is.na(total_rate)){
      if(nrow(dic)>0){
        p1 <- paste0('1.',dmname,'数据中总计数为',sum(freq2$Freq),'，符合代码值的计数为',sum(freq3$Freq),'，占比',round(sum(freq3$Freq)/sum(freq2$Freq),2)*100,'%，缺失的计数为',ifelse(isempty(freq$Freq[freq$data=='缺失值']),0,freq$Freq[freq$data=='缺失值']),'，占比',ifelse(isempty(freq$prop[freq$data=='缺失值']),0,freq$prop[freq$data=='缺失值']),'%；')
        p2 <- paste0('2.在非缺失值中，疑似异常值个数为',sum(freq4$Freq),'个，占比',sum(freq4$prop),'%；（疑似异常值判断方法：代码值域）')
      }else{
        p1 <- paste0('1.',dmname,'数据中总计数为',sum(freq2$Freq),'，缺失的计数为',ifelse(isempty(freq$Freq[freq$data=='缺失值']),0,freq$Freq[freq$data=='缺失值']),'，占比',ifelse(isempty(freq$prop[freq$data=='缺失值']),0,freq$prop[freq$data=='缺失值']),'%；')
        p2 <- "2.无代码映射，无法判断异常值！"
      }
    p3 <- paste0('3.根据',dmname,'数据的实际情况进行比例等同检验，其零假设为',bljy$a6[1],'，结果为：',bljy$a7[1],'；')
    p4 <- paste0('4.根据',dmname,'数据的实际情况进行二项分布检验，其零假设为',bijy$a6[1],'，结果为：',bijy$a7[1],'；')
    p5 <- paste0('5.根据',dmname,'数据的实际情况进行Poisson分布检验，其零假设为',psjy$a6[1],'，结果为：',psjy$a7[1],'；')
    p6 <- paste0('6.根据',dmname,'数据的实际情况与总体率进行比较，建议采用推荐的方法；')
    p7 <- paste0('7.根据',dmname,'数据的实际情况进行拟合优度检验，其零假设为',nhjy$a6[1],'，结果为：',nhjy$a7[1],'；')
    p8 <- paste0('8.根据样本量和构成比的实际情况采用',unique(cl$method[which(cl$isok=='推荐')]),'方法，确定各分类的构成比的',as.numeric(unique(cl$conf.level[which(cl$isok=='推荐')]))*100,'%的置信区间；')
    }else if(nrow(data)>0&&is.na(total_rate)){
      if(nrow(dic)>0){
        p1 <- paste0('1.',dmname,'数据中总计数为',sum(freq2$Freq),'，符合代码值的计数为',sum(freq3$Freq),'，占比',round(sum(freq3$Freq)/sum(freq2$Freq),2)*100,'%，缺失的计数为',ifelse(isempty(freq$Freq[freq$data=='缺失值']),0,freq$Freq[freq$data=='缺失值']),'，占比',ifelse(isempty(freq$prop[freq$data=='缺失值']),0,freq$prop[freq$data=='缺失值']),'%；')
        p2 <- paste0('2.在非缺失值中，疑似异常值个数为',sum(freq4$Freq),'个，占比',sum(freq4$prop),'%；（疑似异常值判断方法：代码值域）')
      }else{
        p1 <- paste0('1.',dmname,'数据中总计数为',sum(freq2$Freq),'，缺失的计数为',ifelse(isempty(freq$Freq[freq$data=='缺失值']),0,freq$Freq[freq$data=='缺失值']),'，占比',ifelse(isempty(freq$prop[freq$data=='缺失值']),0,freq$prop[freq$data=='缺失值']),'%；')
        p2 <- "2.无代码映射，无法判断异常值！"
      }
      p3 <- paste0('3.根据',dmname,'数据的实际情况进行比例等同检验，其零假设为',bljy$a6[1],'，结果为：',bljy$a7[1],'；')
      p4 <- paste0('4.无法获取总体率，不能进行二项分布检验；')
      p5 <- paste0('5.无法获取总体率，不能进行Poisson分布检验；')
      p6 <- paste0('6.无法获取总体率，不能进行与总体率进行比较；')
      p7 <- paste0('7.无法获取总体率，不能进行拟合优度检验；')
      p8 <- paste0('8.根据样本量和构成比的实际情况采用',unique(cl$method[which(cl$isok=='推荐')]),'方法，确定各分类的构成比的',as.numeric(unique(cl$conf.level[which(cl$isok=='推荐')]))*100,'%的置信区间；')
      
    }else{
     p1 <- '无数据'
     p2 <- ''
     p3 <- ''
     p4 <- ''
     p5 <- ''
     p6 <- ''
     p7 <- ''
     p8 <- ''
    }
    z <- list(obj=obj,dmname=dmname,total_rate=total_rate,
              data=data,
              dic=dic,freq0=freq0,
              freq=freq,freq1=freq1,bljy=bljy,bpbj=bpbj,nhjy=nhjy,bijy=bijy,psjy=psjy,
              cl=cl,
              filterT=filterT,
              #sketch2=sketch2,
              p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6,p7=p7,p8=p8)
    
    return(z)
  })
})
  
#筛选条件表
output$filter1 <- renderTable({
    b <- z()
    filterT <- b$filterT
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

output$filter9 <- renderTable({
  b <- z()
  filterT <- b$filterT
  #names(filterT) <- c('dmkey','筛选字段','筛选值')
  filterT[,c(3,2)]
},colnames = F)
output$filter10 <- renderTable({
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
  names(bljy) <- c('检验方法','统计量','单双侧','置信水平','统计值','P值','自由度','零假设','解读')
  DT::datatable(bljy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的比例检验表'))
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
#二项分布检验
output$table33 <- DT::renderDataTable({
  b <- z()
  bijy <- b$bijy[,1:9]
  names(bijy) <- c('检验方法','统计量','总体率(%)','样本率(%)','统计值','P值','自由度','零假设','解读')
  DT::datatable(bijy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的二项分布检验表'))
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
#Poisson分布检验
output$table34 <- DT::renderDataTable({
  b <- z()
  psjy <- b$psjy[,1:10]
  names(psjy) <- c('检验方法','统计量','总体均数','阳性数','总体率(%)','统计值','P值','自由度','零假设','解读')
  DT::datatable(psjy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的Poisson分布检验表'))
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

#2.2 与总体率比较
#二项分布和Poisson分布法
output$table31 <- DT::renderDataTable({
  b <- z()
  bpbj <- b$bpbj
  names(bpbj) <- c('检验方法','阳性次数','实验次数','总体率','总体均数','假设类别','统计量','自由度','P值','显著水平','解读','是否推荐')
  DT::datatable(bpbj,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的与总体率比较检验表（二项分布和Poisson分布法）'))
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
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(12, fontWeight = styleEqual('推荐',  'bold'),
              color = styleEqual('推荐',  'green'))%>%
    formatStyle(12,target = 'row',
                backgroundColor = styleEqual('推荐',  '#FFFF99'))
  
})
#拟合优度检验法
output$table32 <- DT::renderDataTable({
  b <- z()
  nhjy <- b$nhjy
  names(nhjy) <- c('检验方法','统计量','总体率(%)','样本率(%)','统计值','P值','自由度','零假设','解读')
  DT::datatable(nhjy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'的与总体率比较检验表（拟合优度检验法）'))
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
                  htmltools::strong(paste0(b$obj,b$dmname,'的构成比估计表'))
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
    s1$vname <- factor(s1$vname,levels = s1$vname,ordered = T)#保证纵轴按升序排列
    p <- plot_ly(s1, color = I("gray80")) %>%
      add_segments(x = ~lower, xend = ~upper, y = ~vname, yend = ~vname, showlegend = FALSE) %>%
      add_markers(x = ~lower, y = ~vname, name = "CI下限", color = I("darkgreen")) %>%
      add_markers(x = ~proportion, y = ~vname, name = "均值", color = I("black")) %>%
      add_markers(x = ~upper, y = ~vname, name = "CI上限", color = I("red")) %>%
      layout(font=list(family="Microsoft YaHei"),
        title = paste0('<b>',b$obj,'中不同',b$dmname,'的构成比',clf,'置信区间图(',meth,')</b>'),
        xaxis = list(title = "构成比（%）",showline=F,zeroline = FALSE),
        yaxis = list(title = "",showline=F),
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
#二项分布图
output$plot2 <- renderPlot({
  b <- z()
  total_rate <- b$total_rate
  freq0 <- b$freq0
  bijy <- b$bijy
  psjy <- b$psjy
  dic <- b$dic
  
  if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)>0){
    show("plot2")
  } else {
    hide("plot2")
  }
  if(nrow(freq0)>0&&!is.na(total_rate)&&nrow(dic)>0) {
    size0 <- bijy$a8[1]#试验总次数
    pnum <- psjy$a21[1]#阳性次数
    plot(dbinom(x=c(0:pnum),size=size0,prob=total_rate/100),type = "h", col = "red", lwd = 5,main=paste0('二项分布图(n=',size0,',pi=',total_rate/100,')'),xlab='发生次数',ylab='概率')
    
  }else{
    return(NULL)
  }
  
})
#poisson分布图
output$plot3 <- renderPlot({
  b <- z()
  freq0 <- b$freq0
  psjy <- b$psjy
  lambda0 <- b$psjy$a2
  dic <- b$dic
  
  if(nrow(freq0)>0&&!is.na(lambda0)&&nrow(dic)>0){
    show("plot3")
  } else {
    hide("plot3")
  }
  if(nrow(freq0)>0&&!is.na(lambda0)&&nrow(dic)>0) {
    pnum <- psjy$a21[1]
    plot(dpois(x=c(0:pnum),lambda = lambda0),type = "h", col = "red", lwd = 5,main=paste0('Poisson分布图(lambda=',lambda0,')'),xlab='发生次数',ylab='概率')
  }else{
    return(NULL)
  }
  
})

#总体解读
output$text1 <- renderPrint({
  b <- z()
  #print(b$filterT)
  cat(b$p1,b$p2,b$p3,b$p4,b$p5,b$p6,b$p7,b$p8,sep='\n\n')

})

})
