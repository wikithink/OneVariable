#单个离散变量（定量）的描述推断分析
#王文祥
#2017-12-18
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

#定义表1一般统计描述表容器
if(1==1){
  sketch1  <-  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, '指标'),
        th(class = 'dt-center',colspan = 4, '一般统计'),
        th(rowspan = 2, ' '),
        th(class = 'dt-center',colspan = 6, '集中趋势'),
        th(rowspan = 2, ' '),
        th(class = 'dt-center',colspan = 7, '离散趋势'),
        th(rowspan = 2, ' '),
        th(class = 'dt-center',colspan = 2, '分布形态')
      ),
      tr(
        th(class = 'dt-center',rowspan = 1, '总频数'),
        th(class = 'dt-center',rowspan = 1, '有效频数(%)'),
        th(class = 'dt-center',rowspan = 1, '缺失频数(%)'),
        th(class = 'dt-center',rowspan = 1, '唯一数'),
        
        th(class = 'dt-center',rowspan = 1, '算术均数'),
        th(class = 'dt-center',rowspan = 1, '几何均数'),
        th(class = 'dt-center',rowspan = 1, '中位数'),
        th(class = 'dt-center',rowspan = 1, '百分位数(p5-25-75-95)'),
        th(class = 'dt-center',rowspan = 1, '众数'),
        th(class = 'dt-center',rowspan = 1, '均数标准误'),
        
        th(class = 'dt-center',rowspan = 1, '极差'),
        th(class = 'dt-center',rowspan = 1, '最大值'),
        th(class = 'dt-center',rowspan = 1, '最小值'),
        th(class = 'dt-center',rowspan = 1, '四分位数间距'),
        th(class = 'dt-center',rowspan = 1, '方差'),
        th(class = 'dt-center',rowspan = 1, '标准差'),
        th(class = 'dt-center',rowspan = 1, '变异系数'),
        
        th(class = 'dt-center',rowspan = 1, '偏度系数'),
        th(class = 'dt-center',rowspan = 1, '峰度系数')
      ))))  
}

shinyServer(function(input, output,session) {
  
  withProgress({
    setProgress(message = "数据加载中......")
  #先从老朱那里取到themeid和一堆系统参数stat开头、数据筛选条件
  #observe里面支持全局赋值，否则后面output里面不认识，不同于shinyServer函数外面的变量可以被认识
  observe({
    urlkey <<- parseQueryString(session$clientData$url_search)
    if(length(urlkey)==0){
    urlkey <<- list(theme_id='1557',
                   I_report_year='2011',
                   report_district='杨浦区',
                   stat_side_type='双侧',stat_alpha0='0.05',stat_conf='0.95',stat_classRange='10',
                   stat_max0='',stat_min0='',stat_u0='30',stat_sigma0='18',stat_med0='27')
    }
    ##检验的单双侧(全局)
    side_type <<- urlkey$stat_side_type
    ##显著性水平(全局)
    alpha0 <<- as.numeric(urlkey$stat_alpha0)
    ##置信水平(全局)
    conf <<- as.numeric(urlkey$stat_conf)
    ##参考值范围(全局)
    max0 <<- as.numeric(urlkey$stat_max0)
    min0 <<- as.numeric(urlkey$stat_min0)
    ##总体平均数(全局)
    u0 <<- as.numeric(urlkey$stat_u0)
    ##总体标准差(全局)
    sigma0 <<- as.numeric(urlkey$stat_sigma0)
    ##总体中位数(全局)
    med0 <<- as.numeric(urlkey$stat_med0)
    
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
    
    #filterT <- merge(thdm,thd,by='dmkey')
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
    #去除NA
    a <-na.omit(data[,1])
    withProgress({
      setProgress(message = "正在计算指标......")
    s1 <- Hmisc::describe(data[,1],exclude.missing=T)
    #stat.desc函数进行正态检验时要求样本量必须大于等于3
    if(nrow(na.omit(data))>=3){
      s2 <- stat.desc(data[,1],norm=T,p=conf) 
    }else{
      s2 <- stat.desc(data[,1],p=conf)
    }
    
    #总频数
    b1 <- unname(s2[1]+ s2[2]+s2[3])
    #有效频数
    if(b1>0){
    b2 <- paste0(s2[1],'(',round(s2[1]/b1*100,2),')')
    }else{
      b2 <- s2[1]
    }
    #缺失频数
    if(b1>0){
    b3 <- paste0(s2[2]+s2[3],'(',round((s2[2]+s2[3])/b1*100,2),')')
    }else{
      b3 <- s2[2]+s2[3]
    }
    if(b1!=0&(s2[2]+s2[3])/b1>0.15){
      b31 <- '缺失率较高'
    }else if(b1!=0&(s2[2]+s2[3])/b1>0){
      b31 <- '缺失率尚可'
    }else{
      b31 <- '无缺失'
    }
    #唯一数
    b32 <- as.numeric(unname(s1$counts[3]))
    
    #算术均数
    b4 <- round(unname(s2[9]),2)
    #几何均数
    if(b1>0){
    b5 <- round(exp(mean(log(data[,1]),na.rm=T)),3)
    }else{
      b5 <- '无法计算'
    }
    #中位数
    b6 <- round(unname(s2[8]),2)
    #百分位数与四分位数间距Q=p75-p25
    b71 <- psych::describe(a,quant=c(0.05,0.25,0.75,0.95),IQR=F,skew = F)
    if(is.na(b71$Q0.25)){
      b7 <- '无法计算'
      b13 <- '无法计算'
    }else{
      b7 <- paste(b71$Q0.05,b71$Q0.25,b71$Q0.75,b71$Q0.95,sep=' ')
      b13 <- b71$Q0.75-b71$Q0.25
    }
    
    #众数
    s4 <- table(data[,1])
    ##包括NA
    #s4 <- table(data[,1],useNA = "ifany")
    #众数本身
    b8 <-  as.numeric(names(s4[order(s4,decreasing = T)[1]]))
    #众数的频次
    #b81 <- unname(s4[order(s4,decreasing = T)[1]])
    
    #均数标准误
    b9 <- round(unname(s2[10]),2)
    
    #极差
    b10 <- round(unname(s2[6]),2)
    #最大值
    b11 <- round(unname(s2[5]),2)
    #最小值
    b12 <- round(unname(s2[4]),3)
    
    #方差
    b14 <- round(unname(s2[12]),2)
    #标准差
    b15 <- round(unname(s2[13]),2)
    #变异系数cv=标准差/平均数
    b16 <- round(unname(s2[14]),2)
    
    #几何均数解读
    if(!is.na(b6)&(b11/b6>=4)){
      b51 <- '适用'
    }else{
      b51 <- '不适用'
    }
    
    if(nrow(na.omit(data))>=3){
    #偏度系数
    b17 <- paste0(round(s2[15],2),'(',round(s2[16],2),')')
    #峰度系数
    b18 <- paste0(round(s2[17],2),'(',round(s2[18],2),')')
    
    #偏度系数解读
    if(s2[15]==0){
      b171 <- '分布对称'
    }else
      if(s2[15]>0){
        b171 <- '正偏峰'
      }else{
        b171 <- '负偏峰'
      }
    
    if(abs(s2[16])>1){
      b172 <- '统计意义显著'
    }else{
      b172 <- '统计意义不显著'
    }
    
    b173 <- paste0(b171,',',b172)
    
    #峰度系数点评
    if(s2[17]==0){
      b181 <- '同正态分布'
    }else
      if(s2[17]>0){
        b181 <- '较正态分布峰尖峭'
      }else{
        b181 <- '较正态分布峰平阔'
      }
    
    if(abs(s2[18])>1){
      b182 <- '统计意义显著'
    }else{
      b182 <- '统计意义不显著'
    }
    
    b183 <- paste0(b181,',',b182)
    }else{
      b17 <- NA
      b18 <- NA
      b173 <- NA
      b183 <- NA
    }
    #1.1基本统计表(按照新需求，只展示一行，这里不作变更，后面有解读数据需要用到)
    dat1 <- data.frame(d1=c('指标值','解读'),
                       d3=c(b1,NA),
                       d4=c(b2,NA),
                       d5=c(b3,b31),
                       d6=c(b32,NA),
                       d7=c('',''),
                       d8=c(b4,NA),
                       d9=c(b5,b51),
                       d10=c(b6,NA),
                       d11=c(b7,NA),
                       d12=c(b8,NA),
                       d13=c(b9,NA),
                       d14=c('',''),
                       d15=c(b10,NA),
                       d16=c(b11,NA),
                       d17=c(b12,NA),
                       d18=c(b13,NA),
                       d19=c(b14,NA),
                       d20=c(b15,NA),
                       d21=c(b16,NA),
                       d22=c('',''),
                       d23=c(b17,b173),
                       d24=c(b18,b183))
    })
    #1.2 频率分布表
    #e1=组距,e2=频数,e3=频率(%),e4=累计频数,e5=累计频率
    #不计缺失：e31=频率(%),e41=累计频数,e51=累计频率
    #注意：频率分布表，只要有1个有效数据即可
    if(nrow(na.omit(data))>0){
    h1 <- hist(data[,1],plot=F)
    #组距=h1$breaks[2]-h1$breaks[1]
    h2 <- data.frame(e1=paste0(h1$breaks[-length(h1$breaks)],'-'),
                     e2=h1$counts,
                     e3=h1$density)
    h2$e3 <- h2$e2/b1*100
    h2$e31 <- round(h2$e2/(b1-unname(s2[2]+s2[3]))*100,2)#不计缺失值

    h2 <- rbind(h2,
                data.frame(e1=c('缺失值','合计'),
                           e2=c(unname(s2[2]+s2[3]),b1),
                           e3=c(unname(s2[2]+s2[3])/b1*100,100),
                           e31=c(NA,100)))
    h2$e3 <- round(h2$e3,2)
    h2$e4 <- cumsum(h2$e2)
    h2$e41 <- c(h2$e4[1:(length(h2$e4)-2)],NA,NA)#不计缺失值
    h2$e5 <- round(cumsum(h2$e3),2)
    h2$e51 <- round(cumsum(h2$e31),2)#不计缺失值
    h2$e4[nrow(h2)] <- NA
    h2$e5[nrow(h2)] <- NA
    }else{
      h1 <- data.frame(breaks=numeric(0),counts=numeric(0),density=numeric(0))
      h2 <- data.frame(e1=character(0),e2=numeric(0),e3=numeric(0),e31=numeric(0),e4=numeric(0),e41=numeric(0),e5=numeric(0),e51=numeric(0))
    }
    #1.3疑似异常值分布表
    ##非正态分布中采用箱式图来判断异常值，标准为<Q1-1.5*IQR 或 >Q3+1.5*IQR
    ##如符合正态分布，则考虑使用均数+-3个标准差来判断
    ##如果可以读入该数据元的参考值的限值范围，则使用该限值范围，目前未考虑这个代码
    if(length(a)>0){
      point1 <- b71$Q0.25-1.5*b13
      point2 <- b71$Q0.75+1.5*b13
      point3 <- sum(data[,1]<point1,na.rm = T)+sum(data[,1]>point2,na.rm = T)
      dat2 <- data.frame(f1=c('正常值','疑似异常值','异常值*','缺失值','合计'),
                         f2=c(unname(s2[1]-point3),point3,0,unname(s2[2]+s2[3]),b1))
      dat2$f3 <- round(dat2$f2/b1*100,2) 
      tft <- paste0('**1.疑似异常值判断方法：百分位数间距法；','**2.疑似异常值判断标准：小于',round(point1,1),'或大于',round(point2,1),'；**3.异常值判断方法：参考值域范围')
    }else{
      dat2 <- data.frame(f1=character(0),f2=numeric(0),f3=numeric(0))
      tft <- '**1.疑似异常值判断方法：百分位数间距法；**2.异常值判断方法：参考值域范围'
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
        tr(td(class = 'dt-left',colspan = 3,tft))
      )
    ))

    #2.1正态性检验
    #SPSS规定:当有效样本含量3 ≤n ≤5000 时,结果以Shapiro - Wilk (W 检验) 为难,当样本含量n > 5000 结果以Kolmogorov - Smirnov 为准。
    #SAS规定:当有效样本含量n ≤2000 时,结果以Shapiro - Wilk (W 检验) 为准,当样本含量n >2000 时,结果以Kolmogorov - Smirnov (D 检验) 为准。
    withProgress({
      setProgress(message = "正在进行假设检验......")
    if(nrow(na.omit(data))>=3){
      k1 <- shapiro.test(a)
      k2 <- ks.test(x=a,y="pnorm",mean=mean(a),sd=sd(a),alternative = 't')

    if(k1$p.value<0.05){
      g51 <- '非正态性'
    }else{
      g51 <- '正态性'
    }

    if(k2$p.value<0.05){
      g52 <- '非正态性'
    }else{
      g52 <- '正态性'
    }

    if(unname(s2[1])<=5000&unname(s2[1])>=3){
      g61 <- '推荐'
      g62 <- NA
    }else if(unname(s2[1])<3){
      g61 <- '不适用'
      g62 <- '不适用'
    }else{
      g61 <- NA
      g62 <- '推荐'
    }
    norm1 <- data.frame(g1=c('Shapiro-Wilk正态性检验','One-sample Kolmogorov-Smirnov正态性检验'),
                        g2=c('W','D'),
                        g3=c(k1$statistic,k2$statistic),
                        g4=c(k1$p.value,k2$p.value),
                        g5=c(g51,g52),
                        g6=c(g61,g62),row.names = NULL)
    
    norm1[,c(3,4)] <- round(norm1[,c(3,4)],5)
    
    }else{
    norm1 <- data.frame(g1=c('Shapiro-Wilk正态性检验','One-sample Kolmogorov-Smirnov正态性检验'),
                        g2=c('W','D'),
                        g3=c(NA,NA),
                        g4=c(NA,NA),
                        g5=c(NA,NA),
                        g6=c(NA,NA),row.names = NULL)
}

    #2.1绘制QQ图

    #2.2离群点检验
    #单个变量不能做异常值检验，绘制离群箱线图替代

    #多个连续变量的相关性可以用到，图形比较漂亮
    #psych::outlier(x, plot = TRUE, bad = 5,na.rm = TRUE, xlab, ylab, ...)
    #pairs.panels(sat.d2,bg=c("yellow","blue")[(d3 > 25)+1],pch=21)

    #2.3 与总体均数或中位数比较

    #检验结果表
    #a0检验方法	a1统计量	a2单双侧	a3统计值	a4P值	a5自由度(υ)	a6零假设	a7解读	a8是否推荐
    jsjy <- data.frame(a0=c('单样本t检验','单样本Z检验','单样本符号秩和检验'),
                       a1=c('t','Z','V'),
                       a2=c(side_type,side_type,side_type),
                       a3=c(NA,NA,NA),
                       a4=c(NA,NA,NA),
                       a5=c(NA,NA,NA),
                       a6=c(NA,NA,NA),
                       a7=c(NA,NA,NA),
                       a8=c(NA,NA,NA))

    if(side_type=='双侧'){
      alter1 <- "two.sided"
      alter2 <- '等于'
    }else if(side_type=='单侧'&(s2[9]>u0|s2[8]>med0)){
      alter1 <- "greater"
      alter2 <- '大于'
    }else{
      alter1 <- "less"
      alter2 <- '小于'
    }

  if(nrow(na.omit(data))>=3){
    if(is.na(u0)&is.na(med0)){
      jd <- '总体参数未知，不能进行单样本假设检验'
    }else if(is.na(sigma0)&norm1$g5[which(norm1$g6=='推荐')]=='正态性'&nrow(na.omit(data))<=200){
      jg <- t.test(data[,1],mu=u0,alternative = alter1,conf.level = conf)
      jd <- paste0('建议进行单样本的',side_type,'t检验')
      jsjy$a3[1] <- unname(jg$statistic)
      jsjy$a4[1] <- round(unname(jg$p.value),4)
      jsjy$a5[1] <- unname( jg$parameter)
      jsjy$a6[1] <- paste0('样本的总体均数',alter2,u0)

    }else if(!is.na(sigma0)&norm1$g5[which(norm1$g6=='推荐')]=='正态性'&nrow(na.omit(data))>200){
      jg <- BSDA::z.test(x=na.omit(data[,1]),mu=u0,sigma.x=sigma0,alternative=alter1,conf.level = conf)
      jd <- paste0('建议进行单样本的',side_type,'Z检验')
      jsjy$a3[2] <- unname(jg$statistic)
      jsjy$a4[2] <- round(unname(jg$p.value),4)
      jsjy$a6[2] <- paste0('样本的总体均数',alter2,u0)
    }else if(!is.na(med0)){
      jg <- wilcox.test(x=data[,1],mu=med0,alternative = alter1,conf.int = T,conf.level = conf)
      jd <- paste0('建议进行单样本的',side_type,'Wilcox符合秩和检验')
      jsjy$a3[3] <- unname(jg$statistic)
      jsjy$a4[3] <- round(unname(jg$p.value),4)
      jsjy$a6[3] <- paste0('样本的总体中位数',alter2,med0)
    }else{
      jd <- '前置条件不满足，不能进行单样本假设检验'
    }

    ##解读结果
    for(i in 1:nrow(jsjy)){
      if(is.na(jsjy$a4[i])){
        jsjy$a7[i] <- NA
      }else if(jsjy$a4[i]<0.05){
        jsjy$a7[i] <- '拒绝零假设'
      }else if(jsjy$a4[i]>=0.05){
        jsjy$a7[i] <- '不能拒绝零假设'
      }else{
        jsjy$a7[i] <- NA
      }

      if(is.na(jsjy$a4[i])){
        jsjy$a8[i] <- NA
      }else{
        jsjy$a8[i] <- '推荐'
      }
    }
    #前置条件表
    #a0有效样本量 a1总体均数 a2总体中位数 a3总体标准差 a4置信水平 a5单双侧 a6是否正态 a7解读
    qztj <- data.frame(a0=nrow(na.omit(data)),a1=u0,a2=med0,a3=sigma0,a4=conf,a5=side_type,a6=norm1$g5[which(norm1$g6=='推荐')],a7=jd)
    
  }else{
    jsjy <- data.frame(a0=c('单样本t检验','单样本Z检验','单样本符号秩和检验'),
                       a1=c('t','Z','V'),
                       a2=c(side_type,side_type,side_type),
                       a3=c(NA,NA,NA),
                       a4=c(NA,NA,NA),
                       a5=c(NA,NA,NA),
                       a6=c(NA,NA,NA),
                       a7=c(NA,NA,NA),
                       a8=c(NA,NA,NA))
    jd <- '前置条件不满足，不能进行单样本假设检验'
    qztj <- data.frame(a0=nrow(na.omit(data)),a1=u0,a2=med0,a3=sigma0,a4=conf,a5=side_type,a6=NA,a7=jd)
  }
    
    #2.4 比例检验
    if(length(a)>1){
    pr <- prop.test(a,rep(sum(a),length(a)),alternative = alter1,conf.level = conf,correct = T)
    
    bljy <- data.frame(a0='比例检验',a1='X-squared',a2=side_type,a3=round(unname(pr$statistic),2),a4=round(pr$p.value,4),
                       a5=unname(pr$parameter),a6='各个对象的比例相同',a7=NA,stringsAsFactors = F)
    
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
      bljy <- data.frame(a0='比例检验',a1='X-squared',a2=side_type,a3=NA,a4=NA,
                         a5=NA,a6='各个对象的比例相同',a7='不能进行比例检验',stringsAsFactors = F)
    }
    
    #2.5 poisson分布检验
    #poisson分布的4个特点：比例、相等（事件发生的概率不变）、独立（各个观察间相互独立）、罕见
    # 一小段时间内事件发生的概率与该段时间的长短呈比例(proportional)
    # 在一个极小时间段内事件发生两次以上的概率可以忽略(negligible) 不符合
    # 无论时间先后，事件在一段给定时间内发生的概率稳定(stable)
    # 非重叠时间段内事件发生的概率相互独立(independent)
    #poisson分布检验:利用总体参数传入的阳性均数lambda进行判断
    #如何定义罕见？目前默认为5%以下
    #这里没有对罕见做约束，可自行判断
    if(length(a)>0&!is.na(u0)){
    pp <- ks.test(x=a,y="ppois",lambda=u0,alternative = alter1)
    psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=side_type,a21=u0,a3=round(unname(pp$statistic),2),a4=round(pp$p.value,4),
                       a5=NA,a6='符合poisson分布',a7=NA,stringsAsFactors = F)
    if(is.na(psjy$a4[1])){
      psjy$a7[1] <- NA
    }else if(psjy$a4[1]<0.05){
      psjy$a7[1] <- '拒绝零假设'
    }else if(psjy$a4[1]>=0.05){
      psjy$a7[1] <- '不能拒绝零假设'
    }else{
      psjy$a7[1] <- NA
    }
    }else if(length(a)>0&is.na(u0)){
      pp <- ks.test(x=a,y="ppois",lambda=mean(a),alternative = alter1)
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=side_type,a21=round(mean(a),2),a3=round(unname(pp$statistic),2),a4=round(pp$p.value,4),
                         a5=NA,a6='符合poisson分布',a7=NA,stringsAsFactors = F)
      if(is.na(psjy$a4[1])){
        psjy$a7[1] <- NA
      }else if(psjy$a4[1]<0.05){
        psjy$a7[1] <- '拒绝零假设'
      }else if(psjy$a4[1]>=0.05){
        psjy$a7[1] <- '不能拒绝零假设'
      }else{
        psjy$a7[1] <- NA
      }
    }else{
      psjy <- data.frame(a0='Poisson分布检验',a1='D',a2=side_type,a21=NA,a3=NA,a4=NA,
                         a5=NA,a6='符合poisson分布',a7='不能进poisson分布检验',stringsAsFactors = F) 
    }
})

    #3.1参数估计（正态分布法）
    ##前提条件：x满足正态分布的假设
    ##总体标准差sigma未知，采用t分布近似法
    ##总体标准差sigma已知，采用标准正态分布法估计
    withProgress({
      setProgress(message = "正在进行参数估计......")
    confn<-function(x,sigma=sigma0,alpha=alpha0,alter=side_type){
      options(digits = 3)
      n<-length(na.omit(x))
      xb<-mean(na.omit(x))
      if(!is.na(sigma)&alter=="双侧"){
        tmp<-sigma/sqrt(n)*qnorm(1-alpha/2)
        df<-n
        method='正态分布法'
      }else if(!is.na(sigma)&alter=="单侧"){
        tmp<-sigma/sqrt(n)*qnorm(1-alpha)
        df<-n
        method='正态分布法'
      }else if(is.na(sigma)&alter=="双侧"){
        tmp<-sd(na.omit(x))/sqrt(n)*qt(1-alpha/2,n-1)
        df<- n-1
        method='t分布法'
      }else{
        tmp<-sd(na.omit(x))/sqrt(n)*qt(1-alpha,n-1)
        df<- n-1
        method='t分布法'
      }
      u <- data.frame(n=n,mean=round(xb,2),sigma=sigma,df=df,alpha=alpha,alter=alter,confl=round(xb-tmp,3),confu=round(xb+tmp,3),method=method)
      return(u)
    }
    ##不满足正态分布时，采用百分位数估计
    confp<-function(x,sigma=sigma0,alpha=alpha0,alter=side_type,conf=conf){
      options(digits = 3)
      s3 <- psych::describe(x,quant=c(0.005,0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99,0.995),IQR=F)
      
      if(conf==0.95){
      if(alter=="双侧"){
        u <- data.frame(n=s3$n,mean=round(s3$mean,2),sigma=sigma,df=NA,alpha=alpha,alter=alter,confl=round(s3$Q0.025,3),confu=round(s3$Q0.975,3),method='百分位数法(p2.5-p97.5)')
      }else{
        u <- data.frame(n=s3$n,mean=round(s3$mean,2),sigma=sigma,df=NA,alpha=alpha,alter=alter,confl=round(s3$Q0.05,3),confu=round(s3$Q0.95,3),method='百分位数法(p5,p95)')
      }
      }else if(conf==0.9){
        if(alter=="双侧"){
          u <- data.frame(n=s3$n,mean=round(s3$mean,2),sigma=sigma,df=NA,alpha=alpha,alter=alter,confl=round(s3$Q0.05,3),confu=round(s3$Q0.95,3),method='百分位数法(p5-p95)')
        }else{
          u <- data.frame(n=s3$n,mean=round(s3$mean,2),sigma=sigma,df=NA,alpha=alpha,alter=alter,confl=round(s3$Q0.1,3),confu=round(s3$Q0.9,3),method='百分位数法(p10,p90)')
        }
      }else{
        if(alter=="双侧"){
          u <- data.frame(n=s3$n,mean=round(s3$mean,2),sigma=sigma,df=NA,alpha=alpha,alter=alter,confl=round(s3$Q0.005,3),confu=round(s3$Q0.995,3),method='百分位数法(p0.5-p99.5)')
        }else{
          u <- data.frame(n=s3$n,mean=round(s3$mean,2),sigma=sigma,df=NA,alpha=alpha,alter=alter,confl=round(s3$Q0.01,3),confu=round(s3$Q0.99,3),method='百分位数法(p1,p99)')
        }
      }
      
      
      return(u)
    }

    if(nrow(na.omit(data))<3){
      csgj <- confp(x=data[,1],sigma=sigma0,alpha = alpha0,alter=side_type,conf=conf)
     }else if(norm1$g5[which(norm1$g6=='推荐')]=='正态性'){
      csgj <- confn(x=data[,1],sigma=sigma0,alpha = alpha0 ,alter=side_type)
      }else{
      csgj <- confp(x=data[,1],sigma=sigma0,alpha = alpha0,alter=side_type,conf=conf) 
      }
    

    #满足正态分布时，绘制正态曲线，并增加控制线和警戒线
    #构建线条的数据框 a <-na.omit(data[,1])
    #t0 <- ifelse(conf==0.95,'95%',ifelse(conf==0.9,'90%','99%'))
    t0 <- paste0(conf*100,'%')
    datlines1 <- data.frame(type=c(-3,-2,-1,0,1,2,3),
                            x=c(mean(a)-3*sd(a),mean(a)-2*sd(a),csgj$confl,mean(a),csgj$confu,mean(a)+2*sd(a),mean(a)+3*sd(a)),
                            y=c(dnorm(mean(a)-3*sd(a),mean = mean(a), sd = sd(a)),dnorm(mean(a)-2*sd(a),mean = mean(a), sd = sd(a)),
                                dnorm(csgj$confl,mean = mean(a), sd = sd(a)),dnorm(mean(a),mean = mean(a), sd = sd(a)),
                                dnorm(csgj$confu,mean = mean(a), sd = sd(a)),dnorm(mean(a)+2*sd(a),mean = mean(a), sd = sd(a)),
                                dnorm(mean(a)+3*sd(a),mean = mean(a), sd = sd(a))),
                            txt=c('控制线','警戒线',paste0(t0,'CI下限'),'均值',paste0(t0,'CI上限'),'警戒线','控制线'))
    
    

    datlines2 <- datlines1
    datlines2$y <- 0
    datlines <- rbind(datlines1,datlines2)
    #构建标注文字的数据框
    datlines3 <- subset(datlines1,txt=='控制线')
    datlines4 <- subset(datlines1,txt=='警戒线')
    datlines5 <- subset(datlines1,type==-1)
    datlines6 <- subset(datlines1,type==1)
    datlines7 <- subset(datlines1,type==0)

    # #均数的95%可信区间
    # dnorm(csgj$confl,mean = mean(a), sd = sd(a))
    # dnorm(csgj$confu,mean = mean(a), sd = sd(a))
    # #警戒线（2个标准差）
    # dnorm(mean(a)-2*sd(a),mean = mean(a), sd = sd(a))
    # dnorm(mean(a)+2*sd(a),mean = mean(a), sd = sd(a))
    # #控制线（3个标准差）
    # dnorm(mean(a)-3*sd(a),mean = mean(a), sd = sd(a))
    # dnorm(mean(a)+3*sd(a),mean = mean(a), sd = sd(a))

    #准备投射可信区间阴影的数据框,默认取50个点
    if(nrow(na.omit(data))>0){
    x1 <- seq(csgj$confl,csgj$confu,by=(csgj$confu-csgj$confl)/50)
    y1 <- dnorm(x1,mean = mean(a), sd = sd(a))
    x1 <- c(csgj$confl,x1,csgj$confu)
    y1 <- c(0,y1,0)
    datpoly <- data.frame(x1=x1,y1=y1)
    }else{
      datpoly <- data.frame(x1=numeric(0),y1=numeric(0)) 
    }
    })
    ##解读的文字段落
    jjx <- round(datlines1[which(datlines1$txt=='警戒线'),2],2)
    kzx <- round(datlines1[which(datlines1$txt=='控制线'),2],2)
    if(nrow(na.omit(data))>=3){
    p1 <- paste0('1.',dmname,'数据中总频数为',b1,'，有效频数为',s2[1],'，占比',round(s2[1]/b1*100,1),'%，缺失频数为',s2[2]+s2[3],'，占比',round((s2[2]+s2[3])/b1*100,1),'%，',dat1$d5[2],'；')
    p2 <- paste0('2.均数和标准差为',b4,'±',b15,'，分布形态为',dat1$d23[2],'，且',dat1$d24[2],'；')
    p3 <- paste0('3.在非缺失值中，疑似异常值个数为',dat2$f2[2],'个，占比',dat2$f3[2],'%；\n（疑似异常值判断方法：百分位数间距法，疑似异常值判断标准：小于',round(point1,1),'或大于',round(point2,1),'）')
    p4 <- paste0('4.数据经',norm1$g1[which(norm1$g6=='推荐')],'，认为其分布为',norm1$g5[which(norm1$g6=='推荐')],'；')
    p5 <- paste0('5.单个离散型变量无法进行异常值检验！')
    if(sum(is.na(jsjy$a8))==2){
    p6 <- paste0('6.根据',dmname,'数据的实际情况，结合提供的总体参数，',qztj$a7,'，其零假设为\n',jsjy$a6[which(jsjy$a8=='推荐')],'，结果为',jsjy$a7[which(jsjy$a8=='推荐')],'；')
    }else{
      p6 <- paste0('6.根据',dmname,'数据的实际情况，结合提供的总体参数，',qztj$a7,'；') 
    }
    p8 <- paste0('8.根据',dmname,'数据的实际情况进行比例等同检验，其零假设为\n',bljy$a6[1],'，结果为',bljy$a7[1],'；')
    p9 <- paste0('9.根据',dmname,'数据的实际情况，结合提供的总体参数，其零假设为\n',psjy$a6[1],'，结果为',psjy$a7[1],'；')
    p7 <- paste0('7.采用',csgj$method,'，确定均数的',t0,'可信区间为[', csgj$confl,',',csgj$confu,']，警戒线区间为[',jjx[1],',',jjx[2],']，\n控制线区间为[',kzx[1],',',kzx[2],']；')
    }else if(nrow(na.omit(data))>0){
      p1 <- paste0('1.',dmname,'数据中总频数为',b1,'，有效频数为',s2[1],'，占比',round(s2[1]/b1*100,1),'%，缺失频数为',s2[2]+s2[3],'，占比',round((s2[2]+s2[3])/b1*100,1),'%，',dat1$d5[2],'；')
      p2 <- paste0('2.均数和标准差为',b4,'±',b15,',有效数据太少，无法评估其分布形态；')
      p3 <- paste0('3.在非缺失值中，疑似异常值个数为',dat2$f2[2],'个，占比',dat2$f3[2],'%；')
      p4 <- '4.有效数据太少，无法进行正态性检验；'
      p5 <- paste0('5.单个离散型变量无法进行异常值检验！')
      p6 <- '6.有效数据太少，无法与总体参数进行比较分析；'
      if(nrow(na.omit(data))>1){
      p8 <- paste0('8.根据',dmname,'数据的实际情况进行比例等同检验，其零假设为\n',bljy$a6[1],'，结果为',bljy$a7[1],'；')
      }else{
        p8 <-'8.有效数据太少，无法进行比例等同检验；' 
      }
      p9 <- paste0('9.根据',dmname,'数据的实际情况，结合提供的总体参数，其零假设为\n',psjy$a6[1],'，结果为',psjy$a7[1],'；')
      p7 <- paste0('7.采用',csgj$method,'，确定均数的',t0,'可信区间为[', csgj$confl,',',csgj$confu,']，警戒线区间为[',jjx[1],',',jjx[2],']，\n控制线区间为[',kzx[1],',',kzx[2],']；')
      
    }else{
     p1 <- '无数据'
     p2 <- ''
     p3 <- ''
     p4 <- ''
     p5 <- ''
     p6 <- ''
     p8 <- ''
     p9 <- ''
     p7 <- ''
    }
    z <- list(obj=obj,dmname=dmname,dmunit=dmunit,data=data,dat1=dat1,h1=h1,h2=h2,
              a=a,dat2=dat2,
              filterT=filterT,
              sketch2=sketch2,
              norm1=norm1,qztj=qztj,jsjy=jsjy,csgj=csgj,bljy=bljy,psjy=psjy,
              datlines1=datlines1,
              datlines=datlines,datlines3=datlines3,datlines4=datlines4,datlines5=datlines5,
              datlines6=datlines6,datlines7=datlines7,
              datpoly=datpoly,
              p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6,p7=p7,p8=p8,p9=p9)
    
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

#1.1基本统计表
output$table1 <- DT::renderDataTable({
  b <- z()
  dat1 <- b$dat1
  DT::datatable(dat1[1,],
                container = sketch1,
                rownames=FALSE,
                #extensions = 'FixedColumns',
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的统计描述表'))
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
                  dom = "t"
                  #scrollX = TRUE
                  #fixedColumns = list(leftColumns = 1, rightColumns = 2)
                  ))%>%
    #第1列的字体颜色，粗细，背景颜色
    formatStyle(1, fontWeight = 'bold')
})

#1.2 频率分布表
output$table2 <- DT::renderDataTable({
  b <- z()
  h2 <- b$h2
  names(h2) <- c('组距','频数','频率(%)','频率(%)*','累计频数','累计频数*','累计频率(%)','累计频率(%)*')
  DT::datatable(h2,
                #container = sketch1,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的频率分布表'))
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
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的异常值分布表'))
                ),
                options = list(
                  ordering= FALSE,
                  columnDefs = list(list(className = 'dt-left', targets = "_all")),
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

#2.1正态分布检验表
output$table4 <- DT::renderDataTable({
  b <- z()
  norm1 <- b$norm1
  names(norm1) <- c('检验方法','统计量','统计值','P值','意义','是否推荐')
  DT::datatable(norm1,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的正态分布检验表'))
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
    formatStyle(1, fontWeight = 'bold')%>%
    formatStyle(6, fontWeight = styleEqual('推荐',  'bold'),
                   color = styleEqual('推荐',  'green'))%>%
    formatStyle(6,target = 'row',
              backgroundColor = styleEqual('推荐',  '#FFFF99'))
})
##2.1QQ图
output$plot1 <- renderPlot({
  b <- z()
  data <- b$data
  if (nrow(na.omit(data))>=3) {
    show("plot1")
  } else {
    hide("plot1")
  }
  if (nrow(na.omit(data))>0) {
  qqPlot(data[,1],distribution = 'norm',
         main=paste0(b$obj,b$dmname,'（',b$dmunit,'）的QQ图'),
         xlab='正态分布分位数',ylab='实际数据分位数',id.col='grey',
         col='#0066FF',col.lines='#9900CC',grid=F,cex=0.9)
  }else{
    return(NULL)
  }

})

#2.2异常值检验
output$plot2 <- renderPlot({
  b <- z()
  data <- b$data
  if (nrow(na.omit(data))>=3) {
    show("plot2")
  } else {
    hide("plot2")
  }
  if (nrow(na.omit(data))>=2) {
  Boxplot(data[,1],labels=round(data[,1],2),ylab=paste0(b$dmname,'（',b$dmunit,'）'),
            main=paste0(b$obj,b$dmname,'（',b$dmunit,'）的离群箱式图'))
  }else{
    return(NULL)
  }
})
output$text21 <- renderUI({
 h4("单个离散型定量变量无法进行异常值检验！",style="color:red;font-weight:bold;",align='center')
})

#2.3与总体参数比较
output$table5 <- DT::renderDataTable({
  b <- z()
  qztj <- b$qztj
  names(qztj) <- c('有效样本量','总体均数','总体中位数','总体标准差','置信水平','单双侧','是否正态','解读')
  DT::datatable(qztj,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的假设检验前置条件一览表'))
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
output$table6 <- DT::renderDataTable({
  b <- z()
  jsjy <- b$jsjy
  jsjy <- jsjy[order(jsjy$a8,decreasing = T),]
  names(jsjy) <- c('检验方法','统计量','单双侧','统计值','P值','自由度','零假设','解读','是否推荐')
  DT::datatable(jsjy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的假设检验统计指标表'))
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
    formatStyle(9, fontWeight = styleEqual('推荐',  'bold'),
                color = styleEqual('推荐',  'green'))%>%
    formatStyle(9,target = 'row',
                backgroundColor = styleEqual('推荐',  '#FFFF99'))
})

#2.4 比例检验
output$table8 <- DT::renderDataTable({
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
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的比例检验表'))
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

#2.5 poisson分布检验
output$table9 <- DT::renderDataTable({
  b <- z()
  psjy <- b$psjy
  names(psjy) <- c('检验方法','统计量','单双侧','总体均数','统计值','P值','自由度','零假设','解读')
  DT::datatable(psjy,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的Poisson分布检验表'))
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

#3.1参数估计（正态分布法）
output$table7 <- DT::renderDataTable({
  b <- z()
  csgj <- b$csgj
  names(csgj) <- c('有效样本含量','样本均数','总体标准差','自由度','显著水平','单双侧','CI下限','CI上限','方法')
  DT::datatable(csgj,
                #container = sketch2,
                #escape = FALSE,
                rownames=FALSE,
                #class = 'compact hover row-border order-column stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(paste0(b$obj,b$dmname,'（',b$dmunit,'）的参数估计表'))
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
output$plot3 <- renderPlot({
  b <- z()
  data <- b$data
  h1 <- b$h1
  a <- b$a
  csgj <- b$csgj
  datpoly <- b$datpoly
  datlines <- b$datlines
  datlines3 <- b$datlines3
  datlines4 <- b$datlines4
  datlines5 <- b$datlines5
  datlines6 <- b$datlines6
  datlines7 <- b$datlines7
  #在使用shinyjs的基础上，隐藏空白图片的输出
  if (nrow(na.omit(data))>=3) {
    show("plot3")
  } else {
    hide("plot3")
  }
  
  if(nrow(na.omit(data))>=3){
  #可以通过直方图的组距来确定X轴的范围和刻度，左右各增加4倍组距
    xlim1 <- h1$breaks[1]-4*(h1$breaks[2]-h1$breaks[1])#x轴起点
    xlim2 <- max(h1$breaks)+4*(h1$breaks[2]-h1$breaks[1])#x轴终点
  #可以通过正态曲线的顶点和0之间除以0.01确定Y轴的范围和刻度
    ylimt <- max(h1$density)+0.01#这里根据实际效果进行调整
  hist(data[,1],freq=FALSE,xlim = c(xlim1,xlim2),ylim = c(0,1.1*ylimt),axes = F,xlab='',ylab='',main='')
  #单独建立坐标轴是为了保证图形和X轴之间没有空隙
  xsq <- seq(xlim1,xlim2,by=(h1$breaks[2]-h1$breaks[1]))
  xsq <- xsq[xsq%%10==0]
  axis(1,at=xsq,pos=0,tck=0)#x轴
  axis(2,at=seq(0,ylimt*1.1,by=0.01),pos=xlim1,las=2)#y轴
  title(main=paste0(b$obj,b$dmname,'（',b$dmunit,'）的参数估计图(有效样本量=',csgj$n,')'),xlab = paste0(b$dmname,'（',b$dmunit,'）'),ylab="频率密度")
  curve(dnorm(x, mean = mean(a), sd = sd(a)), from=xlim1, to=xlim2, add = TRUE, col = "darkgreen", lwd = 1)
  polygon(x=datpoly$x1, y = datpoly$y1, density = NULL, angle = 45,lty=3,border = NULL, col = '#99FFFF40')#透明度颜色
  curve(dnorm(x, mean = mean(a), sd = sd(a)), from=xlim1, to=xlim2, add = TRUE, col = "darkgreen", lwd = 1)
  lines(datlines$x[which(datlines$type==-3)],datlines$y[which(datlines$type==-3)],type='l',col='red',lwd=2)
  lines(datlines$x[which(datlines$type==3)],datlines$y[which(datlines$type==3)],type='l',col='red',lwd=2)
  lines(datlines$x[which(datlines$type==-2)],datlines$y[which(datlines$type==-2)],type='l',col='blue',lwd=2)
  lines(datlines$x[which(datlines$type==2)],datlines$y[which(datlines$type==2)],type='l',col='blue',lwd=2)
  lines(datlines$x[which(datlines$type==-1)],datlines$y[which(datlines$type==-1)],type='l',col='darkgreen',lwd=2)
  lines(datlines$x[which(datlines$type==1)],datlines$y[which(datlines$type==1)],type='l',col='darkgreen',lwd=2)
  lines(datlines$x[which(datlines$type==0)],datlines$y[which(datlines$type==0)],type='l',col='gold',lwd=2)
  text(datlines3$x,datlines3$y,paste0(datlines3$txt,'(',round(datlines3$x,1),')'),cex=0.7,pos=3,col='red')
  text(datlines4$x,datlines4$y,paste0(datlines4$txt,'(',round(datlines4$x,1),')'),cex=0.7,pos=3,col='blue')
  text(datlines5$x,datlines5$y,paste0(datlines5$txt,'(',round(datlines5$x,1),')'),cex=0.7,pos=2)
  text(datlines6$x,datlines6$y,paste0(datlines6$txt,'(',round(datlines6$x,1),')'),cex=0.7,pos=4)
  text(datlines7$x,datlines7$y,paste0(datlines7$txt,'(',round(datlines7$x,1),')'),cex=0.7,pos=3)
  }else{
    return(NULL)
  }
  #abline不适用，会穿透正态曲线
  # abline(v = c(csgj$confl,csgj$confu), col = "blue")
  # abline(v = c(mean(a)-2*sd(a),mean(a)+2*sd(a)), col = "yellow")
  # abline(v = c(mean(a)-3*sd(a),mean(a)+3*sd(a)), col = "red")
  # abline(a=10,b=2,col="red")
})

#总体解读
output$text1 <- renderPrint({
  b <- z()
  #print(b$filterT)
  #print(urlkey)
  cat(b$p1,b$p2,b$p3,b$p4,b$p5,b$p6,b$p7,b$p8,b$p9,sep='\n\n')
  

})

})
