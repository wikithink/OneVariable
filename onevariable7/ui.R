#单个日期时间变量的规律分析
#王文祥
#2018-1-5
#ui.R
library(RMySQL)
library(shiny)
library(DT)
library(xts)
library(plyr)
library(dplyr)
library(car)
library(shinyjs)
library(Hmisc)
library(pastecs)
library(psych)
library(BSDA)
library(jsonlite)
library(data.table)
library(pracma)
library(trend)
library(tseries)
library(forecast)
library(SimilarityMeasures)
library(dygraphs)
library(proto)
#library(RCurl)
#加载老朱的js代码，用于隐藏左侧菜单栏，并使用url加#1-n来选择不同的菜单
jspath <- "http://192.168.1.35:8083/dmcharts-a1/js/shiny/tabclick.js"

shinyUI(fluidPage(useShinyjs(),
                  tags$head(
                    #加载css文件
                    includeCSS("styles.css"),
                    includeScript(path=jspath),
                    #在标签页显示小图标
                    HTML('<link rel="shortcut icon" type="image/x-icon" href="/report_demo1/linksu.ico" media="screen" />')),
                  #网页标签页标题名称
                  title="单个日期时间变量的规律分析",
                  # h3('单个连续型变量分析示例',align='center'),
                  # fluidRow(
                  #   column(width=3,offset=4,h5("数据来源：XX市尘肺病监测数据",style="color:grey")),
                  #   column(width=2,offset=0,h5("发布日期：2017-11-1",style="color:grey"))
                  # ),
                  # #hr(),
                  # tabsetPanel(id = "tabs",type='tabs',
                  #             tabPanel("一般描述",br(),DT::dataTableOutput('table1', width = "100%")),
                  #             tabPanel("统计图",br(),plotOutput('plot1', width = "80%",height = "500px")),
                  #             #tabPanel("假设检验",br(),DT::dataTableOutput('table2', width = "90%")),
                  #             tabPanel("参数估计",br(),verbatimTextOutput("text1"))
                  # )
                  navlistPanel(widths=c(2,10),
                               # "一般描述",
                               # tabPanel("可视化"),
                               # tabPanel("数据表"),
                               "统计描述",
                               tabPanel("频率分布表",
                                        tableOutput('filter1'),
                                        DT::dataTableOutput('table1', width = "90%")),
                               tabPanel("疑似异常分布表",
                                        tableOutput('filter2'),
                                        DT::dataTableOutput('table2', width = "80%")),
                               "假设检验",
                               tabPanel("时间趋势",
                                        tableOutput('filter4'),
                                        dygraphOutput("plot1",width = "80%", height = "500px"),
                                        br(),
                                        DT::dataTableOutput('table4', width = "90%"),br()),

                               "参数估计",
                               tabPanel("季节性分解",
                                        tableOutput('filter3'),
                                        plotOutput('plot2', width = "90%",height = "500px"),
                                        DT::dataTableOutput('table3', width = "90%"),br()),
                               tabPanel("总体斜率",
                                        tableOutput('filter5'),
                                        DT::dataTableOutput('table5', width = "95%"),br()),
                               tabPanel("曲线相似度",
                                        tableOutput('filter6'),
                                        plotOutput('plot3', width = "90%",height = "500px"),
                                        br(),
                                        DT::dataTableOutput('table6', width = "100%"),br()
                                        ),
                               "——————",
                               tabPanel("总体解读",
                                        tableOutput('filter7'),
                                        h3("总体解读",style="color:#337ab7",align="center"),
                                        hr(),
                                        verbatimTextOutput("text1"))
                  )
))





