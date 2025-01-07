
# 加载全局变量和函数
source("global.R")
#加载需要的包
library(shiny)#创建shiny应用
library(shinydashboard)#创建导航面板
library(httr) #发送http请求
library(jsonlite) #读取json结构数据
library(openxlsx) #导出excel
library(readxl) #读取excel
library(plyr) #基础加强包
library(dplyr) #基础加强包
library(ggplot2) #绘图包
library(plotly) #动态交互包
library(treemapify)#treemap矩形树图
library(showtext)#汉语字体


# 加载UI和服务器逻辑
source("./UI/main_ui.R", local = TRUE)
source("./server/main_server.R", local = TRUE)

# 运行应用
shinyApp(main_ui, main_server)