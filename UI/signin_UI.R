# 定义模块1的UI
signinUI <- function(id) {
  #模块化的UI或renderUI都需要使用ns<-NS(id)开始
  ns <- NS(id)
  tagList(
   # fluidPage(
   #  #页面css样式
   #      tags$head(
   #        tags$style(type = "text/css", 
   #          " .shiny-notification{
   #            position: fixed;
   #            top: 5%;
   #            left: 50%;
   #            transform: translate(-50%, -50%);
   #            z-index: 9999;
   #            }
   #          .signin {
   #            position: absolute; 
   #            z-index: 9999; 
   #            top: 25px; 
   #            right:30px;
   #          }
   #          .navbar {
   #            height:10px;
   #            border-radius: 8px;
   #            background-color: #f5f5f5; 
   #            color: #343a40;
   #          }
   #           .navbar.navbar-nav > li > a {
   #            color: #000000; 
   #            }
   #         .signin{
   #            height:4vh;
   #            background-color: #f5f5f5;
   #          }
   #          .main{
   #            height:92vh;
   #            background-color: #f5f5f5;
   #          } 
   #          .sidebar-panel {
   #            margin-top:10px;
   #            background-color: #fff8f0; 
   #            height: 88vh;
   #            width:390%;
   #            overflow-y: auto;
   #          }
   #           .main-panel {
   #            margin-top:10px;
   #            height: 85vh;
   #            width:100%;
   #            overflow-y: auto;
   #            background-color: #ffffff
   #          } 
   #          "
   #          )
fluidPage(
tags$div(class="signin",style="width:600px;height:0px;display:flex;justify-content: flex-end;align-items:center;",
    #与非模块化的shinyappUI(各种输入或输出组件),所有模块内的UI，id命名必须用“ns("id")格式
    #设定登录区域的动态UI输出，具体输出样式件“登录服务”
    uiOutput(ns("signin"))
  )
 )
)
}