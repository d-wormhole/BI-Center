# 定义模块1的UI
sales_analysUI <- function(id,state) {
  ns <- NS(id)
  tagList(
   fluidPage(
    # #页面css样式
    #     tags$head(
    #       tags$style(type = "text/css", 
    #         " .shiny-notification{
    #           position: fixed;
    #           top: 5%;
    #           left: 50%;
    #           transform: translate(-50%, -50%);
    #           z-index: 9999;
    #           }
    #         .signin {
    #           position: absolute; 
    #           z-index: 9999; 
    #           top: 25px; 
    #           right:30px;
    #         }
    #         .navbar {
    #           height:10px;
    #           border-radius: 8px;
    #           background-color: #f5f5f5; 
    #           color: #343a40;
    #         }
    #          .navbar.navbar-nav > li > a {
    #           color: #000000; 
    #           }
    #        .signin{
    #           height:4vh;
    #           background-color: #f5f5f5;
    #         }
    #         .main{
    #           height:92vh;
    #           background-color: #f5f5f5;
    #         } 
    #         .sidebar-panel {
    #           margin-top:10px;
    #           background-color: #fff8f0; 
    #           height: 88vh;
    #           width:390%;
    #           overflow-y: auto;
    #         }
    #          .main-panel {
    #           margin-top:10px;
    #           height: 85vh;
    #           width:100%;
    #           overflow-y: auto;
    #           background-color: #ffffff
    #         } 
    #         "
    #         )
    #     ),
    #     fluidPage(
    #       # tags$div(
    #       # # tag$p("即将上线")
    #       # )
      )
    )
}