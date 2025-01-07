# 定义模块1的UI
stocks_trendUI <- function(id,state) {
  #模块化的UI或renderUI都需要使用ns<-NS(id)开始
  ns <- NS(id)
  tagList(
   fluidPage(  
    fluidPage(
    #页面css样式,每个页面的样式要在写对应的服务模块下，否则会主服务其中加载的服务顺序，用后加载的被覆盖前面的
        tags$head(
          tags$style(type = "text/css", 
            ".main{
              height:88vh;
              width:100%;
              left:0vh;
              background-color: ;
            } 
            .sidebar-panel {
              margin-top:10px;
              background-color: ; 
              height: 85vh;
              width:390%;
              overflow-y: auto;
            }
             .main-panel {
              margin-top:10px;
              height: 80vh;
              width:150%;
              overflow-y: auto;
              background-color: ;
            } 
            .card {
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
            transition: 0.3s;
            width:80%;
            margin-left:10%;
            border-radius: 5px;
            padding: 16px;
            background-color: ;
            margin-top:10px;
           }
           .card:hover {
            box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
           }
          .card.selected {
           border: 2px solid #007bff;
           background-color: #e9f5ff;
           }
           .card-title {
            font-size: 24px;
            font-weight: bold;
            margin-left: 10px;
           }
           .card-content {
            margin-top: 10px;
            font-size: 16px;
            }
           .card-image {
            width: 10%;
            border-radius: 5px 5px 0 0;
            }
                  "
            )
        )),
        fluidPage(
          fluidRow(
            column(12,class = "main",
              fluidRow(
                 column(2,offset=0,
                    sidebarPanel(class = "sidebar-panel",
                        #设置“仓库选择”的动态UI区域，具体样式见“库存分析模块(stocs_analysServer）”的仓库选择动态UI部分
                        #与非模块化的shinyappUI(各种输入或输出组件),所有模块内的UI，id命名必须用“ns("id")格式
                        uiOutput(ns("style_select"))
                        )),
                  column(10,offset=0,
                    mainPanel(
                        # 设置“属性分析”的动态UI区域，具体样式见“库存分析模块(stocs_analysServer）”的属性选择动态UI部分
                        #与非模块化的shinyappUI(各种输入或输出组件),所有模块内的UI，id命名必须用“ns("id")格式
                        uiOutput(ns("stocks_trend_plot"))
                        ))
                      )
                    )
                  )
                )
      )
    )
}