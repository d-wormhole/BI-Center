# 定义模块1的UI
stocks_analysUI <- function(id,state) {
  #模块化的UI或renderUI都需要使用ns<-NS(id)开始
  ns <- NS(id)
  tagList(
   fluidPage(  
    fluidPage(
    #页面css样式,每个页面的样式要在写对应的服务模块下，不要重复，否则会主服务其中加载的服务顺序，用后加载的被覆盖前面的
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
                        uiOutput(ns("storages")),
                        tags$p("选择统计属性:"),
                        uiOutput(ns("secondinput"))
                        )),
                  column(10,offset=0,
                    mainPanel(
                        # 设置“属性分析”的动态UI区域，具体样式见“库存分析模块(stocs_analysServer）”的属性选择动态UI部分
                        #与非模块化的shinyappUI(各种输入或输出组件),所有模块内的UI，id命名必须用“ns("id")格式
                        uiOutput(ns("shuxing"))
                        ))
                      )
                    )
                  )
                )
      )
    )
}