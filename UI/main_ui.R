
# 加载模块UI
source("./UI/stocks_analys_UI.R", local = TRUE)
source("./UI/signin_UI.R", local = TRUE)
source("./UI/stocks_trend_UI.R", local = TRUE)
# 定义主UI
#页面css样式,每个页面的样式要在写对应的服务模块下，不要重复，否则会主服务其中加载的服务顺序，用后加载的被覆盖前面的
main_ui <-fluidPage(
  fluidPage(
    #页面css样式
        tags$head(
          tags$style(HTML("
            /* 让内容填满整个宽度 */
            .content {
              padding: 0 !important;
              margin: 0 !important;
            }
            .container-fluid {
              padding-left: 0px !important;
              padding-right: 0px !important;
            }
      ")),
          tags$style(type = "text/css", 
            " .shiny-notification{
              position: fixed;
              top: 5%;
              left: 50%;
              transform: translate(-50%, -50%);
              z-index: 9999;
              }
            .signin {
              position: absolute; 
              z-index: 9999; 
              top: 25px; 
              right:30px;
            }
            "
            )
        )),
fluidPage(
  fluidRow(
    dashboardPage(
    dashboardHeader(title = "BI中心"),
    dashboardSidebar(
    sidebarMenu(
      menuItem("首页", tabName = "home", icon = icon("home")),
      menuItem("库存分析", tabName = "stocks_analys", icon = icon("dashboard")),
      menuItem("销售分析", tabName = "sales_ananlys", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        h2("首页内容")
      ),
      tabItem(
        tabName = "stocks_analys",
        p("库存分析"),
        # 子导航栏
        tabsetPanel(
          tabPanel("属性汇总", value = "sub1", stocks_analysUI("stocks_analys")),
          tabPanel("按款汇总", value = "sub2", h3("即将上线")),
          tabPanel("库存趋势", value = "sub3", stocks_trendUI("stocks_trend"))
        )
      ),
      tabItem(
        tabName = "sales_ananlys",
        p("销售分析")
      )
    )
  )
)
)),
fluidPage(
    #调用“登录UI模块(sales_analysUI)”,将对应的UI组件渲染页面，注意括号内的“id命名”，要与对应服务模块调用时id一致
    signinUI("signin")
)
)