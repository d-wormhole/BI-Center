
# 加载子模块服务器逻辑
source("./server/stocks_analys_server.R", local = TRUE)
source("./server/signin_server.R", local = TRUE)
source("./server/stocks_trend_server.R", local = TRUE)

# 定义服务器逻辑
main_server <- function(input, output, session) {
  #调用“登录服务模块(signinServer)"，获取登录成功后的信息/这是R1.5.0版本后的模块modules调用方法参考网址：https://shiny.posit.co/r/articles/improve/modules
  #括号内的“signin”要与main_ui内的“signin_UI”指定的“id”一致，这样才能将相应的动态UI内容(输入框、表格、弹窗等等）输出到对应的outputUI区域
  erp_response <- signinServer("signin")
  #调用"库存分析模块(stocks_analysServer)",并使用“登录模”块返回的登录信息渲染对应的UI页面;前面”res”，若服务模块不需要返回任何值，可以不写；也可以执行
  #注意erp_response参数，若模块需要使用其他模块返回的值，需要在“当前模块(stocks_analysServer)”的函数参数中设定一个自定义参数，并将其默认值设置为主服务调用其他服务获取的值，在这个例子中用到的是“登录服务”
  res<-stocks_analysServer("stocks_analys",erp_response=erp_response)
  #调用“库存趋势模块（stocks_trendServer),并使用“登录模”块返回的登录信息渲染对应的UI页面;前面”res”，若服务模块不需要返回任何值，可以不写；也可以执行
   res<-stocks_trendServer("stocks_trend",erp_response=erp_response)
}