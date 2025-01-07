# 定义服务器逻辑
signinServer <- function(id) {
  moduleServer(
  id,
   function(input, output, session){
    #登录区域：未登录时，显示“登录”按钮；已登录时“显示已登录的账号及重新登录”
       output$signin<-renderUI({
        #模块化的UI或renderUI都需要使用ns<-NS(id)开始
        ns <- NS(id)
        #获取登录信息
        userinfo<-erp_response1()
        if(is.null(userinfo$error)==FALSE) {
          #注意：使用ifelse条件动态输UI时，有多个内容要输出，需要使用list()
          list(
             tags$p(paste(c(userinfo$data$company_name,userinfo$data$name),collapse = " "),style="display: inline-block;font-size: 20px;"),
             actionButton(ns("sigin"),"登录",style="height:40px;width:120px;")
            )}
        else{
          list(
          actionButton(ns("sigin"),"登录",style="height:40px;width:120px;")
        )}
       })
       #点击“登录按钮”弹窗登录对话框
      observeEvent(input$sigin, {
          #模块化的UI输入或renderUI都需要使用ns<-NS(id)开始
          ns <- NS(id)
          # 显示对话框的逻辑
          showModal(modalDialog(
          title = "登录",
          visible = FALSE, # 初始化时不显示对话框
          # 模态对话框中的UI
          tags$div(
            textInput(ns("account"), "员工账号:", placeholder = "输入员工账号"),
            textInput(ns("comapny_code"), "企业编码:", placeholder = "输入企业编码"),
            passwordInput(ns("password"), "登录密码:", placeholder = "输入登录密码"),
            actionButton(ns("login"), "登录")
          ),
          size = "m", # 对话框大小
          easyClose = TRUE # 点击外部或关闭按钮时关闭对话框
        )
      )
        })
      #获取用户输入的账号密码，并判断是否登录成功
      erp_response1<-reactiveVal(list()) #初始化"登录信息“动态变量,再根据登录后的信息更新该变量
      observeEvent(input$login, {
          # 获取用户名和密码输入
          account <- input$account
          comapny_code <- input$comapny_code
          password <- input$password
          url <- "https://api.erp.sizehorse.com/account/login" #登录接口
          #将用户输入的登录信息转为post请求的form-data参数列表
          formData <- list(
            account = account,
            code = comapny_code,
            password = password,
            rememmber =1 
          )
          erp_response<-POST(url,body = formData, encode = "form") #登录请求
          erp_response<-content(erp_response) #登录请求返回的信息
          # 如果登录成功，返回提示信息，并关闭弹窗
          if (erp_response$error==0) {
            erp_response1(erp_response) #如果登录成功，调用动态变量函数，更新登录信息
            # 关闭模态对话框
            removeModal()
            showNotification("登录成功!",duration = 3,type="message")
          } 
          # 如果登录失败，返回提示信息
          else {
            showNotification("账号密码有误，请重新输入!",duration = 3,type="error")
          }
        }) 
      return(list(erp_response1=erp_response1))
  }
)
}

