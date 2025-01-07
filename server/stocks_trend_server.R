# # 定义服务器逻辑
# #注意该模块在主服务（main_server)运行时，需要依赖”注册服务“的返回注册信息，因此需要在外部function需要设定一个额外的参数，即：erp_response；
# #类似地，如果模块有其他数据要引入，也需要设定参数，并且在主服务内设定参数的初始值；初始值可以直接赋值，或者调用其他模块的返回值
stocks_trendServer <- function(id,erp_response) {
  moduleServer(
  id,
   function(input, output, session){
    #引用”注册服务"模块的返回值，需要设定一个reactive变量；注意reactive内部获取返回值的赋值格式“userinfo<-erp_response$erp_response1()”，这个很重要
    #若只返回一个变量，return可写可不写，如果是有多个，必须要指定返回的值；同时返回多个值的话，return内部要使用list
    userinfo<-reactive({
    userinfo<-erp_response$erp_response1()
      return(userinfo)
    })
    #UI-筛选区域
    output$style_select <-renderUI({
    #模块化的UI或renderUI都需要使用ns<-NS(id)开始
      ns <- NS(id)
      #如果没有登录，显示为请登录
      userinfo<-userinfo()
              if(is.null(userinfo$error)){
                tags$h3("请登录!")
              }
              else{
                #输出多个UI内容时，需要使用list
                list(
                  fluidPage(
                 # 使用 flexbox 布局使内容垂直居中
                  fluidRow(
                    tags$div(
                    style = "display: flex; align-items: center;",
                    column(9,offset=0,
                            textInput(ns("style"),"",placeholder="输入款号/名称")
                        ),
                    column(3,offset=0,style = "left:-20px;",
                            actionButton(ns("style_submit"),"查询")
                        )
                    )
                  )                 
                )
              )
            }
    })
    # 获取输入的款号信息
    # 1-点击提交后，使用登录的token查询输入的款号是否存在，并渲染款号选择弹窗
    selected_product<-reactiveVal(list()) #初始化“已选款号动态变量
    products1<-reactiveVal(list())
    observeEvent(input$style_submit, {
    #observeEvent内部可以包含renderUI，但无需再重新定义reactive
            userinfo<-userinfo()
            if(userinfo$error==0){
                token<-userinfo$data$token
                input_style<-input$style
                #若果是向量和字符串之间的拼接，间隔符号用sep；如果是向量内部的凭借使用collapse，具体区别可查看官方文档
                url<-paste(sizehorse_erp_api()$drp,"/search_product",sep = "")
                #配置get请求的hearder参数
                headers <- c(
                `token` = token,
                `Content-Type` = "application/x-www-form-urlencoded",
                `Accept` = "application/json"
                )
                #配置get请求的param参数
                params<-list(
                 key=input_style,
                 page=1,
                 page_size=20
                )
              product_response<-GET(url, add_headers(.headers = headers),query = params,progress(), httr::config(max_recv_speed_large = 10000))
              products<-content(product_response,type="application/json")
              products1(products)
              if(length(products$data$data)==0){
                showNotification("未检索到商品，请重新输入!",duration = 3,type="error")
              }
              else{
                ns <- NS(id)
                #先根据返回的数据，使用lapply循环，生成卡片UI列表
                card_list <- lapply(seq_along(products$data$data), function(i) {
                  tags$div(class = "card",style = "display:flex;align-items:center;",
                    tags$img(class = "card-image",src = paste(sizehorse_erp_api()$img_url,products$data$data[[i]]$default_img_url,sizehorse_erp_api()$img_size,sep="")),
                    tags$div(class = "card-title", 
                         tags$div(paste("款号:",products$data$data[[i]]$style_no)),
                         tags$div(class = "card-content",paste("名称:",products$data$data[[i]]$name))
                         ),
                    actionButton(ns(paste0("select_", i)), "选择",style = "margin-left:10%;height:36px;width:80px;")
                  )
                })
                # 显示对话框的逻辑
                showModal(modalDialog(
                    title = "请选择商品",
                    size = "m", # 对话框大小
                    easyClose = FALSE, # 点击外部或关闭按钮时关闭对话框
                    footer = tagList(
                    modalButton("关闭")),
                    visible = TRUE, # 初始化时不显示对话框
                    # 弹窗内回调taglist布局函数，渲染卡片:tagList 是一个特殊的布局函数，允许您将多个 UI 组件组合在一起，
                    #而无需指定它们的布局方式。这对于动态生成 UI 元素非常有用
                    tags$div(style = "max-height: 600px; overflow-y: auto;",
                    do.call(tagList, card_list))
                  )
                )
              }
            }
            else{
              next
            }
       })
  # 为每个选择按钮添加事件监听器,并根据选择的款号更新“selected_product动态变量
  #使用 reactiveValues 来存储执行时间，记录每个reactive的执行时间
 execution_times <- reactiveValues(stocks_changes_reactive_time = NULL,  stocks_trend_data_reactive_time = NULL,outPlot = NULL,selected_product_time=NULL)
  # 注意不要嵌套在另外一个observeEvent内部，容易导致出现弹窗会被自动触发关闭
  observe({
    lapply(seq_along(products1()$data$data), function(i) {
      observeEvent(input[[paste0("select_", i)]], {
      selected_product1<-products1()$data$data[[i]]
      #根据选择的款号更新“selected_product动态变量
      selected_product(selected_product1)
      #关闭弹窗
      removeModal()
      })
    })
  })
  #根据已选择的款号获取引起库存变动的单据信息：出库、入库、差异单原始数据
  # 初始化一个触发器
  dataUpdateTrigger <- reactiveVal(NULL)
  # 每次点击刷新按钮时更新触发器
  observeEvent(input$refresh, {
  dataUpdateTrigger(Sys.time())  # 使用当前时间戳作为触发器，强制重新评估 getUpdatedData()
  })
  #2-登录成功后，根据选择款号获取出库、入库、差异单原始数据
  stocks_changes<-reactive({
      # 每次触发时都会重新获取数据
      dataUpdateTrigger()
      start_time <- Sys.time()
      # 获取登录信息
      userinfo<-userinfo()
      if(userinfo$error==0){
      showModal(modalDialog(
      title = "请稍候",
      "正在处理请求...",
      footer = NULL,
      easyClose = FALSE
      ))
      token<-userinfo$data$token
      selected_product<-selected_product()
      style_no<-selected_product$style_no
      url<-paste(sizehorse_erp_api()$drp,"/report",sep = "")
      #配置get请求的hearder参数
      headers <- c(
      `token` = token,
      `Content-Type` = "application/x-www-form-urlencoded",
      `Accept` = "application/json"
      )
      #配置get请求的param参数
      stockin_params<-list(
        m="Docs_Stockin",
        f="list",
        page=1,
        page_size= 9999,
        status=1,
        begin_billdate= "",
        end_billdate= "",
        sortby="quantity",
        order="asc",
        storage_id="",
        origin_type="", 
        style_no= style_no,
        distributor_id="",
        attribute_units="",
        receiver_category="",
        receiver_type="",
        receiver_id="", 
        deliver_type="", 
        deliver_id="", 
        origin_order_type="",
        tag="",
        filter=""
      )
      stockout_params<-list(
        m="Docs_Stockout",
        f="list",
        page=1,
        page_size= 9999,
        status=1,
        begin_billdate= "",
        end_billdate= "",
        sortby="quantity",
        order="asc",
        storage_id="",
        origin_type="", 
        style_no= style_no,
        distributor_id="",
        attribute_units="",
        receiver_category="",
        receiver_type="",
        receiver_id="", 
        deliver_type="", 
        deliver_id="", 
        origin_order_type="",
        tag="",
        filter=""
      )
      stockdifference_params<-list(
        m="Custom_Docs",
        f="stock_list",
        page=1,
        page_size= 9999,
        status=1,
        begin_billdate= "",
        end_billdate= "",
        sortby="quantity",
        order="asc",
        storage_id="",
        origin_type="", 
        style_no= style_no,
        temp_id=20,
        has_keys=1,
        continue_sortby_list=1,
        filter=""
      )
    stockin_response<-GET(url, add_headers(.headers = headers),query = stockin_params,progress(), httr::config(max_recv_speed_large = 10000000))
    stockout_response<-GET(url, add_headers(.headers = headers),query = stockout_params,progress(), httr::config(max_recv_speed_large = 1000000))
    stockdifference_response<-GET(url, add_headers(.headers = headers),query = stockdifference_params,progress(), httr::config(max_recv_speed_large = 1000000))
    stockin_list<-content(stockin_response,type="application/json")
    stockout_list<-content(stockout_response,type="application/json")
    stockdifference_list<-content(stockdifference_response,type="application/json")
    removeModal()
  }
  else{
    next
  }
  #记录reactive执行时间的语句要写在return之前
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  # 保存执行时间到reactiveValues
  execution_times$stocks_changes_reactive_time <- time_taken
  #返回获取的入库、出库、差异明细
   return(list(
    stockin_lists=stockin_list,
    stockout_lists=stockout_list,
    stockdifference_lists=stockdifference_list
    ))
  })
  #对返回的库存变动原始数据处理
  stocks_trend_data<-reactive({
    stockin_list<-stocks_changes()$stockin_lists$data$list
    stockout_list<-stocks_changes()$stockout_lists$data$list
    stockdifference_list<-stocks_changes()$stockdifference_lists$data$list
    #取原始数据需要的值，添加至数据框
    #初始化数据框并指定表头类型
    stocksin_change<-data.frame(单据编号=character(),单据类型=character(),源单类型=character(),仓库名称=character(),条码=character(),款号=character(),名称=character(),色号=character(),颜色=character(),
                                尺码号=character(),尺码=character(),单据日期=as.Date(character()),数量=numeric(),stringsAsFactors = FALSE)
    stocksout_change<-data.frame(单据编号=character(),单据类型=character(),源单类型=character(),仓库名称=character(),条码=character(),款号=character(),名称=character(),色号=character(),颜色=character(),
                            尺码号=character(),尺码=character(),单据日期=as.Date(character()),数量=numeric(),stringsAsFactors = FALSE)
    stocksdifference_change<-data.frame(单据编号=character(),单据类型=character(),源单类型=character(),仓库名称=character(),条码=character(),款号=character(),名称=character(),色号=character(),颜色=character(),
                            尺码号=character(),尺码=character(),单据日期=as.Date(character()),数量=numeric(),stringsAsFactors = FALSE)
    for (item in stockin_list) {
      # 提取特定键的值
      code <- item$stockin_code
      origin_type<-item$origin_type
      storage_name <- item$storage_name
      barcode<-item$barcode
      style_no<-item$style_no
      name<-item$name
      color_no<-item$color_no
      color_name<-item$color_name
      size_no<-item$size_no
      size_name<-item$color_name
      billdate<-item$billdate
      sku_quantity<-item$sku_quantity
      # 将提取的值添加到数据框中
      stocksin_change <- rbind(stocksin_change, data.frame(单据编号 = code,单据类型="入库单",源单类型=origin_type, 仓库名称=storage_name,条码=barcode,款号=style_no,名称=name,色号=color_no,颜色=color_name,
                                尺码号=size_no,尺码=size_name,单据日期=billdate,数量=sku_quantity, stringsAsFactors = FALSE))
    }
    #出库明细中的数量取负值
    # 使用 lapply 遍历stockout_list每个子列表，并对 sku_quantity 键的值取负
    stockout_list <- lapply(stockout_list, function(sublist) {
    sublist$sku_quantity <- -as.numeric(sublist$sku_quantity)
    return(sublist)
    })
    for (item in stockout_list) {
    # 提取特定键的值
    code <- item$stockout_code
    origin_type<-item$origin_type
    storage_name <- item$storage_name
    barcode<-item$barcode
    style_no<-item$style_no
    name<-item$name
    color_no<-item$color_no
    color_name<-item$color_name
    size_no<-item$size_no
    size_name<-item$color_name
    billdate<-item$billdate
    sku_quantity<-item$sku_quantity
    # 将提取的值添加到数据框中
    stocksout_change <- rbind(stocksout_change, data.frame(单据编号 = code,单据类型="出库单", 源单类型=origin_type,仓库名称=storage_name,条码=barcode,款号=style_no,名称=name,色号=color_no,颜色=color_name,
                              尺码号=size_no,尺码=size_name,单据日期=billdate,数量=sku_quantity, stringsAsFactors = FALSE))
   }
    for (item in stockdifference_list) {
    # 提取特定键的值
    code <- item$code
    origin_type<-"差异单"
    storage_name <- item$storage_name
    barcode<-item$barcode
    style_no<-item$style_no
    name<-item$name
    color_no<-item$color_no
    color_name<-item$color_name
    size_no<-item$size_no
    size_name<-item$color_name
    billdate<-as.Date(item$update_time,format = "%Y-%m-%d %H:%M:%S")
    sku_quantity<-item$difference_quantity
    # 将提取的值添加到数据框中
    stocksdifference_change <- rbind(stocksdifference_change, data.frame(单据编号 = code,单据类型="差异单",源单类型=origin_type, 仓库名称=storage_name,条码=barcode,款号=style_no,名称=name,色号=color_no,颜色=color_name,
                              尺码号=size_no,尺码=size_name,单据日期=as.character(billdate),数量=sku_quantity, stringsAsFactors = FALSE))
  }

   #将入库明细、出库明细、差异明细合并为一个数据框
   stocks_change<-rbind(stocksin_change,stocksout_change,stocksdifference_change)
   #对合并后的stocks_change进行整合，计算每天的期初库存、期末库存
   #1-计算每个仓库每天的期初库存、期末库存
   if((nrow(stocks_change) == 0) || (ncol(stocks_change) == 0)){
      return(list(
      style_daily_stocks_by_storage1=stocks_change
          ))}
          else{
   style_daily_stocks_by_storage<-aggregate(cbind(as.numeric(数量))~仓库名称+款号+单据日期, data = stocks_change, FUN = sum)
   #将汇总后新生成的列"V1"命名为“变化量”
   names(style_daily_stocks_by_storage)[names(style_daily_stocks_by_storage) == "V1"] <- "变化量"
    # 使用dplyr包中的mutate和lag函数，对style_daily_stocks_by_storage中的变动量：
   #按“仓库名称”为维度，以“每个仓库”数据的第一行为期初值，之后的每一行累加前一行的值作为当前行的期末值
   style_daily_stocks_by_storage <- style_daily_stocks_by_storage %>%
   group_by(仓库名称)%>%mutate(期末库存 = cumsum(变化量))%>% ungroup
   #按单据、条码汇总每个条码的库存变化
   barcode_stocks_change_by_storage<-stocks_change %>%
   group_by(仓库名称,条码)%>%mutate(期末库存 = cumsum(数量))%>% ungroup
  #记录reactive执行时间的语句要写在return之前
   return(list(
      style_daily_stocks_by_storage1=style_daily_stocks_by_storage,
      barcode_stocks_change_by_storage1=barcode_stocks_change_by_storage
      ))}
  })
  #UI-根据是否选择属性确定“趋势图”的显示内容
  output$stocks_trend_plot<-renderUI({
  #模块化的UI或renderUI都需要使用ns<-NS(id)开始
  ns <- NS(id)
      selected_product<-selected_product()
      if(length(selected_product)==0){
        list(
        tags$h3("请选择款号!"))
      }else{
        data<-stocks_trend_data()$style_daily_stocks_by_storage1
        if((nrow(data) == 0)){
        list(
        tags$h3("未查询到改款号的库存数据!"))
      }else{  
       list(
         actionButton(ns("refresh"),"刷新",style="margin-top:10px;",icon = icon("refresh")),
         mainPanel(class = "main-panel",
         plotlyOutput(ns("stocks_trend")),
         DT::dataTableOutput(ns("stocks_trend_table"))
         )
        )}
     } 
  })
 #输出趋势图
 output$stocks_trend<-renderPlotly({
  data<-stocks_trend_data()$style_daily_stocks_by_storage1
  data$单据日期<-as.Date(data$单据日期, format = "%Y-%m-%d") # 如果没有数据，取消后续渲染
  req(nrow(data) > 0, cancelOutput = TRUE)
  p<-ggplot(data, aes(x = 单据日期, y = 期末库存,color = 仓库名称)) +
     geom_line() +  # 添加线条
     geom_point() + # 添加点
     labs(title = "库存趋势", x = "日期", y = "库存") + # 添加标题和轴标签
     theme_minimal()+# 使用简洁的主题
     scale_color_discrete(name = "仓库名称")# 为图例添加标题
  p <- ggplotly(p)
 })
 #输出结果表格
 output$stocks_trend_table<-DT::renderDataTable({
    stocks_trend_data()$barcode_stocks_change_by_storage1
  })
  # 在UI中显示每个reactive的执行时间
  output$stocks_changes_reactive_time <- renderText({
    paste("stocks_changes_reactive_time: ", execution_times$stocks_changes_reactive_time)
  })
  }
 )} 