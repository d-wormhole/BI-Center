
#注意该模块在主服务（main_server)运行时，需要依赖”注册服务“的返回注册信息，因此需要在外部function需要设定一个额外的参数，即：erp_response；
#类似地，如果模块有其他数据要引入，也需要设定参数，并且在主服务内设定参数的初始值；初始值可以直接赋值，或者调用其他模块的返回值
stocks_analysServer <- function(id,erp_response) {
  moduleServer(
  id,
   function(input, output, session){
           #引用”注册服务"模块的返回值，需要设定一个reactive变量；注意reactive内部获取返回值的赋值格式“userinfo<-erp_response$erp_response1()”，这个很重要
           #若只返回一个变量，return可写可不写，如果是有多个，必须要指定返回的值；同时返回多个值的话，return内部要使用list
            userinfo<-reactive({
              userinfo<-erp_response$erp_response1()
              return(userinfo)
              })
            #后面是当前模块内部的逻辑，注意如果有renderUI,也需要使用ns<NS(id)的模块化格式
            #1-登录成功后，获取登录账号的属性组数据
            attribute_group<-reactive({
            userinfo<-userinfo()
            if(userinfo$error==0){
                token<-userinfo$data$token
                url<-paste(sizehorse_erp_api()$drp,"/product_attribute_group",sep = "")
            #无需过滤，不需要穿参数；如需要传参使用httr包get方法中的“params”参数
            # params<-list(
            #   token=token,
            #   Content-Type="application/x-www-form-urlencoded",
            #   )
            #配置get请求的hearder参数
                headers <- c(
                `token` = token,
                `Content-Type` = "application/x-www-form-urlencoded",
                `Accept` = "application/json"
                )
              attrs_response<-GET(url, add_headers(.headers = headers),progress(), httr::config(max_recv_speed_large = 1000000))
              attrs<-content(attrs_response,type="application/json")
              attrs_name<-c()
              for (i in seq_along(attrs$data))
                attrs_name<-c(attrs_name,attrs$data[[i]]$name)
            }
            else{
              next
            }
            #若一个reactive中包含多个变量，默认返回最后一个变量；最好return指定要返回的变量
            return(attrs_name)
            })
          #2-登录成功后，获取登录账号的仓库数据
          storages<-reactive({
            userinfo<-userinfo()
            if(userinfo$error==0){
                token<-userinfo$data$token
                url<-paste(sizehorse_erp_api()$drp,"/storage",sep = "")
                #配置get请求的hearder参数
                headers <- c(
                `token` = token,
                `Content-Type` = "application/x-www-form-urlencoded",
                `Accept` = "application/json"
                )
                #配置get请求的param参数
                params<-list(
                 type=1,
                 status=0
                )
              storage_response<-GET(url, add_headers(.headers = headers),query = params,progress(), httr::config(max_recv_speed_large = 1000000))
              storages<-content(storage_response,type="application/json")
              storagename<-c()
              for (i in seq_along(storages$data$data))
                storagename<-c(storagename,storages$data$data[[i]]$name)
              storage_id<-c()
              for (i in seq_along(storages$data$data))
                storage_id<-c(storage_id,storages$data$data[[i]]$id)
            }
            else{
              next
            }
            #返回获取的仓库名称、仓库id
             return(list(
              storagenames=storagename,
              storage_ids=storage_id
              ))
            })
          # 使用 reactiveVal 触发数据更新
          # 初始化一个触发器
          dataUpdateTrigger <- reactiveVal(NULL)
          #2-登录成功后，导出登录账号的库存excel
          stocks<-reactive({
            # 每次触发时都会重新获取数据
            dataUpdateTrigger()
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
            url<-paste(sizehorse_erp_api()$drp,"/stocks",sep = "")
            storageids<-storages()$storage_ids
            all_storageids<-paste(storageids,collapse=",")
            #配置post请求
            formData <- list(
                `_method` = "export",
                storage_id = all_storageids,
                filetype = "xlsx",
                mode ="sku",
                hide_zero=1,
                with_img=0
              )
            headers <- c(
                `token` = token,
                `Content-Type` = "application/x-www-form-urlencoded",
                `Accept` = "application/json, text/plain, */*"
                )
              stocks_response<-POST(url,add_headers(.headers = headers),body = formData, encode = "form",progress(), httr::config(max_recv_speed_large = 1000000)) #导出库存请求
              stocks<-content(stocks_response,as = "raw")
              # 移除加载提示
              removeModal()}
            else{
              next
            }
            return(stocks)
            })
          #UI-根据属性数据渲染属性选择
          output$secondinput <-renderUI({
            #模块化的UI或renderUI都需要使用ns<-NS(id)开始
            ns <- NS(id)
             #如果没有登录，显示为请登录
              userinfo<-userinfo()
              if(is.null(userinfo$error)){
                tags$h3("请登录!")
              }
              else{
              attrs<-attribute_group()
              lapply(1:length(attrs), function(i) {
                selectInput(ns(paste0("selected_cols_", i)), paste0(), choices = c("请选择"="请选择",attrs))
              })}
            })
          #获取用户选择的属性 
          selected_attributes <- reactive({
             selected_attrs<-c()
             attrs<-attribute_group()
             for (i in 1:length(attrs)) {
              selected_cols <-paste0("selected_cols_", i)
              if (input[[selected_cols]]!="请选择"){
              selected_attrs<-c(selected_attrs,input[[selected_cols]])
              }
              else{
                next
              }
            }
            return(selected_attrs)
          })
           #UI-根据是否选择属性确定表格区域的显示内容
           output$shuxing<-renderUI({
            #模块化的UI或renderUI都需要使用ns<-NS(id)开始
            ns <- NS(id)
            #如果是多重if判断，条件判断语句判断条件变量要紧跟在if前，不可以把多个if盘点的条件变量放在一起，
            #例如当前判断内不可以把userinfo、group_id两个变量放在一起，否则会报错
            userinfo<-userinfo()
            if(is.null(userinfo$error)){
                tags$h3("")
              }
              else{
                group_id<-selected_attributes()
                is_select<-all(group_id == "请选择")
                if(is_select){
                  list(
                  tags$h3("选择统计属性!"))
                }else{
                 list(
                   downloadButton(ns("downloadTable"),"导出",style="margin-top:10px;"),
                   actionButton(ns("refresh"),"刷新",style="margin-top:10px;",icon = icon("refresh")),
                   mainPanel(class = "main-panel",
                   plotlyOutput(ns("stocks_attrs_distribution_plotly")),
                   DT::dataTableOutput(ns("product_attrs_details"))
                   )
                  )
                 }}
            })
          # 每次点击刷新按钮时更新触发器
          observeEvent(input$refresh, {
            dataUpdateTrigger(Sys.time())  # 使用当前时间戳作为触发器，强制重新评估 getUpdatedData()
            })
           #UI-根据仓库数据渲染仓库选择
           output$storages <-renderUI({
            #模块化的UI或renderUI都需要使用ns<-NS(id)开始
            ns <- NS(id)
             #如果没有登录，显示为请登录
            userinfo<-userinfo()
              if(is.null(userinfo$error)){
                tags$h3("")
              }
              else{
              storages<-storages()$storagenames
              selectInput(ns("storage_name"), "请选择仓库", choices = c("请选择"="请选择",storages))
              }
            })
           #获取用户选择的仓库
           selected_storage <- reactive({
            selected_cangku <- input$storage_name
            })
           #根据用户的选择汇总数据
           product_attrs_detail <- reactive({
            #根据选择的选择仓库过滤库存
            storage<-selected_storage()
            get_stocks<-stocks()
            # 将原始 Excel 文件内容写入临时文件
            temp_file <- tempfile(fileext = ".xlsx")
            writeBin(get_stocks, temp_file)
            # 使用 read_excel 读取临时文件中的数据到数据框
            stocks <- read_excel(temp_file)
            #将重复的列名唯一化处理
            colnames(stocks) <- make.names(colnames(stocks), unique = TRUE)
            #将未设定的属性值赋值“未定义”
            stocks[] <- lapply(stocks, function(x) ifelse(is.na(x), "未定义", x))
            storage_stocks <- subset(stocks, 仓库名称 == storage)
            #根据选择的属性构建分组计算公式
            group_id<-selected_attributes()
            #如果选择“全部”，从所有的库存数据中汇总选中的属性：库存、占用库存、可用库存
            if (storage == "请选择") {
              group_id1<-paste("cbind(库存, 占用库存, 可用库存) ~",paste(group_id,collapse=" + "))
              group_ids<-as.formula(group_id1) 
              result <- aggregate(group_ids, data = stocks, FUN = sum)
             }
            #如果选择了“具体仓库”，从“当前仓库”的库存数据中汇总选中的属性：库存、占用库存、可用库存
             else {
              group_id1<-paste("cbind(库存, 占用库存, 可用库存) ~仓库名称 + ",paste(group_id,collapse=" + "))
              group_id2<-as.formula(group_id1) 
              result <- aggregate(group_id2, data = storage_stocks, FUN = sum)
             }
            # 按照选择的“属性”命名对应的表格标题
            names(result)[names(result) == "get(group_id)"] <- group_id
            # 按照选择的“属性”先后顺序执行升序排序
            data <- result %>%arrange(across(all_of(group_id)))
             # 计算需要合计的列的值，这里我们假设所有数值列都需要合计
                  totalValues <- colSums(data[, sapply(data, is.numeric)], na.rm = TRUE)
                  # 创建合计行，只包含需要合计的列
                  totalRow <- data.frame(matrix(ncol = ncol(data), nrow = 1))
                  colnames(totalRow) <- colnames(data)
                  # 对于需要合计的列，添加合计值
                  for (colName in names(totalValues)) {
                    totalRow[[colName]] <- totalValues[[colName]]
                  }
                  # 对于不需要合计的列，添加NA或者空字符串
                  for (colName in names(data)[sapply(data, function(x) !is.numeric(x))]) {
                    totalRow[[colName]] <- NA
                  }
                  #第一列赋值为“合计”
                  totalRow[[1]]<-c("合计")
                  # 将合计行添加到数据框
                  dataWithTotal <- rbind(data, totalRow)
            return(dataWithTotal)
           })
          #输出汇总数据到表格
          output$product_attrs_details<-DT::renderDataTable({
              product_attrs_detail()  
              })
          #使用ggplot2输出静态treemap
          # output$stocks_attrs_distribution<-renderPlot({
          #   attrs_detail<-product_attrs_detail()
          #   attrs_detail<- attrs_detail[-nrow(attrs_detail), ]
          #   remove_columns<-c("仓库名称","库存","占用库存","可用库存")
          #   hierarchy_columns <- names(attrs_detail)
          #   hierarchy_columns <- hierarchy_columns[!hierarchy_columns %in% remove_columns]
          #   create_treemap(data=attrs_detail,area_var="可用库存",fill_var=if(!is.na(hierarchy_columns[3])) hierarchy_columns[3] else if(!is.na(hierarchy_columns[2]))  hierarchy_columns[2] else if(!is.na(hierarchy_columns[1])) hierarchy_columns[1] ,
          #                  subgroup_var =hierarchy_columns[1],
          #                  subgroup2_var=if(!is.na(hierarchy_columns[2])) hierarchy_columns[2] else NULL,
          #                  subgroup3_var=if(!is.na(hierarchy_columns[3])) hierarchy_columns[3] else NULL )
          # })
          #使用ploty输出动态treemap
          output$stocks_attrs_distribution_plotly<-renderPlotly({
            attrs_detail<-product_attrs_detail()
            attrs_detail<- attrs_detail[-nrow(attrs_detail), ]
            remove_columns1<-c("仓库名称","库存","占用库存")
            attrs_detail1 <- attrs_detail[,!(names(attrs_detail) %in% remove_columns1)]
            remove_columns<-c("仓库名称","库存","占用库存","可用库存")
            hierarchy_columns <- names(attrs_detail)
            hierarchy_columns <- hierarchy_columns[!hierarchy_columns %in% remove_columns]
            hierarchy_columns_1 <- hierarchy_columns[1]
            hierarchy_columns_2 <- hierarchy_columns[2]
            hierarchy_columns_3 <- hierarchy_columns[3]
            # 初始化labels, parents, 和values
            labels <- c()
            parents <- c()
            values <- c()
           if(length(hierarchy_columns)==1)
            # 循环遍历每一行数据，构建treemap的labels, parents 和 values
             for(i in 1:nrow(attrs_detail1)){
              # 添加level1的label
                labels <- c(labels, attrs_detail1[[hierarchy_columns_1]][i])
                parents <- c(parents, "")  # level1的父级是自己
                values <- c(values, attrs_detail1[i, "可用库存"]) # 聚合其value
              }
            else if(length(hierarchy_columns)==2)
             for(i in 1:nrow(attrs_detail1)){  
              # 添加level2的label，如果它是该level1的第一个出现的项
                labels <- c(labels, paste(attrs_detail1[[hierarchy_columns_1]][i],attrs_detail1[[hierarchy_columns_2]][i],sep = " > "))
                parents <- c(parents, attrs_detail1[[hierarchy_columns_1]][i])
                values <- c(values, attrs_detail1[i, "可用库存"])# 聚合其value
              # 添加level1的label
              if(!attrs_detail1[[hierarchy_columns_1]][i] %in% labels) {
                labels <- c(labels, attrs_detail1[[hierarchy_columns_1]][i])
                parents <- c(parents, "")  # level1的父级是自己
                values <- c(values, sum(attrs_detail1$可用库存[attrs_detail1[[hierarchy_columns_1]] == attrs_detail1[[hierarchy_columns_1]][i]])) # 聚合其value
              }}
            else if(length(hierarchy_columns)>=3)
            # 循环遍历每一行数据，构建treemap的labels, parents 和 values
             for(i in 1:nrow(attrs_detail1)){ 
              # 添加level3的label和value
              if(!paste(attrs_detail1[[hierarchy_columns_1]][i],attrs_detail1[[hierarchy_columns_2]][i],attrs_detail1[[hierarchy_columns_3]][i],sep = " > ") %in% labels )
              {
              labels <- c(labels, paste(attrs_detail1[[hierarchy_columns_1]][i],attrs_detail1[[hierarchy_columns_2]][i],attrs_detail1[[hierarchy_columns_3]][i],sep = " > "))
              parents <- c(parents, paste(attrs_detail1[[hierarchy_columns_1]][i],attrs_detail1[[hierarchy_columns_2]][i],sep = " > "))
              values <- c(values, if(length(hierarchy_columns)==3) attrs_detail1[i, "可用库存"] else sum(attrs_detail1$可用库存[attrs_detail1[[hierarchy_columns_3]]==attrs_detail1[[hierarchy_columns_3]][i]
                                                                                                                              &attrs_detail1[[hierarchy_columns_2]]==attrs_detail1[[hierarchy_columns_2]][i]
                                                                                                                              &attrs_detail1[[hierarchy_columns_1]]==attrs_detail1[[hierarchy_columns_1]][i]
                                                                                                                              ]))}
              
              # 添加level2的label，如果它是该level1的第一个出现的项
              if(!paste(attrs_detail1[[hierarchy_columns_1]][i],attrs_detail1[[hierarchy_columns_2]][i],sep = " > ") %in% labels )
              {
                labels <- c(labels, paste(attrs_detail1[[hierarchy_columns_1]][i],attrs_detail1[[hierarchy_columns_2]][i],sep = " > "))
                parents <- c(parents, attrs_detail1[[hierarchy_columns_1]][i])
                values <- c(values, sum(attrs_detail1$可用库存[attrs_detail1[[hierarchy_columns_2]]==attrs_detail1[[hierarchy_columns_2]][i]&attrs_detail1[[hierarchy_columns_1]]==attrs_detail1[[hierarchy_columns_1]][i]]))# 聚合其value
              }
              # 添加level1的label
              if(!attrs_detail1[[hierarchy_columns_1]][i] %in% labels) {
                labels <- c(labels, attrs_detail1[[hierarchy_columns_1]][i])
                parents <- c(parents, "")  # level1的父级是自己
                values <- c(values, sum(attrs_detail1$可用库存[attrs_detail1[[hierarchy_columns_1]] == attrs_detail1[[hierarchy_columns_1]][i]])) # 聚合其value
              }}
            create_treemap_plotly(labels, parents, values)
          })
          output$downloadTable <- downloadHandler(
              # 文件名
              filename = function() {
                paste("属性汇总", Sys.Date(), ".xlsx", sep = "")
              },
              # 文件内容
              content = function(file) {
                group_id<-selected_attributes()
                is_select<-all(group_id == "请选择")
                if (is_select){ 
                  message("属性未选择，不生成文件。")
                }
                else{
                product_attrs_detail<-product_attrs_detail()
                write.xlsx(product_attrs_detail, file)
                }
              }
            )
      return(list(
        attribute_group=attribute_group,
        storages=storages
        ))
    }
  )
}
