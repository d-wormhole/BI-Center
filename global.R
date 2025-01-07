
# 全局变量及函数--接口地址
sizehorse_erp_api <- function() {
  # 返回接口地址
  data.frame(
    erp =c("https://api.erp.sizehorse.com"),
    drp = c("https://api.erp.sizehorse.com/app/drp"),
    pos = c("https://api.erp.sizehorse.com/app/pos"),
    img_url=c("http://image.sizehorse.com/files/"),
    img_size=c("?x-oss-process=image/resize,m_fill,limit_0,w_180,h_240")
  )
}
Permanent_token<-function(){
  #返回长期有效token
  data.frame(
    erp_token =c("未设定"),
    drp_token =c("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdGFmZl9pZCI6IjQyMTMiLCJhY2NvdW50IjoiYWRtaW4iLCJzdGFmZl9uYW1lIjoiXHU3YmExXHU3NDA2XHU1NDU4IiwiY29tcGFueV9pZCI6IjI4MzMiLCJjb21wYW55X2NvZGUiOiJCWUNSIiwiY29tcGFueV9uYW1lIjoiXHU2ZDU5XHU2YzVmXHU1ZTAzXHU4ODYzXHU4MzQ5XHU0ZWJhXHU2NzBkXHU5OTcwXHU2NzA5XHU5NjUwXHU1MTZjXHU1M2Y4IiwibGV2ZWwiOiIyIiwibG9naW5fdGltZSI6MTczNTAyMjM4NCwiZXhwaXJlc190aW1lIjoxNzM1NjI3MTg0LCJpc19hZG1pbiI6MSwic2Vzc2lvbl9pZCI6IjI4MzM0MjEzMTczNTYyNzE4NCJ9.aY0aNEy0bsVuGNnDqh6jxXcIHILcgvdkIsHLvY8fBvs
"),
    pos_token = c("未设定")
  )
}
#使用ggplot2输出属性分布“矩形树图（Treemap）”
create_treemap <- function(data, area_var, fill_var, subgroup_var , subgroup2_var =NULL, subgroup3_var=NULL ) {
  # 基础的ggplot调用
  p <- ggplot(data, aes(area = get(area_var), fill = get(fill_var),subgroup = get(subgroup_var),
                        subgroup2 = if(!is.null(subgroup2_var)) get(subgroup2_var) else NULL, subgroup3 = if(!is.null(subgroup3_var)) get(subgroup3_var) else NULL))+
    geom_treemap()+labs(title = "属性库存分布") +theme_minimal()+  geom_treemap_text(                                         
      aes(label =get(subgroup_var)),  # 使用动态列名
      place = "centre",
      grow = TRUE,
      colour = "black",
      size = 0
    )
  # 如果提供了子分组变量，添加子分组图层
  if (!is.null(subgroup_var)) {
    p <- p + geom_treemap_subgroup_border() +
      geom_treemap_subgroup_text( place = "centre", grow = TRUE, alpha = 0.2, colour = "black", fontface = "italic")
  }
  # 如果提供了第二层子分组变量，添加第二层子分组图层
  if (!is.null(subgroup2_var)) {
    p <- p + geom_treemap_subgroup2_border(colour = "black", size = 5) +
      geom_treemap_subgroup2_text(colour = "white", alpha = 0.4, fontface = "italic")
  }
  # 如果提供了第三层子分组变量，添加第三层子分组图层
  if (!is.null(subgroup3_var)) {
    p <- p + geom_treemap_subgroup3_border(colour = "black", size = 1) +
      geom_treemap_subgroup3_text(place = "top", colour = "blue", alpha = 0.8)
  }
  # 返回ggplot对象
  return(p)
}

# 使用plotly创建一个函数来生成动态三层树状图
create_treemap_plotly <- function(labels, parents, values, root_color = "lightgrey", maxdepth = 4) {
  # 创建一个plotly树状图对象
  fig <- plot_ly(type = "treemap",
                 labels = labels,
                 parents = parents,
                 values = values,
                 root_color = root_color,
                 textposition = "inside",
                 insidetextanchor = "middle",
                 branchvalues = "total",
                 textinfo = "label+value")
                 # %>%
                 # layout(
                 #    autosize = TRUE,      # 自动调整大小
                 #    height = 600,         # 指定高度
                 #    width = 800,          # 指定宽度
                 #    margin = list(t = 50, b = 50)  # 调整边距
                 #  )
                  
  # 设置树状图的最大深度
  fig <- fig %>% layout(maxdepth = maxdepth)
  
  # 更新布局，设置边距
  fig <- fig %>% layout(title = "属性分布(按选择的前三个属性逐级显示)",margin = list(t = 50, l = 25, r = 25, b = 25))
  
  # 返回图形对象
  return(fig)
}
