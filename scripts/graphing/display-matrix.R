# The function to produce a complex display  will consist of three levels
# Level-1 : elemental graph, the cell of the super matrix (plot_info_one)
# Level-2 : GGally::ggmatrix wrapper (matrix_coef)
# Level-3 : grid wrapper (super_matrix)

# graphs a single model information index (LL, AIC, BIC)
plot_info_one <- function(
  x,
  model_type_="aefb",  
  process_ ="mmse",
  index_name = "BIC"
){
  # values for testing and development
  # x <- ds_catalog
  # model_type_="aefb"
  # process_ ="mmse"
  # index_name = "BIC"
  
  # name of the variable to story prettified index
  index_pretty <- paste0(index_name,"_pretty")
  # 
  d <- x %>% 
    dplyr::filter(model_type == model_type_) %>% 
    dplyr::filter(process == process_) %>% 
    # dplyr::select_(.dots = c())
    dplyr::distinct(
      model_name, model_number, wave_set, model_set, model_type, process, N, parameters, AIC, BIC
    ) %>%
    # tidyr::gather_(key = "index",value = "misfit_value", c("AIC","BIC")) %>% 
    dplyr::mutate(
      # index = factor(index),
      # counts = paste0("N = ",scales::comma(N),", p = ", parameters)
      n_p = paste0("  ",scales::comma(N),"-", parameters,"  ")
      
    )
  d[,index_pretty] <- sprintf("%1.0f", d[,index_name])
  d[,index_pretty] <- scales::comma(as.numeric(d[,index_pretty]))
  
  dd <- d %>% dplyr::distinct(model_set, n_p)  %>% dplyr::arrange(desc(model_set) )
  persons_parameters <- dd$n_p
  # custom_lables = levels(d %>% dplyr::select())
  # max_misfit <- max(d %>% dplyr::select(misfit_value))
  # max_misfit <- ceiling(max_misfit + .1*max_misfit)
  g <-  ggplot2::ggplot(d,aes_string(y     = "model_set", 
                                     x     = index_name
                                     # shape = "index"
                                     # color = "sign",
                                     # fill  = "sign",
                                     # shape = "model_number"
  ))  
  # g <- g + geom_point(size = 7, shape = 124)
  g <- g + scale_x_continuous(labels = scales::comma)
  # g <- g + geom_text(aes_string(label=index_pretty), size=4, vjust=.5, hjust = -.1)
  # g <- g + geom_text(aes_string(label=index_pretty), size=4, vjust=0, hjust = 0)
  g <- g + geom_text(aes_string(label=index_pretty), size=3)
  # g <- g + geom_text(aes(label = counts, x=Inf), hjust=-1)
  # g <- g + geom_text(aes(label = counts, x=Inf), hjust = .1)
  g <- g + scale_y_discrete(position = "left", labels = persons_parameters )
  # g <- g + scale_shape_manual(values = c("AIC"=65, "BIC"=66))
  # g <- g + guides(fill=FALSE, color=FALSE)
  g <- g + guides(fill=FALSE, color=FALSE, shape = FALSE)
  g <- g + labs(x = toupper(index_name), y = NULL)
  g <- g + main_theme
  # g <- g + theme(legend.position=c(.5,.5)) 
  # g <- g + theme(legend.background=element_rect(fill="white", colour="black"))
  g <- g + theme(axis.text.y = element_text(size=baseSize))
  g
}   
# ds_catalog %>% plot_info_one()




# uses plot_coef() to create a matrix of plots
matrix_coef <- function(
  x,
  term_group,
  model_type,
  process,
  info_index="BIC"
){
  # x <- ds_catalog
  # model_type="aefb"
  # process ="mmse"
  # # # term_group = "level"
  # # # term_group = "error"
  # term_group ="info_index"
  
  terms <- ls_terms[[term_group]]
  lst <- list()
  if(term_group=="info_index"){
    for(i in seq_along(terms)){
      lst[[i]] <- x %>% plot_info_one(model_type,process,index_name = ls_terms[["info_index"]])
    }
  }else{
    for(i in seq_along(terms)){
      lst[[i]] <- x %>% plot_coef(terms[i],model_type,process)
    }
  }
  pm <- GGally::ggmatrix(
    lst,
    nrow = 1, ncol = length(terms),
    # title = "MMSE",
    xAxisLabels = terms
    # yAxisLabels = "MMSE score",
    # legend = 1
  ) + theme(
    legend.position = "right",
    strip.text.x = element_text(size=baseSize+2)
    
  )
  pm
}
# matrix_coef(ds_catalog,"level","aefb","mmse")
# matrix_coef(ds_catalog,"misfit","aefb","mmse")

# used matrix_coef() to create a supermatrix of plots
super_matrix <- function(
  x,
  folder_name,
  process, 
  model_type,
  suffix = F,
  width, 
  height,
  res
){
  # x <- ds_catalog
  # model_type = "aefb"
  # process = "mmse"
  # folder_name = "./reports/model-examiner/graph2/"
  # height = 1200
  # width = 1400
  # res = 600
  # 
  # assemble the name of the file to be saved
  if(is.character(suffix)) {
    # suffix = "1"
    path_save  <- paste0(folder_name,process,"-",model_type,"-",suffix,".png")
    main_title <- paste0(suffix, ": estimated parameters modeling ",toupper(process) )               
  }else{
    path_save = paste0(folder_name,process,"-",model_type,".png")
    main_title <- paste0(toupper(process)," : model parameters")
  }
  
  
  g1 <- matrix_coef(x,"level",    model_type,process)
  g2 <- matrix_coef(x,"linear",   model_type,process)
  g3 <- matrix_coef(x,"quadratic",model_type,process)
  g4 <- matrix_coef(x,"error",    model_type,process)
  g5 <- matrix_coef(x,"info_index",   model_type,process) #+ theme(axis.text.y = element_blank())
  # g5 <- matrix_coef(x,"misfit",   model_type,process) + theme(strip.text = element_text)
  
  
  n_columns <- 18
  column_unit <- 100/n_columns
  # open PNG connection
  png(filename = path_save, width = width, height = height,res = res)
  vpLayout <- function(rowIndex, columnIndex) { return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) }
  grid::grid.newpage()
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=5, ncol=n_columns,
                              widths=grid::unit( rep(column_unit,n_columns) ,rep("null",n_columns)),
                              heights=grid::unit(c(.05,.24,.24,.24,.24), rep("null",5))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2:12),hjust = "1")
  # print(g1, vp=grid::viewport(layout.pos.row=2,layout.pos.col=1:18))
  # print(g2, vp=grid::viewport(layout.pos.row=3,layout.pos.col=1:18))
  # print(g3, vp=grid::viewport(layout.pos.row=4,layout.pos.col=1:18))
  # print(g4, vp=grid::viewport(layout.pos.row=5,layout.pos.col=1:4))
  # print(g5, vp=grid::viewport(layout.pos.row=5,layout.pos.col=5:12))
  print(g1, vp=grid::viewport(layout.pos.row=3,layout.pos.col=1:18))
  print(g2, vp=grid::viewport(layout.pos.row=4,layout.pos.col=1:18))
  print(g3, vp=grid::viewport(layout.pos.row=5,layout.pos.col=1:18))
  print(g4, vp=grid::viewport(layout.pos.row=2,layout.pos.col=1:4))
  print(g5, vp=grid::viewport(layout.pos.row=2,layout.pos.col=5:12))
  
  dev.off() # close PNG device
  # return(grid::popViewport(0))
}
