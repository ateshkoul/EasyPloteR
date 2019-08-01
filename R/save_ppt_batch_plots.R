save_ppt_batch_plots <- function(Data_file_name,save_tiff=FALSE,save_ppt=FALSE,description,
                                 folder_path,figure_path){
  
  
  Data <- read.csv(file.path(folder_path,paste0(Data_file_name,".csv")))
  
  
  if(save_tiff) {
    if(missing(figure_path)) figure_path <- "Sub_projects/Realtime_models/SVM_classification_models/Results/Figures/"
    figure_path <- file.path(,paste0(Data_file_name,".tiff"))
    # full page size 174 mm, cell press
    tiff(filename = figure_path ,width = 174,height = 170,units = "mm",res=300)
  }
  
  time_colNo <- grep("X([[:digit:]])",names(Data))
  # it is necessary to do it this way because otherwise the order of time points is based on strings
  new_times <- as.factor(as.double(gsub("X","",grep("X([[:digit:]])",names(Data),value = T))))
  
  
  p1 <- easy_plot(Data,y_col = "y_col",x_col = "time","plot_unprocessed",perform_reshape=TRUE,
                  gen_aes_list = list(group="type",color="type"),
                  reshaping_list = list(varying_list=time_colNo,
                                        idvar=c("subj","type","X"),
                                        new_times = new_times),
                  plot_parameters = list(labs=labs(title=Data_file_name,x ="time", y = "Accuracy"),
                                         limits = coord_cartesian(ylim = c(0.4,1))))
  
  
  # # "X" is the row number
  # p1 <- easy_plot(Data,y_col = "y_col",x_col = "time","plot_unprocessed",perform_reshape=TRUE,
  #                 gen_aes_list = list(group="type",color="type"),
  #                 reshaping_list = list(varying_list=4:13,
  #                                       idvar=c("subj","type","X"),
  #                                       new_times = as.factor(seq(10,100,10))),
  #                 plot_parameters = list(labs=labs(title=Data_file_name,x ="time", y = "Accuracy"),
  #                                        limits = coord_cartesian(ylim = c(0.4,1))))
  
  # p1 <- easy_plot(Data,y_col = "y_col",x_col = "time","plot_unprocessed",perform_reshape=TRUE,
  #                 gen_aes_list = list(group="type",color="type"),
  #                 reshaping_list = list(varying_list=4:23,
  #                                       idvar=c("subj","type","X"),
  #                                       new_times = as.factor(seq(5,100,5))),
  #                 plot_parameters = list(labs=labs(title=Data_file_name,x ="time", y = "Accuracy"),
  #                                        limits = coord_cartesian(ylim = c(0.4,1))))
  # if u dont use this, the plot isnt shown
  print(p1)
  if(save_tiff) dev.off()
  
  if(save_ppt){
    
    save_ppt_title_image(slide_title = description[[Data_file_name]],slide_image = print(p1),
                         file.path("Sub_projects/Realtime_models/SVM_classification_models/Results/results_linear.pptx"))
    
  }
  
  
  return(p1)
}