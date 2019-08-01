get_plot_parameters <- function(plot_parameters){
  
  
  if(missing(plot_parameters)){
    
  cat("\nUsing default plot parameters")
    plot_parameters <- NULL
    
  }
  

  
  
  
  
  
  
  if(!("errorWidth" %in% names(plot_parameters))) plot_parameters[["errorWidth"]] <- 0.01
  if(!("errorSize" %in% names(plot_parameters))) plot_parameters[["errorSize"]] <- 0.8
  # red color
  #errorCol <- "#B22608"
  # black color
  if(!("errorCol" %in% names(plot_parameters))) plot_parameters[["errorCol"]] <- grey(0)
  
  #colorHigh <- grey(0.35)
  if(!("colorHigh" %in% names(plot_parameters))) plot_parameters[["colorHigh"]] <- "#BE302E"
  
  #colorLow <- grey(0.6)
  if(!("colorLow" %in% names(plot_parameters))) plot_parameters[["colorLow"]] <- "#007ba5"
  

  if("limits" %in% names(plot_parameters)) limits <- plot_parameters[["limits"]]
  
  if("labs" %in% names(plot_parameters)) labs <- plot_parameters[["labs"]]
  
  colorSize <- 1
  
  
  # plot_parameters <- list(plot_parameters=plot_parameters,theme_ci_piace=theme_ci_piace)
  # plot_parameters <- list(theme_ci_piace=theme_ci_piace,limits=limits,labs=labs,
  #                         errorWidth=errorWidth,errorSize=errorSize,
  #                         errorCol=errorCol,colorSize=colorSize)
  
  # only for remembering these values
  # full page size 174 mm, cell press
  #tiff(filename = figure_path ,width = 174,height = 170,units = "mm",res=300)
  
  return(plot_parameters)
}


get_theme <- function(){
  
  extrafont::loadfonts(device="win",quiet = TRUE)
  theme_ci_piace <- theme_minimal() +
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=16,family="Arial"),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size=10),
          axis.title = element_text(size=10),
          legend.position = c(0.2, 0.9),
          legend.text = element_text(size=8),
          plot.title = element_text(hjust = 0.5,size=12))
  
  
  return(theme_ci_piace)
}


errorFunMin <- function(x) mean(x) - sd(x)/sqrt(length(x))
errorFunMax <- function(x) mean(x) + sd(x)/sqrt(length(x))

