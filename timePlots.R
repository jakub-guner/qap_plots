makeTimePlot <- function(df){
  ggplot(df, aes(x=algorithm, y=runningTime)) + 
    geom_boxplot(outlier.shape = NA) + 
    scale_y_log10() +
    coord_cartesian(ylim = quantile(df$runningTime, c(0, 0.95))) + 
    theme_bw()
  
}

saveTimePlots <-function(df){
  for (qapInstance in levels(df$instance)){
    instanceDF=df%>%filter(instance==qapInstance)
    timePlot<-makeTimePlot(instanceDF)
    fileName <-paste(as.character(qapInstance), "Time.png", sep="")
    ggsave(fileName, plot = timePlot, width = 7, height = 3)
  }
}