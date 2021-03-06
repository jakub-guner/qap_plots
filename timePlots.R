# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

makeTimePlot <- function(df){
  ggplot(df, aes(x=algorithm, y=runningTime)) + 
    scale_y_log10() +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
    theme_bw() +
    theme(axis.title.x=element_blank(), 
          axis.text=element_text(size=10), 
          axis.title.y=element_text(size=20)
    )
  
}

saveTimePlots <-function(df){
  for (qapInstance in levels(df$instance)){
    instanceDF=df%>%filter(instance==qapInstance)
    timePlot<-makeTimePlot(instanceDF)
    fileName <-paste(as.character(qapInstance), "Time.png", sep="")
    ggsave(fileName, plot = timePlot, width = 12, height = 3)
  }
}