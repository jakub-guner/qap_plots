saveZad3Plots <- function(blah){
  df <- blah %>% mutate(init_quality=getOptimum(instance)/initialValue, final_quality=getOptimum(instance)/value)
  
  for (qapInstance in levels(df$instance)){
    dfZad3 <- df %>% filter(instance==qapInstance)
    plotZad3 <- ggplot(dfZad3, aes(x=init_quality, y=final_quality, color=algorithm)) + geom_point() + theme_bw()
    fileName <-paste(as.character(qapInstance), "_InitVsFinalQuality.png", sep="")
    ggsave(fileName, plot = plotZad3, width = 7, height = 3)
  }
  
  for (qapAlgorithm in levels(df$algorithm)){
    dfZad3 <- df %>% filter(algorithm==qapAlgorithm)
    plotZad3A <- ggplot(dfZad3, aes(x=init_quality, y=final_quality, color=instance)) + geom_point() + theme_bw()
    fileName <-paste(as.character(qapAlgorithm), "_InitVsFinalQuality.png", sep="")
    ggsave(fileName, plot = plotZad3A, width = 7, height = 3)
  }
  
}