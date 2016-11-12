getOptimum <- Vectorize(function(instance){
  switch(as.character(instance),
         "chr12b" = 9742,
         "had12"  = 1652,
         "bur26a" = 5426670,
         "esc32f" = 2,
         "tai40b" = 637250948,
         "tai60a" = 7208572,
         "tai80a" = 13557864,
         "scr12"  = 31410)
})

makeQualityPlot <- function(df){
  ggplot(df, aes(y=quality, x=algorithm)) + 
    geom_boxplot() + 
    theme_bw()
}

saveQualityPlots <-function(df){
  df <- df %>% mutate(quality=getOptimum(instance)/value)
  df$algorithm <- factor(df$algorithm, levels = c("random", "heuristic", "greedy-2swap", "greedy-3swap", "steepest-2swap", "steepest-3swap"))
  for (qapInstance in levels(df$instance)){
    instanceDF <- df%>%filter(instance==qapInstance)
    optimumPlot <- makeQualityPlot(instanceDF)
    fileName <-paste(as.character(qapInstance), "Quality.png", sep="")
    ggsave(fileName, plot = optimumPlot, width = 7, height = 3)
  }
  
}