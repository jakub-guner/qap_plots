saveZad4Plots <- function(restartsDF){
  restartsClean <- restartsDF %>%
    group_by(instance, algorithm) %>%
    mutate(noOfRestarts=row_number(),
           bestSoFar=rollapply(value, FUN=min, width=noOfRestarts, align="right"),
           meanSoFar=rollapply(value, FUN=mean, width=noOfRestarts, align="right")
    ) %>%
    select(instance, algorithm, noOfRestarts, bestSoFar, meanSoFar) %>%
    gather(typeOfResult, result, bestSoFar:meanSoFar)
  
  for (qapInstance in levels(restartsClean$instance)){
    dfZad4 <- restartsClean %>% filter(instance==qapInstance)
    plotZad4 <- ggplot(dfZad4, aes(x=noOfRestarts, y=result, colour=typeOfResult)) +
      facet_wrap(~ algorithm, ncol = 1) +
      geom_point(size=0.2) + 
      theme_bw()
    fileName <-paste(as.character(qapInstance), "_Restarts.png", sep="")
    ggsave(fileName, plot = plotZad4, width = 7, height = 5)
  }
}