getOptimum <- Vectorize(function(instance){
  switch(as.character(instance),
         "chr12b" = 9742,
         "had12"  = 1652,
         "bur26a" = 5426670,
         "esc32f" = 2,
         "tai40b" = 637250948,
         "tai50b" = 458821517,
         "tai60a" = 7208572,
         "tai80a" = 13557864,
         "scr12"  = 31410,
         "chr20a" = 2192,
         "rou20"  = 725522 )
})

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

makeQualityPlot <- function(df){
  ggplot(df, aes(y=quality, x=algorithm)) + 
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
    theme_bw() +
    theme(axis.title.x=element_blank(), 
          axis.text=element_text(size=10), 
          axis.title.y=element_text(size=20)
          )
}

saveQualityPlots <-function(df){
  df <- df %>% mutate(quality=getOptimum(instance)/value)
  df$algorithm <- factor(df$algorithm, levels = c("random", "heuristic", 
                                                  "greedy-2swap",  "simAnn-2swap",
                                                  "steepest-2swap", "tabu-2swap",
                                                  "greedy-3swap", "simAnn-3swap",
                                                  "steepest-3swap", "tabu-3swap")
                         )
  for (qapInstance in levels(df$instance)){
    instanceDF <- df%>%filter(instance==qapInstance)
    optimumPlot <- makeQualityPlot(instanceDF)
    fileName <-paste(as.character(qapInstance), "Quality.png", sep="")
    ggsave(fileName, plot = optimumPlot, width = 12, height = 3)
  }
  
}