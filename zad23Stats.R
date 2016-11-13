saveZad23Stats <- function(df){
  df %>% group_by(algorithm) %>% 
    summarise(czasObliczen=sum(runningTime), 
              najlepszyWynik=min(value), 
              jakosc=taiOpt/min(value)) %>% 
    arrange(desc(jakosc)) %>%
    write.csv(file="zad23Stats.csv", quote=FALSE)
}