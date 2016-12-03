saveStepsAndEvalSolutions <- function(dataframe){
  steps <- df %>% 
    filter(steps != -1) %>% 
    group_by(instance, algorithm) %>% 
    summarise(steps_mean=mean(steps), eval_sol=mean(evaluatedSolutions))
  
  stepsPlot <- ggplot(steps, aes(x=algorithm, y=steps_mean, color=instance)) + 
    scale_y_log10() + 
    geom_point() + 
    theme_bw() +
    theme(axis.title.x=element_blank(), 
          axis.text=element_text(size=10), 
          axis.title.y=element_text(size=20)
    )
  
  evalSolPlot <- ggplot(steps, aes(x=algorithm, y=eval_sol, color=instance)) + 
    scale_y_log10() + 
    geom_point() + 
    theme_bw() +
    theme(axis.title.x=element_blank(), 
          axis.text=element_text(size=10), 
          axis.title.y=element_text(size=20)
    )
  
  
  ggsave("stepsPlot.png", plot = stepsPlot, width = 12, height = 3)
  ggsave("evalSolPlot.png", plot = evalSolPlot, width = 12, height = 3) 
}