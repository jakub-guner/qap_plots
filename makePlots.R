library(ggplot2)
library(dplyr)
library(tidyr)
source("optimumPlots.R")
source("timePlots.R")
source("stepsPlots.R")
source("zad3Plots.R")
source("zad4Plots.R")
source("zad23Stats.R")

df <-read.csv("results.csv", strip.white=TRUE)
df23 <- read.csv("zad2_3.csv", strip.white=TRUE)
df34 <- read.csv("resultsZad3-4.csv", strip.white=TRUE)

#ordering of categorical features
df$instance <- factor(df$instance, levels = c("chr12b", "had12", "esc32f", "bur26a", "esc32f", "tai40b", "tai50b","tai60a","tai80a"))
df$algorithm <- factor(df$algorithm, levels = c("random", "heuristic", 
                                                "greedy-2swap",  "simAnn-2swap",
                                                "steepest-2swap", "tabu-2swap",
                                                "greedy-3swap", "simAnn-3swap",
                                                "steepest-3swap", "tabu-3swap")
                       )

#creating and saving all plots to PNG files
saveQualityPlots(df)
saveTimePlots(df)
saveStepsAndEvalSolutions(df)
# saveZad3Plots(df34)
# saveZad4Plots(df34)
# saveZad23Stats(df23)
