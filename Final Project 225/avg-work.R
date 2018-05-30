library(tidyverse)
library(dplyr)

#data frame of homeless and unemploy sum rate 
data <- read.csv("~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/jointed.csv")

#calculating avg of homeless and unemploy data
avg <- data %>% group_by(state.nam, state.code) %>% 
  mutate(mean.homeless = mean(Sum_Total_Homeless),
            mean.unemploy = mean(Sum_Unemploy_Rate))

write.csv(avg, "~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/avg-homeless-unemploy.csv", 
          row.names =FALSE)
