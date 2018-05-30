library(tidyverse)
library(dplyr)

master <- read.csv("~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/data.csv")

region <- read.csv('~/Desktop/FinalProjectSOC225/Final Project 225/census-regions-master/us census bureau regions and divisions.csv')

colnames(region) <- tolower(colnames(region))
join <- suppressWarnings(suppressMessages(master %>% left_join(region , by = "state"))) 

colnames(join) <- tolower(colnames(join))

join <- join %>% select(year, division, region, state, state.code.x, est_pop, 
                        sum_total_homeless, sum_unemploy_rate, 
                        mean.homeless, mean.unemploy, 
                        homeless_proportion, unemploy_proportion) %>% rename(state.code = state.code.x)

#master data frame 
write.csv(join,  
          "~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/master_data.csv", 
          row.names =FALSE)
