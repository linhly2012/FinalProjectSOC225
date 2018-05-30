library(tidyverse)
library(dplyr)

#data frame of homeless and unemploy sum rate and avg
data <- read.csv("~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/avg-homeless-unemploy.csv")

state_df <- read.csv('~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/state-pop-07-16.csv')

#join AVG, UNEMPLOY, HOMELESS, POPULATION, YEAR, STATE data frame together
jointed_a_s <- data %>% left_join(state_df, by= c("state.nam" = "state")
                                    ) %>% rename("state" = "state.nam") 

#re-structure the population format in a way that is not repeated more than due to the 
#way of the other data frame structure.
master_data <-  jointed_a_s %>% 
  gather(key = "Estimate Date", value = "Estimate", EST_JUL_2007:EST_JUL_2016) %>% 
  filter(Year == as.numeric(gsub("EST_JUL_", "",`Estimate Date`))) %>% 
  mutate(homeless_proportion = Sum_Total_Homeless / Estimate, 
         unemploy_proportion = Sum_Unemploy_Rate/ Estimate) %>%
  select(-c("Estimate Date")) %>% rename(est_pop = Estimate)

#master data frame 
write.csv(master_data, 
          "~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/data.csv", 
          row.names =FALSE)
#-----------------------------------------------------------------------------------------
#maybe pick two states from each region 

#as.numeric(gsub("EST_JUL_", "", as.character(colnames(test_a_s)))[5:14])

# test <- left_join(t_data, t_avg, by = "state") --> t() - transform the data format from h-> v, v ->h

# #re-structure the population format in a way that is not repeated more than due to the 
# #way of the other data frame structure.
# x <- test %>% 
#   gather(key = "Estimate Date", value = "Estimate", EST_JUL_2007:EST_JUL_2016) %>% 
#   filter(Year == as.numeric(gsub("EST_JUL_", "",`Estimate Date`))) %>% 
#   rename(avg_homeless_07to16 = mean.homeless, 
#          avg_unemploy_07to16 = mean.unemploy)
