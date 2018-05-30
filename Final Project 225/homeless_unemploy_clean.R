library(tidyverse)
library(dplyr)
#install.packages("httpuv", type="binary")
#load in data-------------------------------------------------
totalHomeless_sum <-read_csv('~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/homeless-total-output.csv')
unemployRate_sum <- read.csv('~/Desktop/FinalProjectSOC225/Final Project 225/clean_output/unemploy-total-output.csv')

#Join both data---------------------------------------------------------------------------------
#the reason a state-code data frame was create to make sure when merge 
#homeless data and unemploy together, the data will go according to the state
#since the way the data structure is completely different 
#concept for rename(df, new.name = old.name)
state_code <- data.frame(state.code = state.abb, state.nam = state.name) %>% 
  filter(!state.abb %in% c("FL", "AK", "GA"),
         !state.name %in% c("Florida", "Alaska", "Georgia"))

totalHomeless_sum <- totalHomeless_sum %>% rename(state.code = State)
#merge homeless data with state code first
jointed_homeless_unemploy <- merge(x= totalHomeless_sum, y= state_code, all.x=TRUE)
#then merge unemploy after it - to prevent error 
unemployRate_sum <- unemployRate_sum %>% rename(state.nam = State) #for merging purpose
jointed_homeless_unemploy <- merge(x = jointed_homeless_unemploy, y = unemployRate_sum) %>% arrange(state.nam)

# #merge df 
# write.csv(full_data, 
#           "~/Desktop/soc225/Final Project 225/clean_output/jointed.csv", 
#           row.names =FALSE)
#------------------------------------------------------------------------------------------------
#using for loop to create multiple lists
# for(i in sort(years, decreasing = FALSE)){ #used for loop to create multiple trace
#   val <- (clean_total_homeless_count %>% filter(Year == i))$Sum_Total_Homeless
#   name <- assign(paste("trace", i), val)
# }
