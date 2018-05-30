library(tidyverse)
library(dplyr)

# source: state population (on county level) 2000 - 2010 & 2010 - 2016: 
#https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html
#https://www.census.gov/data/tables/2016/demo/popest/state-total.html
#load df
unemployment <- read.csv('~/Desktop/FinalProjectSOC225/Final Project 225/data/unemployment-by-county-us/output.csv')

#only looking at 2007 - 2016, to match with homeless data.  
unemploy_2007.2016 <- unemployment %>% filter(Year >= 2007, Month == 'January')

addSumRateState <- function(name) {
  df <- unemploy_2007.2016 %>% filter(State == name)
  res <- c()
  years <- sort(as.array(unique(unemploy_2007.2016$Year)))
  for(i in years) {
    year_df <- df %>% filter(Year == i)
    res <- c(res,sum(year_df$Rate)) 
  }
  new_df <- data.frame(Year = years, State = name, Sum_Rate = res)
  return(new_df)
}

#Alaska, Florida, Georgia, was excluded
unemployRate_sum <- lapply(sort(unique(unemploy_2007.2016$State)), FUN=addSumRateState) %>% bind_rows()

write.csv(clean_unemploy, 
          "~/Desktop/soc225/Final Project 225/clean_output/unemploy-total-output.csv", 
          row.names =FALSE)
