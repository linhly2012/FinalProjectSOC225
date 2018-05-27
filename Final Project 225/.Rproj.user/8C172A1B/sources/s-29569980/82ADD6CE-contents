library(tidyverse)
library(dplyr)

#state population (on county level) 2000 - 2010: 
#https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-counties.html
#IN ONE FILE
#https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html
#https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t

unemployment <- read.csv('data/unemployment-by-county-us/output.csv')

#only looking at 2007 - 2016, to match with homeless data. 
#choose to filter down to only 
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

#test with the function
# W <- addSumRateState('Wyoming')
# A <- addSumRateState('Alabama')

#Alaska, Florida, Georgia, was excluded
l_state_name <- sort(unique(unemploy_2007.2016$State))
clean_unemploy <- lapply(l_state_name, FUN=addSumRateState) %>% bind_rows()

write.csv(clean_unemploy, 
          "~/Desktop/soc225/Final Project 225/clean_output/unemploy-total-output.csv", 
          row.names =FALSE)

# use it to test if the sum func work properly------------------------------------------------
addValue <- function(df.u) {
  sumVal <- 0
  size <- df.u %>% select(Year) %>% nrow
  for(i in 0:size) {
    sumVal <- sum(sumVal + df.u$Rate[i])
  }
  return(sumVal)
}
Wyo <- unemploy_2007.2016 %>% filter(Year == 2010, State == 'Wyoming')
Ala <- unemploy_2007.2016 %>% filter(Year == 2016, State == 'Alabama')
print(paste("Wyo 2010,", addValue(Wyo)))
print(paste("Ala 2016,", addValue(Ala)))

