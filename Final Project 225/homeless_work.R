library(tidyverse)
library(dplyr)
library(readr)

#load in data-------------------------------------------------
homeless <- read.csv('~/Desktop/FinalProjectSOC225/Final Project 225/data/homelessness/2007-2016-Homelessnewss-USA.csv')

#homeless df - note: the count value is not the rate.--------- 
#change the year to actual year in numeric bc it is in 1/1/YYYY format
homeless$Year <- as.numeric(gsub("1/1/", "",as.character(homeless$Year))) 
#global variable - because this will be use more often through out the file
years <- sort(as.array(unique(homeless$Year)))

#a directory output to store the sum of total homeless
#dir.create("~/Desktop/soc225/Final Project 225/clean_output") 

#Looking the data measure at "Total Homeless" (t.h) level-----------------------------------------
#calculate the sum of the totalhomeless count by year and state; and droping 
#these states from total homeless count because the unemployment does not has 
#data about these states. 
totalHomeless_sum <- homeless %>% filter(Measures == 'Total Homeless') %>% arrange(State) %>%
  group_by(Year, State) %>%
  summarise(total.homeless_sum = sum(as.numeric(gsub(",","",Count)))) %>%
  filter(!State %in% c("AK", "DC", "FL", "GA", "GU", "PR", "VI"))

write.csv(totalHomeless_sum, 
          "~/Desktop/soc225/Final Project 225/clean_output/homeless-total-output.csv", 
          row.names =FALSE)

#USE IT TO TEST THE ACCURACY OF SUM METHOD 
#return the Total Homeless count of that year by state and arrange it in 
#ascending order. Since the data structure of unemployment is completely different 
#the best way to do it for now is looking at the sum of the year for homeless data, 
#and for unemployment, also sum up the data of the rate. 
#Looking the data measure at "Total Homeless" (t.h) level-------------------------------------------
df <- homeless %>% filter(Measures == 'Total Homeless') %>% arrange(State)

#return the sum of the count of homeless of the state in that one year
addValue <- function(df.t , y) {
  sumVal <- 0
  dataYear <- df.t %>% filter(Year == y)
  size <- dataYear %>% select(Count) %>% nrow
  for(i in 0:size) {
    #stripped any comma that was found in the count value
    sumVal <- sum(sumVal + as.numeric(gsub(",","",dataYear$Count[i])))
  }
  return(sumVal)
}
#the function will filter down the dataframe to state level then find the sum of total homeless of the state in each year
#then return a dataframe of that state (the sum of total homeless) in a dataframe
totalHomelessMeasureState<- function(code) {
  data <- df %>% filter(State == code)
  val <- c()
  l_year <- as.array(unique(data$Year)) 
  for(i in l_year) {  #converted to array for index purpose
    val <- c(val, addValue(data, i))
  }
  df <- data.frame(Year = l_year, State = code, Sum_Total_Homeless = val)
  return(df)
}

l_state <- unique(df$State) #list of state
clean_total_homeless_count <- lapply(l_state, FUN=totalHomelessMeasureState)  %>% bind_rows()

#test run ---------------------------------------------------------------------------------------
#data frame include Year / State / Measures / total count of that one year
#AL_state <- totalHomelessMeasureState('AL')
# df_t <- df %>% filter(State == 'AL')
# write.csv(df_t, 
#           "~/Desktop/soc225/Final Project 225/clean_output/test_homeless.csv", 
#           row.names =FALSE)
# AK_state <- totalHomelessMeasureState('AK')
# CA_state <- totalHomelessMeasureState('CA')
# AK <- df %>% filter(State == 'AK', Measures == 'Total Homeless') %>% arrange(Year)
# addValue(AK, 2007)
# addValue(AK, 2010)
# addValue(AK, 2016)
# AL_total_homeless <- df %>% filter(State == 'AL', Measures == 'Total Homeless') %>% arrange(Year))
# addValue(AL_total_homeless, 2007) #5452   result
# addValue(AL_total_homeless, 2008) #5387
# addValue(AL_total_homeless, 2016) #4561
# CA <- df %>% filter(State == 'CA', Measures == 'Total Homeless') %>% arrange(Year)
# addValue(CA, 2007)
# addValue(CA, 2010)
# addValue(CA, 2016)

#drop column(s) and rename column(s)------------------------------------------------------------
# drop.cols <- c("CoC.Number", "CoC.Name")
# unemploy_homeless <- shelby_unemploy %>% full_join(shelby_homeless, by = "Year") %>%
#   select(-one_of(drop.cols)) %>% select(Year, Month, State.x, State.y,
#                                         County, Measures, Rate, Count)
# 
# unemploy_homeless <- plyr::rename(unemploy_homeless, c("State.x" = "State Name", 
#                                                        "State.y" = "State Code",
#                                                        "Rate" = "Unemploy Rate",
#                                                        "Count" = "Homeless Count"))
#example how to use str_detect-----------------------------------------------------------------
# str: #https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html
# test str <- "Birmingham/Jefferson, St. Clair, Shelby Counties CoC"
# if(str_detect(test, 'Shelby')){ print("true") }
#change a list from horizontal to vertical-----------------------------------------------------
#matrix(unlist(list_name))   