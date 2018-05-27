library(tidyverse)
library(dplyr)
#install.packages("httpuv", type="binary")
#load in data-------------------------------------------------
homeless <- read.csv('data/homelessness/2007-2016-Homelessnewss-USA.csv')
unemployment <- read.csv('data/unemployment-by-county-us/output.csv')

#note: the count value is not the rate. 
#change the year to actual year in numeric bc it is in 1/1/YYYY format
homeless$Year <- as.numeric(gsub("1/1/", "",as.character(homeless$Year))) 

#global variable - because this will be use more often through out the file
years <- sort(as.array(unique(homeless$Year)))

#homeless df--------------------------------------------------------------------------------------
#a directory output to store the sum of total homeless
#dir.create("~/Desktop/soc225/Final Project 225/clean_output") 

#return the Total Homeless count of that year by state and arrange it in 
#ascending order. Since the data structure of unemployment is completely different 
#the best way to do it for now is looking at the sum of the year for homeless data, 
#and for unemployment, also sum up the data of the rate. 

#Looking the data measure at "Total Homeless" (t.h) level-----------------------------------------
df <- homeless %>% filter(Measures == 'Total Homeless') %>% arrange(State)

#interest group : homeless individuals, homeless people in families , homeless veterans 
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

totalHomelessMeasureState<- function(code) {
  data <- df %>% filter(State == code)
  val <- c()
  for(i in years) {  #converted to array for index purpose
    val <- c(val, addValue(data, i))
  }
  df <- data.frame(Year = years, State = code, Sum_Total_Homeless = val)
  return(df)
}

l_state <- unique(df$State) #list of state
clean_total_homeless_count <- lapply(l_state, FUN=totalHomelessMeasureState)  %>% bind_rows()

#unemploy df ----------------------------------------------------------------------------------
#filter the data down to january level to match with the way homeless data was structure
unemploy_2007.2016 <- unemployment %>% filter(Year >= 2007, Month == 'January')

#the function will take it a state name and filter the dataframe to only that state leve
#and then loops through each year to find the sum of those year in one dataframe for 
#for one state (of the county level)
addSumRateState <- function(name) {
  df <- unemploy_2007.2016 %>% filter(State == name)
  res <- c()
  for(i in years) {
    year_df <- df %>% filter(Year == i)
    res <- c(res,sum(year_df$Rate)) 
  }
  new_df <- data.frame(Year = years, State = name, 
                       Sum_Unemploy_Rate = res)
  return(new_df)
}
#return a dataframe from 2007 to 2016 of each state unemployrate by year. 
l_state_name <- sort(unique(unemploy_2007.2016$State))
clean_unemploy <- lapply(l_state_name, FUN=addSumRateState) %>% bind_rows()

#droping these states from total homeless count because the unemployment 
#does not has data about these states. 
clean_total_homeless_count <- clean_total_homeless_count %>% 
  filter(!State %in% c("AK", "DC", "FL", "GA", "GU", "PR", "VI")) 

#write the whole dataframe to csv for later use - like graphing purpose-------------------------
#homeless df
write.csv(clean_total_homeless_count, 
          "~/Desktop/soc225/Final Project 225/clean_output/homeless-total-output.csv", 
          row.names =FALSE)

#unemploy df 
write.csv(clean_unemploy, 
          "~/Desktop/soc225/Final Project 225/clean_output/unemploy-total-output.csv", 
          row.names =FALSE)

#Join both data---------------------------------------------------------------------------------
#the reason a state-code data frame was create to make sure when merge 
#homeless data and unemploy together, the data will go according to the state
#since the way the data structure is completely different 
#concept for rename(df, new.name = old.name)
state_code <- data.frame(state.code = state.abb, state.nam = state.name) %>% 
  filter(!state.abb %in% c("DC", "FL", "AK", "GA", "GU", "PR", "VI"),
         !state.name %in% c("Florida", "Alaska", "Georgia"))

#for merging purpose
clean_unemploy <- clean_unemploy %>% rename(state.nam = State)
clean_total_homeless_count <- clean_total_homeless_count %>% rename(state.code = State)

#merge homeless data with state code first
full_data <- merge(x= clean_total_homeless_count, y= state_code, all.x=TRUE)
#then merge unemploy after it - to prevent error 
full_data <- merge(x = full_data, y = clean_unemploy) %>% arrange(state.nam)

#merge df 
write.csv(full_data, 
          "~/Desktop/soc225/Final Project 225/clean_output/jointed.csv", 
          row.names =FALSE)

#------------------------------------------------------------------------------------------------
#create a states map for sum of total homeless 2016
# g <- list(scope = 'usa', projection = list(type = 'albers usa'),
#   showlakes = TRUE, lakecolor = toRGB('white'))
# 
# p <- plot_geo(clean_total_homeless_count, 
#               locationmode = 'USA-states') %>% 
#   add_trace(z=~Sum_Total_Homeless, 
#             locations = ~State, color = ~Sum_Total_Homeless) %>%
#   layout(title = "Sum of Total Homeless 2016", 
#     geo = g)
# p

#using for loop to create multiple lists
# for(i in sort(years, decreasing = FALSE)){ #used for loop to create multiple trace
#   val <- (clean_total_homeless_count %>% filter(Year == i))$Sum_Total_Homeless
#   name <- assign(paste("trace", i), val)
# }

#find z-score (val - mean / )



 