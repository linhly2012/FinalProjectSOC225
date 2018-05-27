library(dplyr)
library(tidyverse)
#this file is focus on cleanning two data, one is state population from 2010 - 2016
#the other one is population of state with specific location(s) of the state, but state
#total population is also included (the time frame was 2000 - 2010)

#read in data of county level 
state_pop <- read.csv('data/state-pop-10-16/state_est_10-16clean.csv')
county_pop <- read.csv('data/county-00-10/clean-county-00-10.csv')

#data preference note. 
#for some reason, the population on county file in 2010 is different in county population and state file. 
#I chose state file because it seem like it was more updated since it was follow after from 2010 to 2016 and 
#for the county file it was from 2000 - 2010 

#clean county population data, dropped 2001 - 2006 and 2010, because the data
#with homeless and unemployment measure go from 2007 to 2016; also due to the way
#the population data is gathered. Dropped city column because we only look at 
#state level (since the file did included total population of state from 2000 - 2010) . 
c_county <- county_pop %>% select(-c(SUMLEV, REGION, DIVISION, STATE, COUNTY,
                                     ESTIMATESBASE2000, CENSUS2010POP, EST_JUL_2000, 
                                     EST_JUL_2001, EST_JUL_2002, EST_JUL_2003, 
                                     EST_JUL_2004, EST_JUL_2005, EST_JUL_2006, EST_JUL_2010)
                                      ) %>% rename(state = STNAME, city = CTYNAME
                                                   ) %>% filter(city %in% state) %>% select(-city)

#filtered out any state data that was repeated more than one time. 
c_county <- c_county[!duplicated(c_county$state), ]

#dropped Puerto Rico data due to the structure of homeless and unemployment data frames. 
c_state <- state_pop %>% rename(state = GEO.display.label) %>% filter(state != "Puerto Rico")

#merging population measurement together from 2006 - 2009, and 2010 - 2016 from two different 
#data frame. 
full_data <-merge(x = c_county , y =c_state) %>% select(-c(GEO.id, GEO.id2)) %>% 
  filter(!state %in% c("Alaska", "District of Columbia", "Florida", "Georgia"))

#for later graphing purpose
write.csv(full_data, 
          "~/Desktop/soc225/Final Project 225/clean_output/state-pop-07-16.csv", 
          row.names =FALSE)

