###                                                                                   ###
### FIRST: Import and source Download_data.R in the same directory before this script ###
###                                                                                   ### 

#########################################################################################

# U.S Accidents (3.0 milion record) 
# A Countrywide Traffic Accident Dataset (2016-2019)
# Downloaded from kaggle: https://www.kaggle.com/sobhanmoosavi/us-accidents
# License: CC BY-NC-SA 4.0
# Only for research and academic purposes
# -----------------------------------------------------------

# Author: Carlos Perez 

# -----------------------------------------------------------


# Install all needed libraries if required

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(stringi)) install.packages("stringi") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(lubridate)) install.packages("lubridate")
if(!require(gtrendsR)) install.packages("gtrendsR")
if(!require(usmap)) install.packages("usmap")
if(!require(caret)) install.packages("caret")
if(!require(scales)) install.packages("scales")
if(!require(reshape2)) install.packages("reshape2")
if(!require(fastDummies)) install.packages("fastDummies")
if(!require(randomForest)) install.packages("randomForest")


# Load needed libraries

library(tidyverse)
library(stringi)
library(kableExtra)
library(gridExtra)
library(lubridate)
library(gtrendsR)
library(usmap)
library(caret)
library(scales)
library(reshape2)
library(fastDummies)
library(randomForest)

 
###########################################################

# This process will take time, dataset occupies 1GB 

accidents <- read.csv("./Data/US_Accidents_Dec19.csv", header = T)


# Funtion to make tables

make_table <- function(data){ 
  kable(data) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  position = "center",
                  font_size = 10,
                  full_width = FALSE)
}

# Structure and dimensions of dataset

dim(accidents)
make_table(head(accidents))

#### DATA PROCESSING ####

names(accidents)

## REMOVE UNNECESSARY COLUMNS ##

# The data was briefly annalized in the data explorer of kaggle
# The next variables are removed due to redundancy, empty columns, constant columns... and also
# because the aim of this project is to make a predictive model that can be used by institutions
# to help them improve management of accidents when they are reported, not after. This is also 
# explained better in the .Rmd file


accidents <- accidents %>% select(-c(End_Lat, End_Lng, Description, Number, Street, City,
                                     Country, Turning_Loop, TMC, Zipcode, ID, County))

# TIME #


# Extract those accidents that did not occurred in 2016, 2017, 2018 and 2019 

accidents$Start_Time %>% year() %>% table()

# inspect those accidents 

accidents[-which(accidents$Start_Time %>% year() %in% c(2016, 2017, 2018, 2019)),]

# remove rows, the one occured in 2015 seems an unbeliable accident

accidents <- accidents[-which(accidents$Start_Time %>% year() %in% c(2015, 2020)),]

# remove those accidents that have a negative time duration

accidents <- accidents[-which(as.numeric(difftime(strptime(accidents$Start_Time, "%Y-%m-%d %H:%M:%S"),
                    strptime(accidents$End_Time, "%Y-%m-%d %H:%M:%S"))) > 0),]

# remove End_Time because it will not be used in further analysis

accidents <- accidents %>% select(-End_Time)

accidents$Start_Time

# Let's see susceptible columns to be removed #

# Points-Of-Interest. The following variables show if there's any near to the accident.

ind <- which(names(accidents) %in% c("Amenity", "Bump", "Crossing", "Give_Way", "Junction", "No_Exit",
                                     "Railway", "Roundabout", "Station", "Stop", "Traffic_Calming", "Traffic_signal"))

prop_POI <- apply(accidents[,ind], 2, function(c) prop.table(table(c))) 

prop_POI

# Many of them have a prevalence under 0.05% and this can be a major problem in ongoing predicitons
# The ones with very low prevalence will be removed

accidents <- accidents %>% select(-c(names(which(prop_POI[2,] < 0.05))))

rm(prop_POI, ind)


## CLEAN CHARACTER VARIBALES ##

# Let's see if character columns are correct

# Source column does not present any extrange label

make_table(table(accidents$Source))


# State names must have the same pattern and length must be 48 (number of
# contiguous states) + District of Columbia (DC)

names_states <- accidents$State %>% levels()

length(names_states) == 49

# Wind_Direction

names_wind <- accidents$Wind_Direction %>%levels()

names_wind

# Some levels are repeated in different formmats (Ex: Calm == CALM)/ the order to change the levels was previously
# taked into account

levels(accidents$Wind_Direction)[names_wind %in% c("Calm", "East", "North", "South", "West", "Variable")] <- 
  c("CALM", "E", "N", "S", "VAR", "W")

# Make sure that are correct

accidents$Wind_Direction %>% levels()

# Wind_speed shows a pattern, it's either words or separated characters by slashes

accidents$Weather_Condition %>% unique()

rm(names_states, names_wind)

## CONVERT EMPTY VALUES IN NAs ## 

# Many columns are factors that take into account empty values as the first level

str(accidents)

# The first factor of Side, Timezone, Airport_code, Weather timesptamp, Wind_direction, Weather condition, 
# Sunrise_Sunset, Civil_Twilight, Nautical_Twilight and Astronomical_Twilight is empty and should be converted
# into NAs


ind <- which(names(accidents) %in% c("Side", "Timezone", "Airport_Code", "Weather_Timestamp", "Wind_Direction", 
                                     "Weather_Condition", "Sunrise_Sunset", "Civil_Twilight", "Nautical_Twilight",
                                     "Astronomical_Twilight"))
for(i in ind){
  
  levels(accidents[,i])[1] <- NA
  
}

rm(ind, i)

## PERCENTAGE OF NAs IN VARIBALES ##

prop_NAs <- apply(accidents, 2, function(c) prop.table(table(is.na(c))))

make_table(unlist(prop_NAs)[unlist(prop_NAs) != 1])

rm(prop_NAs)

# Wind chill an Precipitation has many NAs but NAs in Precipitation does not mean "not recorded"
# it means that there was no rain

accidents <- accidents %>% select(-Wind_Chill.F.)

# CREATE COLUMNS FOR YEAR, MONTH, DAY OF THE WEEK

accidents <- accidents %>% mutate(Year = year(Start_Time), 
                                  Month = month(Start_Time), 
                                  Weekday = factor(as.POSIXlt(Start_Time)$wday), 
                                  Hour = hour(Start_Time))

# change levels of weekday from 1 to 7 starting on Sunday

levels(accidents$Weekday) <- c(7,1,2,3,4,5,6)

# CUT DISTANCE INTO WHOLE MILES #

accidents <- accidents %>% mutate(Distance = Distance.mi. %>% cut(breaks = seq(range(.)[1], range(.)[2] + 1, 1), labels = seq(1, range(.)[2] + 1, 1), include.lowest = T, right = T))

# MAKE SEVERITY A FACTOR

accidents$Severity <- factor(accidents$Severity)

# The final dataset contains 31 variables, 28 were removed

dim(accidents)

make_table(head(accidents))


prop.table(table(accidents$Severity[accidents$State == 'WY']))

#### EXPLORATORY DATA ANALYSIS ####

# The evolution of Severity across time shows that the mean of Severity is near 2 
# and the number of accidents increase over time and also Severity 1 has almost no prevalence

make_table(accidents %>% group_by(Severity) %>% summarise(Count = n()))

p1 <- accidents %>% group_by(Severity) %>% 
  summarise(Count = n()) %>% ggplot(aes(x = Severity, y = Count, fill = Severity)) +
  geom_bar(stat = "identity") + geom_vline(xintercept = mean(as.numeric(accidents$Severity)),
                                           linetype = "dashed", size = 1.2, color = "red") +
  ggtitle("Number of accidents by Severity") 

p2 <- accidents %>% group_by(Severity, Year) %>% 
  summarise(Count = n()) %>% ggplot(aes(x = Year, y = Count, fill = Severity)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of accidents by Year")

grid.arrange(p1, p2, ncol = 2)

rm(p1, p2)

# The following plot shows fluctuations in Severity 2 and 3 over time
# The number of accidents decrease in the first half of every year

p3 <- accidents %>% mutate(Date_week = paste(year(Start_Time), week(Start_Time), sep = "-")) %>% group_by(Date_week, Severity) %>% summarise(Accidents = n()) %>%
  ggplot(aes(Date_week, Accidents, group = Severity, col = Severity)) +
  geom_line(size = 0.8) + theme(axis.text.x = element_blank(),
                                axis.ticks.x = element_blank()) +
  xlab("Time in Weeks") +
  ggtitle("Trends across time in Weeks")

p3

p4 <- accidents %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>%
  ggplot(aes(Month, Count, fill = Month)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = 1:12)+
  facet_grid(~Year)+
  ggtitle("Trend across time in Months")

p4

grid.arrange(p3, p4,ncol = 2)


# The days of the week are also related to the Severity of the accidents 
# Saturday and Sunday have more sever accidents compared to the other days

# Plot mean of Severity by Weekday and Year 

p5 <- accidents %>% filter(!is.na(Weekday)) %>% group_by(Weekday, Year) %>% summarise(Severity = mean(as.numeric(Severity))) %>% 
  ggplot(aes(Weekday, Severity, group = Year, col = Year)) +
  geom_line() +
  geom_point(size = 3) +
  ggtitle("Mean of Severity across Weekday")
  

p6 <- accidents %>% filter(!is.na(Weekday)) %>% group_by(Weekday) %>% summarise(Accidents = n()) %>% 
  ggplot(aes(Weekday, Accidents, col = Weekday, fill = Weekday)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of accidents by Weekday")

grid.arrange(p5, p6, ncol = 2) 


# The hour of the day also affects the number and the Severity of accidents

p7 <- accidents %>% filter(!is.na(Hour)) %>% group_by(Hour) %>%
  summarise(Accidents = n()) %>% ggplot(aes(x = Hour, y = Accidents)) +
  geom_point(col = "salmon2", size = 3) + geom_line(col = "salmon4", size = 1) +
  ggtitle("Number of accidents by Hour")

p8 <- accidents %>% filter(!is.na(Hour)) %>% group_by(Hour) %>%
  summarise(Severity = mean(as.numeric(Severity))) %>% ggplot(aes(x = Hour, y = Severity)) +
  geom_point(col = "salmon2", size = 3) + geom_line(col = "salmon4", size = 1) +
  ggtitle("Severity of accidents by Hour")

grid.arrange(p3, p4, p5, p6, p7, p8, nrow = 3, ncol = 2)

rm(p3, p4, p5, p6, p7, p8)

# Let's see if there is a pattern between Severity and Source.
# Bing and MapQuest-Bing are the ones with less prevalence and this
# affects the severity of accidents across time
 
p9 <- accidents %>% 
  group_by(Month, Source) %>% summarise(Count = n()) %>% 
  ggplot(aes(Month, Count,  fill = Source, col = Source)) +
  geom_bar(stat = "identity", col = "black")+
  xlab("2017")+
  scale_x_continuous(breaks = 1:12)+
  facet_grid(~Source)+
  ggtitle("Number of accidents by Source and Month")

p10 <- accidents %>% 
  group_by(Month, Source) %>% summarise(Severity = mean(as.numeric(Severity))) %>% 
  ggplot(aes(Month, Severity, col = Source)) +
  geom_point(size = 3)+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  scale_y_continuous(limits = c(2,3))+
  geom_hline(yintercept = mean(as.numeric(accidents$Severity)), linetype = "dashed")+
  facet_grid(~Source)+
  ggtitle("Severity of accidents by Source and Month")

grid.arrange(p9, p10, ncol = 2)

rm(p9, p10)


# RELIABILITY OF WEATHER TIMESTAMP #

# let's see if the time when the weather condition was recorded is near the Start_Time
# of te accident

diff_time <- as.numeric(difftime(strptime(accidents$Start_Time, "%Y-%m-%d %H:%M:%S"),
                                 strptime(accidents$Weather_Timestamp, "%Y-%m-%d %H:%M:%S"))) %>% data.frame(Hours = .)/3600 

# The following table shows that many accidents' weather condition were recorded 10 or more hours
# before or after the accident 

range(diff_time$Hours, na.rm = T)

make_table(t(table(round(dAiff_time[diff_time < 6 & diff_time > -6]))))

# Index those rows that its weather condition was recorded one hour before or after
# the accident accured

ind_weather <- diff_time$Hours <= 1 & diff_time$Hours >= -1

rm(diff_time)

# WEATHER CONDITIONS, NUMBER OF ACCIDENTS AND SEVERITY OF ACCIDENTS #

# TEMPERATURE #


# the range of temperatures are impossible, the highest temperature ever recorded 
# was in Death Valley (134 F) and the lowest in Lincoln, Montana (-70 F)
# Source: https://en.wikipedia.org/wiki/U.S._state_and_territory_temperature_extremes

range(accidents$Temperature.F., na.rm = T) # -77.8 to 170.6

# Index plausible Temperatures

ind_Temperature <- accidents$Temperature.F. >= -70 & accidents$Temperature.F. <= 134


# Density plot of temperature to see which are more frequent than others

p22 <- accidents %>%  ggplot(aes(Temperature.F.)) +
  geom_density(fill = 'salmon1', adjust = 1) +
  ggtitle("Number of accidents by Temperature F.") +
  xlab("Temperature")

  # Cut temperature
  
Temp_cat <- cut(accidents$Temperature.F.,
                  breaks = c(seq(-70,134, 5)),
                  labels = c(seq(-70,134, 5)[-1]))
  
  
  p23 <- accidents %>% mutate(Temp_cat) %>% filter(ind_weather & !is.na(Temp_cat)) %>%
  group_by(Temp_cat) %>% summarise(Severity = mean(as.numeric(Severity))) %>% 
  ggplot(aes(Temp_cat, Severity, group = 1))+
  geom_line(col = "salmon4", size = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Severity and Temperature in Fharenheits")+
  xlab("Temperature")

grid.arrange(p22, p23, ncol = 2)

rm(p22, p23)

sum(accidents$Humidity... == 100 & !is.na(accidents$Precipitation.in.), na.rm = T)

# Temperatures seem to follow a normal distribution, and Severity decreases as temperature rises

# HUMIDITY #

# On 6/27/2011, The National Weather Service recorded relative humidity of 
# 1% at McCarren Airport in Las Vegas, Nevada and the highest was reported in the 
# Lawrence Journal-World, in July of 1980. Houston tops the list with an average peak of 89.7 %

ind_Humidity

range(accidents$Humidity..., na.rm = T)

p24 <- accidents[ind_weather,] %>% filter(!is.na(Humidity...)) %>% 
  group_by(Humidity...) %>% summarise(Accidents = n()) %>%
  ggplot(aes(Humidity..., Accidents)) +
  geom_point(col = "salmon3") +
  ggtitle("Number of accidents and Humidity")
  
p25 <-  accidents[ind_weather,] %>% filter(!is.na(Humidity...)) %>% 
  group_by(Humidity...) %>% summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(Humidity..., Severity)) +
  geom_point(col = "salmon3") +
  geom_smooth()+
  ggtitle("Severity and Humidity")


grid.arrange(p24, p25, ncol = 2)

rm(p24, p25)

# Humidity seems to have an interaction with Severity, we have to take into account that the 
# first values of Humidity have almost no observations


# PRESSURE #
# Pressure is  messured in inches Hg
# the lowest and the highest pressure ever recorded were 25.9 and 32.01 inches Hg respectivly
# Source: https://sciencing.com/high-low-reading-barometric-pressure-5814364.html

# Index those Pressure recordings that are possible

ind_Pressure <- accidents$Pressure.in. >= 25.9 & accidents$Pressure.in. <= 32.01

# Plot Pressure

p26 <- accidents[ind_weather & ind_Pressure,] %>% filter(!is.na(Pressure.in.)) %>% group_by(Pressure.in.) %>% summarise(Accidents = n()) %>%
  ggplot(aes(Pressure.in., Accidents)) +
  geom_point(col = "salmon3") +
  scale_y_log10() +
  xlab("Accidents log10")

p27 <- accidents[ind_weather & ind_Pressure,] %>% filter(!is.na(Pressure.in.)) %>% group_by(Pressure.in.) %>% 
  summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(Pressure.in., Severity)) +
  geom_point(col = "salmon3") +
  geom_smooth()

grid.arrange(p26, p27, ncol = 2)

rm(p26, p27)

# Pressure has much more observations around 30 because this is the average pressure 
# and as the numbers of observations decrease the mean of Severity gets more sparse.
# Nevertheless, there's a positive correlation

# Visibility

# Visibility does not explain number of accidents and it also has a plausible range
# Source: https://en.wikipedia.org/wiki/Visibility

range(accidents$Visibility.mi., na.rm = T)

p28 <- accidents[ind_weather,] %>% filter(!is.na(Visibility.mi.)) %>% group_by(Visibility.mi.) %>% summarise(Accidents = n()) %>%
  ggplot(aes(Visibility.mi., Accidents)) +
  geom_point(col = "salmon3") +
  geom_smooth()

p29 <- accidents[ind_weather,] %>% filter(!is.na(Visibility.mi.)) %>% group_by(Visibility.mi.) %>% summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(Visibility.mi., Severity)) +
  geom_point(col = "salmon3") +
  geom_smooth()

grid.arrange(p28, p29, ncol = 2)

rm(p28, p29)

# Visibility does not show a relationship with Severity


# WIND SPEED #

# The range of values shows impossible recordings, wind speed can reach over 480 km/h in extreme tornados but
# in the fastest wind speed ever recorded in a city was in New York in 2010 and this was 201 km/h
# Source: https://www.currentresults.com/Weather-Extremes/US/windiest-cities.php
# Wind speed must be truncated. 124.9 miles per hour are 201 km/h

range(accidents$Wind_Speed.mph., na.rm = T)

# Index plausible wind speeds

ind_Wind <- accidents$Wind_Speed.mph. <= 124.9

# The following plot shows an unbeliable trend, this plot can explain that when speed reaches
# a certain point people don't take the car 

p30 <- accidents[ind_weather & ind_Wind,] %>% filter(!is.na(Wind_Speed.mph.)) %>% group_by(Wind_Speed.mph.) %>% summarise(Accidents = n()) %>%
  ggplot(aes(Wind_Speed.mph., Accidents)) +
  geom_point(col = "salmon3") +
  scale_y_log10()+
  geom_smooth()

p31 <- accidents[ind_weather & ind_Wind,] %>% filter(!is.na(Wind_Speed.mph.)) %>% group_by(Wind_Speed.mph.) %>% 
  summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(Wind_Speed.mph., Severity)) +
  geom_point(col = "salmon3") +
  geom_smooth()


grid.arrange(p30, p31, ncol = 2)

rm(p30, p31)

# Wind Speed also has no relationship with Severity

rm(ind_Wind)

# Precipitation

# Let's first convert empty values into 0 because Precipitation is recorded if it's any amount of rain

accidents$Precipitation.in.[is.na(accidents$Precipitation.in.)] <- 0

# The range is plausible 
# Source: https://en.wikipedia.org/wiki/United_States_rainfall_climatology

range(accidents$Precipitation.in., na.rm= T)

p32 <- accidents[ind_weather,] %>% filter(!is.na(Precipitation.in.)) %>% group_by(Precipitation.in.) %>% summarise(Accidents = n()) %>%
  ggplot(aes(Precipitation.in., Accidents)) +
  geom_point(col = "salmon3") +
  geom_smooth()

p33 <- accidents[ind_weather,] %>% filter(!is.na(Precipitation.in.)) %>% group_by(Precipitation.in.) %>% 
  summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(Precipitation.in.,Severity)) +
  geom_point(col = "salmon3") +
  geom_smooth()

grid.arrange(p32, p33, ncol = 2)

rm(p32, p33)

# Precipitation doesn't show any useful pattern and that is also related to the lack of observations as seen in Visibility
# and Wind speed

# WiIND DIRECTION #

# The following plot shows no interaction with Wind Direction

pwind <- accidents[ind_weather,] %>% filter(!is.na(Wind_Direction)) %>% group_by(Wind_Direction) %>% 
  summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(Wind_Direction, Severity, group = 1))+
  geom_line()+
  geom_point(size = 3, col = "salmon4") +
  scale_y_continuous(limits = c(2,3))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# WEATHER CONDITION

# The following plot shows that most Weather Conditions have a few observations

p34 <- accidents[ind_weather,] %>% filter(!is.na(Weather_Condition)) %>% group_by(Weather_Condition) %>% summarise(Accidents = n()) %>% 
  arrange(desc(Accidents)) %>% head(15) %>%
  ggplot(aes(reorder(Weather_Condition, -Accidents), Accidents))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Number of accidents by weather condition")+
  xlab("Weather Ordered by N accidents ")

p34

# plot mean of Severity and confidence interval 

se <- sapply(c(1:120), function(x){
  
  ind <- as.numeric(accidents$Weather_Condition) == x
  
  (sd(as.numeric(accidents$Severity)[ind & ind_weather], na.rm = T)/sqrt(sum(!is.na(as.numeric(accidents$Severity)[ind & ind_weather])))) * qt(0.975, df = sum(!is.na(as.numeric(accidents$Severity)[ind & ind_weather]))-1)
  
  
})

se <- data.frame(Weather_Condition = levels(accidents$Weather_Condition), Se = se)

p35 <- accidents %>% filter(ind_weather & !is.na(Weather_Condition)) %>% group_by(Weather_Condition) %>% 
  summarise(Severity = mean(as.numeric(Severity)), N = n()) %>%
  inner_join(se, by = "Weather_Condition") %>% 
  ggplot(aes(reorder(Weather_Condition,-Severity), Severity, group=1))+
  geom_line()+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = Severity - Se, ymax = Severity + Se),
                width = 0.2,
                size = 0.75)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0,5))+
  ggtitle("Mean Seveirty and CI by weather condition")+
  xlab("Ordered weather condition")
  

grid.arrange(p34, p35, ncol = 2)


# The previous plots show a lack of observations in the bast majority of weather Conditions
# that affect the mean of severity. The ones that has an standard error bigger than 0.1 are indexed
# in order to avoid future ishues in predictions

ind_names_Weather_Condition <- se$Weather_Condition[se$Se<0.1 & !is.na(se$Se)]

p36 <- accidents %>% filter(ind_weather & !is.na(Weather_Condition) & ind_Weather_Condition) %>% 
  group_by(Weather_Condition) %>% summarise(Severity = mean(as.numeric(Severity)), N = n()) %>%
  inner_join(se, by = "Weather_Condition") %>% 
  ggplot(aes(Weather_Condition, Severity, group=1))+
  geom_line()+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = Severity - Se, ymax = Severity + Se),
                width = 0.2,
                size = 0.75)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(2,4))+
  ggtitle("Mean Seveirty and CI by weather condition")+
  labs(subtitle = "Only weather conditions with small CI")

p36

rm(p34, p35, p36, se)

# EXAMINE NUMBER OF ACCIDENTS AND ACCIDENTS' SEVERITY BY TIMEZONE AND STATE

# Eastern timezone has the highest amount of accidents and also higher Severity on average

p11 <-  accidents %>% filter(!is.na(Timezone)) %>% group_by(Timezone) %>% 
  summarise(Accidents = n(), Severity = as.factor(round(mean(as.numeric(Severity)),2)))  %>% 
  ggplot(aes(reorder(Timezone,-Accidents), Accidents, col = Severity, fill = Severity)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Timezone")+
  ggtitle("Number of accidents by Timezone and their Severity")


# Let's see if the states with more accidents are in Eastern timezone

p12 <- accidents %>% filter(!is.na(Timezone)) %>% group_by(State, Timezone) %>% 
  summarise(Accidents = n()) %>% arrange(desc(Accidents)) %>% head(12) %>%
  ggplot(aes(reorder(State, -Accidents), Accidents, col = Timezone, fill = Timezone))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("States ordered by N accidents")+
  ggtitle("Number of accidents by State")

grid.arrange(p11, p12, ncol= 2)  

rm(p11)

# Here we see that 8 out of 12 states are from Eastern but the State
# with more accidents is California

# Unfortunately there is no data from 2019 of the number of vehicles per state
# The following plot is as the previous ones but the numbers of cars per State are obtained from the average
# of the previous years

means <-  cbind(vehicles_state_2016$total_2016, vehicles_state_2017$total_2017, vehicles_state_2018$total_2018) %>% rowMeans() 

vehicles_state_2019 <- data.frame(State = vehicles_state_2016$State, total_2019 = means)


# Let's see the proportion of accidents by State only in 2017
# The number of accidents are normalized by the number of cars of each State obtained from
# the web of the federal highway administration of US
# The following plot shows that South Carolina has the greatest proportion of accidents 

p13 <- accidents %>% filter(Year == 2016 & ind_Pressure & ind_weather & ind_Temperature) %>% group_by(State) %>% 
  summarise(N_Accidents = n(), Severity = mean(as.numeric(Severity))) %>%
  inner_join(vehicles_state_2016, by = "State") %>% arrange(desc(Severity)) %>% 
  mutate(Percent_Accidents = N_Accidents/total_2016*10^6) %>%
  ggplot(aes(State, Percent_Accidents, label = round(Severity,1))) +
  geom_bar(aes(reorder(State, -Severity), Percent_Accidents, fill = State), stat = 'identity') +
  geom_segment(aes(x = State, y = 0, xend =  reorder(State, -Severity), yend = Percent_Accidents + 500), size = 1) +
  geom_label(aes(State, Percent_Accidents + 500))  +
  ylab("States ordered by rate") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p12, p13, nrow = 2)

# The next plots show that South Carolina is the State with the highest proportion of accidents in 2017 and 2018
# 2016 is not as reliable as the other years due to the smallest amount of accidents present as it is seen in previous plots

p14 <- accidents %>% filter(Year == 2017 & ind_Pressure & ind_weather & ind_Temperature) %>% group_by(State) %>% 
  summarise(N_Accidents = n(), Severity = mean(as.numeric(Severity))) %>%
  inner_join(vehicles_state_2017, by = "State") %>% arrange(desc(Severity)) %>% 
  mutate(Percent_Accidents = N_Accidents/total_2017*10^6) %>%
  ggplot(aes(State, Percent_Accidents, label = round(Severity,1))) +
  geom_bar(aes(reorder(State, -Severity), Percent_Accidents, fill = State), stat = 'identity') +
  geom_segment(aes(x = State, y = 0, xend =  reorder(State, -Severity), yend = Percent_Accidents + 500), size = 1) +
  geom_label(aes(State, Percent_Accidents + 500))  +
  ylab("States ordered by rate") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p15 <- accidents %>% filter(Year == 2018 & ind_Pressure & ind_weather & ind_Temperature) %>% group_by(State) %>% 
  summarise(N_Accidents = n(), Severity = mean(as.numeric(Severity))) %>%
  inner_join(vehicles_state_2018, by = "State") %>% arrange(desc(Severity)) %>% 
  mutate(Percent_Accidents = N_Accidents/total_2018*10^6) %>%
  ggplot(aes(State, Percent_Accidents, label = round(Severity,1))) +
  geom_bar(aes(reorder(State, -Severity), Percent_Accidents, fill = State), stat = 'identity') +
  geom_segment(aes(x = State, y = 0, xend =  reorder(State, -Severity), yend = Percent_Accidents + 500), size = 1) +
  geom_label(aes(State, Percent_Accidents + 500))  +
  ylab("States ordered by rate") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p16 <- accidents %>% filter(Year == 2019 & ind_Pressure & ind_weather & ind_Temperature) %>% group_by(State) %>% 
  summarise(N_Accidents = n(), Severity = mean(as.numeric(Severity))) %>%
  inner_join(vehicles_state_2019, by = "State") %>% arrange(desc(Severity)) %>% 
  mutate(Percent_Accidents = N_Accidents/total_2019*10^6) %>%
  ggplot(aes(State, Percent_Accidents, label = round(Severity,1))) +
  geom_bar(aes(reorder(State, -Severity), Percent_Accidents, fill = State), stat = 'identity') +
  geom_segment(aes(x = State, y = 0, xend =  reorder(State, -Severity), yend = Percent_Accidents + 500), size = 1) +
  geom_label(aes(State, Percent_Accidents + 500))  +
  ylab("States ordered by rate") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p14, p13, p15, nrow = 3)

rm(p12, p13, p14, p15)

# plot heatmap of US States colored by rate of accidents

 p17 <-  accidents %>% filter(State != "DC"  & ind_Pressure & ind_weather & ind_Temperature) %>%
   group_by(State) %>% summarise(Severity = mean(as.numeric(Severity))) %>%
  mutate(fips = fips(State)) %>%
  plot_usmap(data = ., values = "Severity", color = "#003300", labels = F) +
  scale_fill_continuous(low = "white", high = "#003300",
                        name = "Prevalence", label = scales::comma)+
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "Mean Severity")


# You also have to take into account that number of vehicles per state may not be accurate. 

rm(p16, p17, p18, p19, means)

# NUMBER AND SEVERITY OF ACCIDENTS BY SIDE # 

# Side means the relative side in adress field. I suppose that shows the  relative
# side where the acciednts occured.

p37 <- accidents %>% filter(!is.na(Side)& ind_Pressure & ind_weather & ind_Temperature) %>% 
  group_by(Side) %>% 
  summarise(Count = n(), Severity = as.factor(round(mean(as.numeric(Severity)),1))) %>%
  ggplot(aes(Side, Count,  fill = Severity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Number of accidents by Side")

rm(p20)

# Seems logic that the right side has much more accidents because in US they
# drive on the right side

# DISTANCE AND SEVERITY #

# The Distance is cut in whole miles 

p38 <- accidents %>% filter(!is.na(Side)& ind_Pressure & ind_weather & ind_Temperature) %>% 
  mutate(Distance = Distance.mi. %>% cut(breaks = seq(range(.)[1], range(.)[2] + 1, 1), labels = seq(1, range(.)[2] + 1, 1), include.lowest = T, right = T)) %>% 
  group_by(Distance) %>% summarise(mu = mean(as.numeric(Severity)), N = n()) %>% 
  ggplot(aes(Distance, mu)) + 
  geom_point(col = "salmon4") +
  scale_x_discrete(breaks = seq(0,334, 5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Severity and Distance")+
  ylab("Severity")


# POINT OF INTEREST

# We have seen that the prevalences of POI are very low but the mean of Severity
# by Crossing, Junction and Traffic_Signal seem to have an interaction with Severity.
# However, those differences are small

p39 <- melt(accidents %>% filter(!is.na(Side)& ind_Pressure & ind_weather & ind_Temperature) %>% select(Crossing, Junction, Traffic_Signal, Severity), "Severity") %>%
  group_by(variable, value)  %>% 
  summarise(Severity = mean(as.numeric(Severity))) %>%
  ggplot(aes(variable, Severity, group = value, col = value, fill = value)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_y_continuous(limits = c(0,2.5), oob = rescale_none)+
  xlab("POI")+
  ggtitle("Severity by POI")

# SUNRISE SUNSET AND TWILIGHT

# the following plot shows that day does not affect Severity but night does and 
# the differneces in Severity between Day and night are bigger when thay are 
# measured by Astronomical_Twilight

p40 <- melt(accidents %>% filter(!is.na(Side)& ind_Pressure & ind_weather & ind_Temperature) %>% select(Sunrise_Sunset, Civil_Twilight, Nautical_Twilight, Astronomical_Twilight, Severity), "Severity") %>% 
  group_by(variable, value) %>% 
  summarise(Severity = mean(as.numeric(Severity))) %>% filter(!is.na(value)) %>%
  ggplot(aes(variable, Severity, group = value, col = value, fill = value)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_y_continuous(limits = c(2,2.5), oob = rescale_none)+
  xlab(element_blank())+
  ggtitle("Severity by sunlight")


### PREDICTIVE MODEL ###

# The final dataset used for predicitve models will not be the entire dataset, this must be filtered by the indexes created during
# the exploratory analysis, due to computational burden the predicitve models will only be applied to one State 


final_accidents <- accidents %>% filter(ind_Pressure & ind_weather & ind_Temperature & ind_Time &  ind_Weather_Condition & ind_Distance) %>% 
  select(Start_Lat, Start_Lng, Source, Distance, Humidity..., Pressure.in., State,  
         Weather_Condition,  Temperature.F., Timezone, Sunrise_Sunset, Civil_Twilight, Nautical_Twilight, 
         Astronomical_Twilight, Traffic_Signal, Crossing, Junction, Month, Weekday, Hour, Severity)

rm(ind_Distance, ind_Pressure, ind_Temperature, ind_Time, ind_weather, ind_Weather_Condition, accidents)

# The most interesant States were California for having the highest number of accidents but also South Carolina 
# due to the high rate of accidents per number of registered vehicles. 

final_accidents_SC <- final_accidents %>% filter(State == "SC")



# EXAMINE RATIOS OF SEVERITY #

final_accidents_SC %>% group_by(Severity) %>% summarise(N = n(), Percent = n()/nrow(final_accidents_SC)*100) %>% make_table()

# As we have seen in exploratory analysis, Severity 1 and 4 are the ones with lowest prevalence. Moreover, 
# the final dataset used for predictions (South Carolina only) has even lower prevalences and this makes Severity
# 1 and outliers that predictive models will not be able to predict so these two classes shoul be removed.

final_accidents_SC <- final_accidents_SC %>% filter(Severity %in% c(2,3))

# recode levels

final_accidents_SC$Severity <- final_accidents_SC$Severity %>% factor()

# plot severity 

final_accidents_SC %>% ggplot(aes(Start_Lng,  Start_Lat, col = Severity)) +
  geom_point()


# Coherce all factors and character variables into numeric meaningfuly

str(final_accidents_SC)

# Distance must be passed to numeric and also weekday and hour

final_accidents_SC$Distance <- as.numeric(final_accidents_SC$Distance)

final_accidents_SC$Weekday <- as.numeric(as.character(final_accidents_SC$Weekday))

final_accidents_SC$Hour <- as.numeric(as.character(final_accidents_SC$Hour))




# State and Timezone are constants 

final_accidents_SC$Timezone %>% unique()

final_accidents_SC <- final_accidents_SC %>% select(-c(Timezone, State))


# "Night" and "True" will be converted into ones

final_accidents_SC[,c("Sunrise_Sunset", "Civil_Twilight", "Nautical_Twilight", "Astronomical_Twilight")] <- final_accidents_SC %>% select(Sunrise_Sunset, Civil_Twilight, Nautical_Twilight, Astronomical_Twilight) %>% 
  apply(2, function(x) ifelse(x == "Day", 0, 1))

final_accidents_SC[,c("Traffic_Signal", "Crossing", "Junction")] <- final_accidents_SC %>% select(Traffic_Signal, Crossing, Junction) %>%
  apply(2, function(x) ifelse(x, 1, 0))


# Source will be ranked by Severity

final_accidents_SC %>% group_by(Source) %>% summarise(mu = mean(as.numeric(Severity), na.rm = T))

final_accidents_SC$Source <- ifelse(final_accidents_SC$Source == "MapQuest", 2, 
                                    ifelse(final_accidents_SC$Source == "Bing", 1, 3))

# Make weather condition a dummy variable 

final_accidents_SC$Weather_Condition <- factor(final_accidents_SC$Weather_Condition)

final_accidents_SC <- dummy_cols(final_accidents_SC, select_columns = "Weather_Condition", 
           remove_selected_columns = T)

# filter by complete cases

final_accidents_SC <- final_accidents_SC %>% filter(complete.cases(.))

# CHANGE WEATHER CONDITION #

# Predictive models may have problems with Weather Conditions varibles, there are 48 
# with many of them having few cases that makes them uncorrelated with severity, the most important varibles 
# will remain but the other ones will be converted into one called "Other". To know if every variable is related 
# to Severity, Chi-squared tests will be run recursively with every weather condition and Severity

# This function stores the warnings

tryCatch.W.E <- function(expr) # obtained from https://stat.ethz.ch/pipermail/r-help/2010-December/262626.html
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

# recursive Chi squared

Chi_test <- sapply(train_set[,19:66] %>% names(), function(x){
  c(chisq.test(train_set$Severity,train_set[,x])$p.value,
    tryCatch.W.E(chisq.test(train_set$Severity,train_set[,x])))
}) 

# print those variable names with significant p-value and no warning

Chi_test[-2,which(Chi_test[1,] < 0.05) %>% names()] %>% make_table()

# Weather_Condition_Mostly Cloudy / Windy and Weather_Condition_Light Freezing Drizzle have warnings

# COMBINE the weather conditions into one variable #

weather_names <- c(which(Chi_test[1,] > 0.05) %>% names(), "Weather_Condition_Mostly Cloudy / Windy", 
                   "Weather_Condition_Light Freezing Drizzle")

# make sure the variable is binary

rowSums(final_accidents_SC[,weather_names]) %>% unique()

# index weather names that will be converted into "Other"

ind <- names(final_accidents_SC)[which(!names(final_accidents_SC) %in% weather_names)] 

final_accidents_SC <- final_accidents_SC[,ind] %>% 
  mutate(Weather_Condition_Other = rowSums(final_accidents_SC[,weather_names]))

rm(Chi_test, weather_names, ind)


# EXAMINE RATIOS OF SEVERITY #

final_accidents_SC %>% group_by(Severity) %>% summarise(N = n(), Percent = n()/nrow(final_accidents_SC)*100) %>% make_table()

# make test, validation and train set

set.seed(2020)

test_index <- final_accidents_SC$Severity %>% createDataPartition(times = 1, p = 0.1, list = F)

test_set <- final_accidents_SC[test_index,]
train_set <- final_accidents_SC[-test_index,]

validation_index <- train_set$Severity %>% createDataPartition(times = 1, p = 0.1, list = F)

validation_set <- train_set[validation_index,]
train_set <- train_set[-validation_index,]

rm(validation_index, test_index)

# LOGISTIC REGRESSION #

set.seed(2020)

fitControl <- trainControl(method = "cv", number = 10)

log_fit <- train(Severity ~ . , data = train_set, method = "glm", family = binomial(), 
                 trControl = fitControl)

# Asses the best cutoff number for probabilities

y_hat_log <- predict(log_fit, validation_set, type = "prob")

accuracy <- sapply(seq(0.1, 0.99, by = 0.02), function(x){
  
  y_hat <- ifelse(y_hat_log[,"3"] > x, 3, 2) %>% factor(levels = c(2,3))
  
  confusionMatrix(y_hat, validation_set$Severity)$overall[["Accuracy"]]
  
})



plot(seq(0.1, 0.99, by = 0.02),accuracy) 

best_fit_log <- seq(0.1, 0.99, by = 0.02)[which(accuracy == max(accuracy))]

# test set

y_hat_log <- predict(log_fit, test_set, type = "prob")

y_hat <- ifelse(y_hat_log[,"3"] > best_fit_log, 3, 2) %>% factor(levels = c(2,3))

confusionMatrix(y_hat, test_set$Severity)

# the best probability cutoff is 0.54 and it gives a 0.8053 per cent of Accuracy


# DECISION TREE #

x <- train_set %>% select(-c(Severity))

y <- train_set$Severity

train_rpart <- train(x, y, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     trControl = fitControl)

# the accuracy here is much better than in logistic regression

confusionMatrix(predict(train_rpart, test_set, type = "raw"),test_set$Severity)


# RANDOM FOREST #

train_rf <- sapply(c(5,8,10,12,20,30), function(node){
  
  rf_fit <- randomForest(x, y, nodesize = node)
  
  mean(predict(rf_fit, validation_set, type = "response") == validation_set$Severity)
  
})

# the best nodesize is 12

plot(c(5,8,10,12,20,30), train_rf)

# final accuracy

rf_fit <-  randomForest(x, y, nodesize = 12)

confusionMatrix(predict(rf_fit, test_set, type = "response"), test_set$Severity)






