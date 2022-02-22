library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)

# importing the divvy_trip_data of whole year 2021
 library(readr)
 X202101_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202101-divvy-tripdata.csv")
X202102_divvy_tripdata_2 <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202102-divvy-tripdata 2.csv")
X202103_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202103-divvy-tripdata.csv")
X202104_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202104-divvy-tripdata.csv")
X202105_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202105-divvy-tripdata.csv")
X202106_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202106-divvy-tripdata.csv")
X202107_divvy_tripdata_2 <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202107-divvy-tripdata 2.csv")
X202108_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202108-divvy-tripdata.csv")
X202109_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202109-divvy-tripdata.csv")
X202110_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202110-divvy-tripdata.csv")
X202111_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202111-divvy-tripdata.csv")
X202112_divvy_tripdata <- read_csv("~/Desktop/cyclist data/Divvy_trip data (20-21)/2021/csv file/202112-divvy-tripdata.csv")
# (Viewing the data),let's check the colnames of each dataset to know more about them
colnames(X202101_divvy_tripdata)
colnames(X202102_divvy_tripdata_2)
colnames(X202103_divvy_tripdata)
colnames(X202104_divvy_tripdata)
colnames(X202105_divvy_tripdata)
colnames(X202106_divvy_tripdata)
colnames(X202107_divvy_tripdata_2)
colnames(X202108_divvy_tripdata)
colnames(X202109_divvy_tripdata)
colnames(X202110_divvy_tripdata)
colnames(X202111_divvy_tripdata)
colnames(X202112_divvy_tripdata)
# applying the summarizing function to know more about the data
str(X202101_divvy_tripdata)
str(X202102_divvy_tripdata_2)
str(X202103_divvy_tripdata)
str(X202104_divvy_tripdata)
str(X202105_divvy_tripdata)
str(X202106_divvy_tripdata)
str(X202107_divvy_tripdata_2)
str(X202108_divvy_tripdata)
str(X202109_divvy_tripdata)
str(X202110_divvy_tripdata)
str(X202111_divvy_tripdata)
str(X202112_divvy_tripdata)
#applying the glimpse function to know more about the data
glimpse(X202101_divvy_tripdata)
glimpse(X202102_divvy_tripdata_2)
glimpse(X202103_divvy_tripdata)
glimpse(X202104_divvy_tripdata)
glimpse(X202105_divvy_tripdata)
glimpse(X202106_divvy_tripdata)
glimpse(X202107_divvy_tripdata_2)
glimpse(X202108_divvy_tripdata)
glimpse(X202109_divvy_tripdata)
glimpse(X202110_divvy_tripdata)
glimpse(X202111_divvy_tripdata)
glimpse(X202112_divvy_tripdata)
# Now we will combine the whole year 2021 data into SINGLE DATA FRAME
all_trip_2021 <- bind_rows(X202101_divvy_tripdata,X202102_divvy_tripdata_2,X202103_divvy_tripdata,X202104_divvy_tripdata,X202105_divvy_tripdata,X202106_divvy_tripdata,X202107_divvy_tripdata_2,X202108_divvy_tripdata,X202109_divvy_tripdata,X202110_divvy_tripdata,X202111_divvy_tripdata,X202112_divvy_tripdata)
View(all_trip_2021)
#STEP2 CLEANING THE DATA( we will use rename function to modify the name of our column for better working experiance)
all_trip_rename <- rename(all_trip_2021, ride_type = rideable_type,customer_type = member_casual)
View(all_trip_rename)
# now we will sort and filter our data using the arrange and filter function
all_trip_sort <- arrange(all_trip_rename,started_at)
View(all_trip_sort)
glimpse(all_trip_sort)
#we will remove the colums which not required or beyond the scope of project
all_trips_2021 <- all_trip_sort %>% 
  select(-c(start_lat:end_lng))
glimpse(all_trips_2021)
#Now we will add new columns that can be used for aggregate function
all_trips_2021$day_of_week <- weekdays(all_trips_2021$started_at)
glimpse(all_trips_2021)
all_trips_2021$month <- format(as.Date(all_trips_2021$started_at),'%b_%y')
glimpse(all_trips_2021)
# let's find the hour,minute and second from date
all_trips_2021$time <- format(all_trips_2021$started_at, format = "%H:%M")
all_trips_2021$time <- as.POSIXct(all_trips_2021$time, format = "%H:%M")
glimpse(all_trips_2021)
# column for trip duration in minutes
all_trips_2021$trip_duration <- (difftime(all_trips_2021$ended_at, all_trips_2021$started_at))/60
glimpse(all_trips_2021)
# Now we willcheck the trip_duration with negative columns and we will remove it
nrow(subset(all_trips_2021,trip_duration < 0))
#checking for testrides that were made by company for quality checks
nrow(subset(all_trips_2021, start_station_name %like% "TEST"))
nrow(subset(all_trips_2021, start_station_name %like% "test"))
nrow(subset(all_trips_2021, start_station_name %like% "Test"))
# in our test we have the output the trip_suration which is less than 0 is 147 elements and the data-set do not have test station_names so it is  0.
# we will remove the trip_duration which are less than 0.
# remove negative trip durations 
all_trips <- all_trips_2021[!(all_trips_2021$trip_duration < 0),]
glimpse(all_trips)
# checking count of  distinct value 
table(all_trips$customer_type)
#defining trip_duration by customer type

setNames(aggregate(trip_duration ~ customer_type, all_trips, sum), c("customer_type", "total_trip_duration"))
# ANALYZING THE TRIPS DATA.
# finding the statistical summary of trip_duration
summary(all_trips$trip_duration)
# let's see statistica; summary of trip_duration
#statistical summary of trip_duration by customer_type
all_trips %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))

# aggregating the data for getting a better overview
# fix the order for the day_of_the_week and month variable so that they show up 
# in the same sequence in output tables and visualizations
all_trips$day_of_the_week <- format(as.Date(all_trips$started_at),'%a')
#column for month when the trip started
#column for month when the trip started
all_trips$month <- format(as.Date(all_trips$started_at),'%b_%y')
#The time is then converted back to POSIXct with today’s date – the date is of no interest to us,
#only the hours-minutes-seconds are.
all_trips$time <- format(all_trips$started_at, format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")
#column for trip duration in min
all_trips$trip_duration <- (difftime(all_trips$ended_at, all_trips$started_at))/60

# check the dataframe
glimpse(all_trips)
# checking for trip lengths less than 0
nrow(subset(all_trips,trip_duration < 0))

#checking for testrides that were made by company for quality checks
nrow(subset(all_trips, start_station_name %like% "TEST"))
nrow(subset(all_trips, start_station_name %like% "test"))
nrow(subset(all_trips, start_station_name %like% "Test"))
# checking count of distinct values
table(all_trips$customer_type)
#aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips, sum), c("customer_type", "total_trip_duration(mins)"))
# Analyze and Visualize the data
# statictical summary of trip_duration for all trips
summary(all_trips$trip_duration)
#statistical summary of trip_duration by customer_type
all_trips %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))
# fix the order for the day_of_the_week and month variable so that they show up 
# in the same sequence in output tables and visualizations
all_trips$day_of_the_week <- ordered(all_trips$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trips$month <- ordered(all_trips$month, levels=c("Jan_21", "Feb_21", "Mar_21", "Apr_21", "May_20", "June_21", "July_21",
                                                           "Aug_21", "Sep_21", "Oct_21", "Nov_21", "Dec_21"))
all_trips %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))
# VISUALIZATION
all_trips %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# finding average number of trip by customer type and month
all_trips %>% 
group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))
# dropping the null values
all_trips_2021 <- drop_na(all_trips)
all_trips_2021 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))
# Visualization( customer type vs month)
all_trips_2021 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#customer type vs on each day_of_the_week
all_trips_2021 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(trip_duration, .groups = 'drop')) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")
#average trip duration by customer time vs month
all_trips_2021 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))
#Visualization of bike_demand over 24 hours period
all_trips_v2 %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")
# Visualization of ride_type vs number of trips by customers
all_trips_2021 %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

