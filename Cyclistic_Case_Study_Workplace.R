install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("janitor")
install.packages("Rcpp")
library(Rcpp)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(janitor)

m09_2020 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202009-divvy-tripdata.csv")
m10_2020 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202010-divvy-tripdata.csv")
m11_2020 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202011-divvy-tripdata.csv")
m12_2020 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202012-divvy-tripdata.csv")
m01_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202101-divvy-tripdata.csv")
m02_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202102-divvy-tripdata.csv")
m03_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202103-divvy-tripdata.csv")
m04_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202104-divvy-tripdata.csv")
m05_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202105-divvy-tripdata.csv")
m06_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202106-divvy-tripdata.csv")
m07_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202107-divvy-tripdata.csv")
m08_2021 <- read.csv("C:/Users/Wel/Desktop/Courses/Case Study/202108-divvy-tripdata.csv")

colnames(m09_2020)
colnames(m10_2020)
colnames(m11_2020)
colnames(m12_2020)
colnames(m01_2021)
colnames(m02_2021)
colnames(m03_2021)
colnames(m04_2021)
colnames(m05_2021)
colnames(m06_2021)
colnames(m07_2021)
colnames(m08_2021)


str(m09_2020)
str(m10_2020)
str(m11_2020)
str(m12_2020)
str(m01_2021)
str(m02_2021)
str(m03_2021)
str(m04_2021)
str(m05_2021)
str(m06_2021)
str(m07_2021)
str(m08_2021)

compare_df_cols(m09_2020, m10_2020, m11_2020, m12_2020, m01_2021, m02_2021, m03_2021, m04_2021,
                m05_2021, m06_2021,m07_2021, m08_2021,  return="mismatch")

m09_2020 <- mutate(m09_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
m10_2020 <- mutate(m10_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
m11_2020 <- mutate(m11_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
head(m12_2020)


total_trips <- bind_rows(m09_2020,m10_2020,m11_2020,m12_2020,m01_2021,m02_2021,
                           m03_2021,m04_2021,m05_2021,m06_2021,m07_2021,m08_2021)
head(total_trips[which(total_trips$started_at == "01-12-2020 00:01"), ])


colnames(total_trips)
dim(total_trips)
head(total_trips)
str(total_trips)


  
#total_trips$started_at <- as_datetime(total_trips$started_at, format = "%d-%m-%Y %H:%M")
#total_trips$ended_at <- as_datetime(total_trips$ended_at,  format = "%d-%m-%Y %H:%M")
head(total_trips)


total_trips$ride_length <- difftime(total_trips$ended_at, total_trips$started_at)
head(total_trips)

total_trips$ride_length <- as.numeric(total_trips$ride_length)

total_trips <- total_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id))

total_trips$date <- as.Date(total_trips$started_at)
total_trips$month <- format(as.Date(total_trips$started_at), "%m")
total_trips$day <- format(as.Date(total_trips$started_at), "%d")
total_trips$year <- format(as.Date(total_trips$started_at), "%Y")
total_trips$day_of_week <- format(as.Date(total_trips$date), "%A")
head(total_trips)

str(total_trips)
dim(total_trips)
colnames(total_trips)
nrow(total_trips)
summary(total_trips)

paste("Number of Rows",nrow(total_trips))
paste("Number of Missing Values", sum(is.na(total_trips)))
total_trips <-total_trips %>%
  drop_na()
paste("Number of Missing Values", sum(is.na(total_trips)))
paste("Number of Rows",nrow(total_trips))


total_trips_v2 <- total_trips[!(total_trips$ride_length<0),]
head(total_trips_v2)

is.factor(total_trips_v2$ride_length)
total_trips_v2$ride_length <- as.numeric(as.character(total_trips_v2$ride_length))
is.numeric(total_trips_v2$ride_length)


mean(total_trips_v2$ride_length) #calculating average ride_length of the user.
median(total_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths

max(total_trips_v2$ride_length) #longest ride
min(total_trips_v2$ride_length) #shortest ride

summary(total_trips_v2$ride_length)


aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN=mean)
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN=median)
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN=max)
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN=min)

aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual+total_trips_v2$day_of_week, FUN=mean)

total_trips_v2$day_of_week <- ordered(total_trips_v2$day_of_week,
                                      levels=c("Sunday","Monday","Tuesday","Wednesday",
                                               "Thursday","Friday","Saturday"))

aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual +
            total_trips_v2$day_of_week, FUN = mean)
tail(total_trips_v2)

total_trips_v2 %>%
  mutate(day_type = ifelse(day_of_week %in% c("Saturday","Sunday"),"Weekend","Weekday")) %>%
  group_by(member_casual, day_type) %>%
  summarize(number_of_rides = n())

rides_per_weekend <- total_trips_v2 %>%
  # create variable to indicate weekend or not (check the weekend day names)
  mutate(day_type = ifelse(day_of_week %in% c("Saturday", "Sunday"), "WEEKEND","WEEK")) %>%
  # build gouping by member type and day type
  group_by(total_trips_v2$member_casual, day_type) %>%
  # summarise total ride length
  summarize(total_ride_length = sum(ride_length, na.rm = TRUE))


total_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n(), #calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sorts


total_trips_v2 %>%
  group_by(member_casual, month) %>%
  arrange(member_casual,month) %>%
  arrange(month, member_casual) %>%
  summarize(number_of_rides = n())


rides_per_month <-total_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>%  
  group_by(member_casual, month) %>%                  
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%          
  arrange(member_casual, month)


top_5_start_stations <- total_trips_v2 %>%
  group_by(member_casual="casual", start_station_name) %>%
  summarize(number_of_rides =n()) %>%
  arrange(desc(number_of_rides)) %>%
  head()

top_5_end_stations<- total_trips_v2 %>%
  group_by(member_casual="casual", end_station_name) %>%
  summarize(number_of_rides =n()) %>%
  arrange(desc(number_of_rides)) %>%
  head()

which(is.na(total_trips_v2$end_station_name), arr.ind=TRUE)

library(scales)
total_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+ scale_y_continuous(labels=comma, name="Number of rides") +
  scale_x_discrete(name="Day of the week") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))        



total_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+ scale_y_continuous(name="Ride Length (in seconds)") +
  scale_x_discrete(name="Day of the week") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))        


#creating table for the top 5 start station used by casual members.
head(total_trips_v2 %>%
       group_by(member_casual="casual", start_station_name) %>%
       summarize(number_of_rides =n()) %>%
       arrange(desc(number_of_rides))) %>%
  ggplot(aes(x = start_station_name, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")  +                             
  coord_flip() + scale_y_continuous(name="Number of rides") +
  scale_x_discrete(name="Start Station") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))            


#Finding number of rides for every month with respect member types:
total_trips_v2 %>%
  mutate(month = month(started_at, label = TRUE)) %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n()) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+ scale_y_continuous(labels=comma, name= "number of rides") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))        


counts <- aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual +
                      total_trips_v2$day_of_week, FUN = mean)
write.csv(counts, "C:/Users/Wel/Desktop/Courses/Case Study/avg_ride_length.csv")
write.csv(top_5_start_stations, "C:/Users/Wel/Desktop/Courses/Case Study/start_stations.csv")
write.csv(top_5_end_stations, "C:/Users/Wel/Desktop/Courses/Case Study/end_stations.csv")
write.csv(rides_per_weekend, "C:/Users/Wel/Desktop/Courses/Case Study/rides_per_weekend.csv")
write.csv(rides_per_month, "C:/Users/Wel/Desktop/Courses/Case Study/rides_per_month.csv")

