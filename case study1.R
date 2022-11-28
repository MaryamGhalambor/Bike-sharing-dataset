install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

# importing data:
df1 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt110.csv")
df2 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt111.csv")
df3 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt112.csv")
df4 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt201.csv")
df5 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt202.csv")
df6 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt203.csv")
df7 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt204.csv")
df8 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt205.csv")
df9 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt206.csv")
df10 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt207.csv")
df11 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt208.csv")
df12 <- read.csv("C:\\Users\\anita\\Desktop\\data\\dt209.csv")

# combining all 12 files into one data frame:
all_trips <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

colnames(all_trips)

# deletion of unimportant columns:
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

# CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS:
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# how many observation fall under each usertype:
table(all_trips$member_casual)

# add columns to list date, month, day and year of each ride:
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add "ride_length" column to all_trips (inseconds):
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
colnames(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# check if any ride_length value is negative:
any(all_trips$ride_length < 0)

# check how many ride_length value is negative:
install.packages("plyr")
library(plyr)
with(all_trips, count(sign(ride_length)))

# remove rows with negative ride_length:
final_trips <- filter(all_trips, ride_length >= 0)

# check the new data frame:
nrow(final_trips)
any(final_trips$ride_length < 0)

# descriptive analysis:
mean(final_trips$ride_length)   # straight average (total ride length / rides)
median(final_trips$ride_length) # midpoint number in the ascending array of ride lengths
max(final_trips$ride_length)    # longest ride
min(final_trips$ride_length)    # shortest ride
# or:
summary(final_trips$ride_length)

# rename "member_casual" column to "usertype":
install.packages("dplyr")
library(dplyr)
final_trips <- final_trips %>% 
  rename("usertype" = "member_casual")

colnames(final_trips)

# Compare members and casual users:
aggregate(final_trips$ride_length ~ final_trips$usertype, FUN = mean)
aggregate(final_trips$ride_length ~ final_trips$usertype, FUN = median)
aggregate(final_trips$ride_length ~ final_trips$usertype, FUN = max)
aggregate(final_trips$ride_length ~ final_trips$usertype, FUN = min)

# The average ride time by each day for members vs casual:
aggregate(final_trips$ride_length ~ final_trips$usertype + final_trips$day_of_week,
          FUN = mean)

# reorder the days of the week:
final_trips$day_of_week <- ordered(final_trips$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# analyze bike-ride data by usertype and weekday:
final_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(usertype, weekday) %>%                      
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)                           

# visualize the number of rides by usertype:
final_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge")

# create a visualization for average duration:
final_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) + geom_col(position = "dodge")

# TRANSFERRING THE SUMMARY FILE FOR FURTHER ANALYSIS:
counts <- aggregate(final_trips$ride_length ~ final_trips$usertype + final_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'C:\\Maryam Ghalambor\\DATA Analysis\\Coursera\\course8\\case study1.csv')



















