## Google Data Analyst Professional Certificate 
## Capstone Project - Case Study 1  

## Loading the relevant libraries 

library(tidyverse)

library(janitor) 

library(lubridate)  

library(dplyr)

library(skimr) 

library(DescTools)

library(geosphere)

library(ggplot2)

## Reading the files 

trips_aug20 <- read_csv("202008-divvy-tripdata.csv")
trips_sep20 <- read_csv("202009-divvy-tripdata.csv")
trips_oct20 <- read_csv("202010-divvy-tripdata.csv")
trips_nov20 <- read_csv("202011-divvy-tripdata.csv")
trips_dec20 <- read_csv("202012-divvy-tripdata.csv")
trips_jan21 <- read_csv("202101-divvy-tripdata.csv")
trips_feb21 <- read_csv("202102-divvy-tripdata.csv")
trips_mar21 <- read_csv("202103-divvy-tripdata.csv")
trips_apr21 <- read_csv("202104-divvy-tripdata.csv")
trips_may21 <- read_csv("202105-divvy-tripdata.csv")
trips_jun21 <- read_csv("202106-divvy-tripdata.csv")
trips_jul21 <- read_csv("202107-divvy-tripdata.csv")

## Inspecting the data 

str(trips_aug20)
str(trips_sep20)
str(trips_oct20)
str(trips_nov20)
str(trips_dec20)
str(trips_jan21)
str(trips_feb21)
str(trips_mar21)
str(trips_apr21)
str(trips_may21)
str(trips_jun21)
str(trips_jul21)

## Checking the columns of the dataset before combining them. The data and the column names have to be similar in order to be combined correctly. 

compare_df_cols(trips_aug20, trips_sep20, trips_oct20, trips_nov20, trips_dec20, trips_jan21, trips_feb21, trips_mar21, trips_apr21, trips_may21, trips_jun21, trips_jul21, return = "mismatch")


## Converting both start_station_id and end_station_id into character 

trips_aug20 <- mutate(trips_aug20, end_station_id = as.character(end_station_id), start_station_id = as.character(start_station_id))
trips_sep20 <- mutate(trips_sep20, end_station_id = as.character(end_station_id), start_station_id = as.character(start_station_id))
trips_oct20 <- mutate(trips_oct20, end_station_id = as.character(end_station_id), start_station_id = as.character(start_station_id))
trips_nov20 <- mutate(trips_nov20, end_station_id = as.character(end_station_id), start_station_id = as.character(start_station_id))


## Comparing once more to double check 

compare_df_cols(trips_aug20, trips_sep20, trips_oct20, trips_nov20, trips_dec20, trips_jan21, trips_feb21, trips_mar21, trips_apr21, trips_may21, trips_jun21, trips_jul21, return = "mismatch")


## Combine all into one large dataframe 

trips_total <- rbind(trips_aug20, trips_sep20, trips_oct20, trips_nov20, trips_dec20, trips_jan21, trips_feb21, trips_mar21, trips_apr21, trips_may21, trips_jun21, trips_jul21)


## Data Cleaning 

## Removing duplicates in data 

trips_totalc <- trips_total[!duplicated(trips_total$ride_id),] 

print(paste("Removed", nrow(trips_total) - nrow(trips_totalc), "duplicated rows"))


## Checking for entries with NA 

sum(!complete.cases(trips_totalc)) 

# Removing data entries with NA 

trips_totalc <- trips_totalc[complete.cases(trips_totalc), ]


## Removing entries with started_at greater than ended_at

trips_totalc <- trips_totalc %>%
  filter(trips_totalc$started_at < trips_totalc$ended_at) 

## Creating new ridetime column 

trips_totalc <- trips_totalc %>%
  mutate(ride_length = (difftime(trips_totalc$ended_at, trips_totalc$started_at)))  

# Convert "ride_length" from Factor to numeric so we can run calculations on the data

is.factor(trips_totalc$ride_length)

trips_totalc$ride_length <- as.numeric(as.character(trips_totalc$ride_length))

is.numeric(trips_totalc$ride_length) 

# Creating ride time (minutes) column

trips_totalc$ride_length_min <- (trips_totalc$ride_length/60)  

trips_totalc$ride_length_min

## Creating separate columns for date elements and Creating day of week column 

trips_totalc$date <- as.Date(trips_totalc$started_at)
trips_totalc$year <- format(as.Date(trips_totalc$started_at), "%Y")
trips_totalc$month <- format(as.Date(trips_totalc$started_at), "%m")
trips_totalc$day <- format(as.Date(trips_totalc$started_at), "%d")
trips_totalc$day_of_week <-paste(format(as.Date(trips_totalc$started_at), "%u"), "-", format(as.Date(trips_totalc$started_at), "%a"))

trips_totalc$day_of_week

## Creating column for start hour of trip (might be useful for analysis)

trips_totalc$start_hour <- format(trips_totalc$started_at, "%H")

trips_totalc$start_hour

## Creating trip distance column  

trips_totalc <- trips_totalc %>%
  mutate(trip_distance_km = (distHaversine(cbind(trips_totalc$start_lat, trips_totalc$start_lng), cbind(trips_totalc$end_lat, trips_totalc$end_lng)))/1000)

trips_totalc$trip_distance_km


## Checking for "bad" data if any 

sum(trips_totalc$start_station_name == "HQ QR")

sum(trips_totalc$ride_length < 0)

## Inspecting the new dataframe  

colnames(trips_totalc)

dim(trips_totalc)

head(trips_totalc)

str(trips_totalc) 

summary(trips_totalc)

skim(trips_totalc)


## Analysis 

## Descriptive Analysis 

## Proportion of each member group in dataset 

trips_totalc %>% 
  group_by(member_casual) %>% 
  summarize(count = n(), 
            percentage = length(ride_id)/nrow(trips_totalc))

## Plot for member proportion 

ggplot(trips_totalc, aes(x=member_casual, fill=member_casual))+
  geom_bar()+
  labs(title="Casuals and Members Distribution", x="Membership Type")

## Descriptive Analysis for ride_length
# Mean ride_length 

trips_totalc %>% 
  summarize(mean(ride_length_min))

# Median ride_length

trips_totalc %>%
  summarize(median(ride_length_min))

# Max ride_length 

trips_totalc %>% 
  summarize(max(ride_length_min))

# Min ride_length 

trips_totalc %>% 
  summarize(min(ride_length_min))

# Alternative method 
summary(trips_totalc$ride_length_min)

# Mode of day_of_week 

Mode(trips_totalc$day_of_week)

## Compare member and casual riders 

aggregate(trips_totalc$ride_length_min ~ trips_totalc$member_casual, FUN = mean) 

aggregate(trips_totalc$ride_length_min ~ trips_totalc$member_casual, FUN = median)

aggregate(trips_totalc$ride_length_min ~ trips_totalc$member_casual, FUN = max) 

aggregate(trips_totalc$ride_length_min ~ trips_totalc$member_casual, FUN = min)

## Average ride_length for member and casual riders 

trips_totalc %>% 
  group_by(member_casual) %>% 
  summarize(mean(ride_length_min))

## Average ride time by each day for member and casual riders 

aggregate(trips_totalc$ride_length_min ~ trips_totalc$member_casual + trips_totalc$day_of_week, FUN = mean) 

trips_totalc %>% 
  group_by(day_of_week, member_casual) %>% 
  summarize(mean(ride_length_min))

# Ridership data by type and weekday 

trips_totalc %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #group by member type and weekday 
  summarise(number_of_rides = n(),  #calculates number of rides 
            average_duration = mean(ride_length)) %>%  #calculate average duration 
  arrange(member_casual, weekday) #sorts

# Visualise number of rides by rider type 

trips_totalc %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge") 

# Visualisation for average duration 

trips_totalc %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge") 


## Further Analysis with other variables 

## Popular stations 

# Start station 
trips_totalc %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n(),  
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) %>% 
  arrange(desc(number_of_rides))

# Popular Start Station for members 
trips_totalc %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n(),  
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) %>% 
  arrange(desc(member_rides))

# Popular Start Station for casuals 
trips_totalc %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n(),  
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) %>% 
  arrange(desc(casual_rides))

# End Station 
trips_totalc %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n(), 
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) %>% 
  arrange(desc(number_of_rides))

# Popular End Station for members 
trips_totalc %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n(),  
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) %>% 
  arrange(desc(member_rides))

# Popular End Station for casuals 
trips_totalc %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n(),  
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) %>% 
  arrange(desc(casual_rides))

## Distribution of ride data by year 

trips_totalc %>% 
  group_by(year) %>% 
  summarise(number_of_rides = n(),
            percentage = (number_of_rides/nrow(trips_totalc)*100), 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100))   


ggplot(trips_totalc, aes(x=year, fill=member_casual))+
  geom_bar()+ 
  labs(title="Distribution by Year") 

## Distribution of ride data by month 

trips_totalc %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(),
            percentage = (number_of_rides/nrow(trips_totalc)*100), 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100))   



ggplot(trips_totalc, aes(x=month, fill=member_casual))+
  geom_bar()+ 
  labs(title="Distribution by Month")

chicago_mean_temp <- c(-4.2, -2.5, 3.6, 9.6, 15.6, 21.1, 23.9, 22.9, 18.9, 11.8, 5.2, -1.5)

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")  

chicago_monthly_data <- data.frame(months, chicago_mean_temp) 

monthly_data <- trips_totalc %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(),
            percentage = (number_of_rides/nrow(trips_totalc)*100), 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100))   
cor(chicago_monthly_data$chicago_mean_temp, monthly_data$number_of_rides)  

## Distribution by day 

trips_totalc %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n(), 
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) 

ggplot(trips_totalc, aes(x=day_of_week, fill=member_casual))+
  geom_bar()+ 
  labs(title="Distribution by Day")

## Distribution by Start Hour 

trips_totalc %>% 
  group_by(start_hour) %>% 
  summarise(number_of_rides = n(), 
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) 

ggplot(trips_totalc, aes(x=start_hour, fill=member_casual))+ 
  geom_bar()+ 
  labs(title="Distribution by Start Hour")  

## Combining both day and start hour 

ggplot(trips_totalc, aes(x=start_hour, fill=member_casual))+ 
  geom_bar()+ 
  labs(title="Distribution by Start Hour and Day")+ 
  facet_wrap(~day_of_week)  

## Distribution by bike type 

trips_totalc %>% 
  group_by(rideable_type) %>%  
  summarise(number_of_rides = n(), 
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) 

ggplot(trips_totalc, aes(x=rideable_type, fill=member_casual))+ 
  geom_bar()+
  labs(title="Distribution by Bike Types")

## Combining both bike type and day of week 

trips_totalc %>% 
  group_by(rideable_type, day_of_week) %>%  
  summarise(number_of_rides = n(), 
            percentage = (number_of_rides/nrow(trips_totalc))*100, 
            member_rides = sum(member_casual == "member"), 
            member_percentage = ((sum(member_casual == "member")/number_of_rides)*100), 
            casual_rides = sum(member_casual == "casual"), 
            casual_percentage = ((sum(member_casual == "casual")/number_of_rides)*100)) 

## Combining bike type, membership and day of week 

ggplot(trips_totalc, aes(x=day_of_week, fill=member_casual))+ 
  geom_bar()+
  labs(title="Distribution within membership and bike type") + 
  facet_wrap(~member_casual + rideable_type)

## Trip Duration 

summary(trips_totalc$ride_length_min) 

# Max value is too large and min value too small, need to check for outliers 

quantile(trips_totalc$ride_length_min) # checking the percentile 

quantile(trips_totalc$ride_length_min, probs = seq(0, 0.05, 0.01)) # breaking down the 1st percentile 

quantile(trips_totalc$ride_length_min, probs = seq(0.95, 1, 0.01)) # breaking down the 95th percentile 

percentile_duration <- quantile(trips_totalc$ride_length_min, probs = seq(0, 1, 0.01))  # saving the percentile values 

# Outlier data might not be informative 
# Removing both 5th and 100th percentiles  

trips_total_no_outliers <- trips_totalc %>% 
  filter(ride_length_min > percentile_duration["5%"]) %>% 
  filter(ride_length_min < percentile_duration["99%"])

num_of_rows_v1 <- nrow(trips_total_no_outliers) # saving the number of rows 

print(paste("Removed", nrow(trips_totalc)-nrow(trips_total_no_outliers), "rows as outliers")) ## Number of rows removed

## Distribution of trip duration by membership 

trips_total_no_outliers %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(ride_length_min), 
            "first_q" = quantile(ride_length_min, 0.25), 
            median = median(ride_length_min), 
            "third_q" = quantile(ride_length_min, 0.75), 
            IQR = third_q - first_q) 

## Plot distribution of trip duration 

ggplot(trips_total_no_outliers, aes(x=member_casual, y=ride_length_min, fill=member_casual))+
  geom_boxplot()+ 
  labs(title = "Distribution of Ride Length by Membership", x="Membership Type", y="Ride Length (Mins)")  

## Distribution of trip duration and day of week  

ggplot(trips_total_no_outliers, aes(x=member_casual, y=ride_length_min, fill=member_casual))+
  geom_boxplot()+ 
  labs(title = "Distribution of Ride Length by Membership", x="Membership Type", y="Ride Length (Mins)")+ 
  facet_wrap(~day_of_week) 

## Trip Duration, Day of Week and Membership 

ggplot(trips_total_no_outliers, aes(x=day_of_week, y=ride_length_min, fill=rideable_type))+ 
  geom_boxplot()+ 
  facet_wrap(~member_casual)


## Trip Duration, Type of Bike and Membership 

ggplot(trips_total_no_outliers, aes(x=rideable_type, y=ride_length_min, fill=member_casual))+ 
  geom_boxplot()+ 
  facet_wrap(~member_casual)

## Trip Distance 

summary(trips_total_no_outliers$trip_distance_km)


# Max value might be too large and min value is too small, need to check for outliers 

quantile(trips_total_no_outliers$trip_distance_km, probs = seq(0, 1, 0.05))  

# Breaking down the values in the 95th-100th percentile range 

quantile(trips_total_no_outliers$trip_distance_km, probs = seq(0.95, 1, 0.01))

# Breaking down the values in the 5th-10th percentile range 

quantile(trips_total_no_outliers$trip_distance_km, probs = seq(0.05, 0.1, 0.01))

# Saving the percentile values 

percentile_dist <- quantile(trips_total_no_outliers$trip_distance_km, probs = seq(0, 1, 0.01))


## Outlier data unlikely to be informative, especially those with 0 as value 
## Remove outlier data 

trips_total_no_outliers <- trips_total_no_outliers %>% 
  filter(trip_distance_km > percentile_dist["10%"]) %>% 
  filter(trip_distance_km < percentile_dist["99%"])

print(paste("Removed", nrow(num_of_rows_v1)-nrow(trips_total_no_outliers), "rows as outliers")) ## Number of rows removed

## Distribution of Trip Distance by membership 

trips_total_no_outliers %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(trip_distance_km), 
            "first_q" = quantile(trip_distance_km, 0.25), 
            median = median(trip_distance_km), 
            "third_q" = quantile(trip_distance_km, 0.75), 
            IQR = third_q - first_q) 

ggplot(trips_total_no_outliers, aes(x=member_casual, y=trip_distance_km, fill=member_casual))+
  geom_boxplot()+ 
  labs(title = "Distribution of Distance by Membership", x="Membership Type", y="Distance (km)")  

## Combining Trip Distance, Day of week and Membership 

ggplot(trips_total_no_outliers, aes(x=day_of_week, y=trip_distance_km, fill=member_casual))+
  geom_boxplot()+ 
  labs(title = "Distribution of Distance by Membership", x="Membership Type", y="Distance (km)")+ 
  facet_wrap(~member_casual)

## Combining Trip Distance, Type of Bike and Membership 

ggplot(trips_total_no_outliers, aes(x=rideable_type, y=trip_distance_km, fill=member_casual))+
  geom_boxplot()+ 
  labs(title = "Distribution of Distance by Membership", x="Membership Type", y="Distance (km)")


## Exporting file for further analysis

alltrips <- trips_total_no_outliers 

write.csv(alltrips, file = "all_trips.csv", row.names = FALSE) 
