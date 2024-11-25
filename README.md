# Cyclistic Bike Share Case Study Capstone Project

## Introduction
Welcome to the Cyclistic bike-share analysis case study! In this case study my role is Junior Data Analyst 
for company Cyclistic, along with some key team members. In order to answer the
business questions, I will follow the steps of the data analysis process: Ask, Prepare, Process,
Analyze, Share, and Act. 
## Background
 Cyclistic, a bike-share company in Chicago that started a bike-share program that features more 
 than 5,800 bicycles and 600 docking stations. Cyclistic sets itself apart by also oering reclining
 bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities
and riders who can’t use a standard two-wheeled bike. The majority of riders opt for
traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more
likely to ride for leisure, but about 30% use the bikes to commute to work each day.

Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to
broad consumer segments. One approach that helped make these things possible was the
flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships.
Customers who purchase single-ride or full-day passes are referred to as casual riders.
Customers who purchase annual memberships are Cyclistic members.

Cyclistic’s finance analysts have concluded that annual members are much more protable
than casual riders. Although the pricing exibility helps Cyclistic attract more customers,
Moreno(the directorof marketing and my manager) believes that maximizing the number of annual
members will be key to future growth. Rather than creating a marketing campaign that targets 
all-new customers, Moreno believes there is a solid opportunity to convert casual riders into
members. She notes that casual riders are already aware of the Cyclistic program and have chosen
Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into
annual members. In order to do that, however, the team needs to beer understand how
annual members and casual riders dier, why casual riders would buy a membership, and how
digital media could aect their marketing tactics. Moreno and her team are interested in
analyzing the Cyclistic historical bike trip data to identify trends

# Approach/Steps
##  Phase1- Ask
Key Questions:

    A. How do annual members and casual riders use Cyclistic bikes dierently?

    B. Why would casual riders buy Cyclistic annual memberships?
 
    C. How can Cyclistic use digital media to inuence casual riders to become members?

 ##  Phase2- Prepare
To prepare for this project I used Cyclistic's historical trip data from year 2022 for my analysis.
The data has been made available under this license by Motivate International Inc.

Cyclistic Trip Data Download https://divvy-tripdata.s3.amazonaws.com/index.html

### Tools:
    * RStudios
    * Tableau
    * GitHub
 ##  Phase3- Process
 I imported the bike data as a csv file to RStudios. I being processing the datasets to prepare
 the data for analysis. I merged the 12 datasets into one dataset and clean it. By removing duplicates, 
 missing values, outliers, inconsistencies and errors in the dataset. I then used 
 Tableau to created visual from the data from RStudios. 

 ##  Phase4 & 5- Analyze & Share
    RStudio: Data Analysis, Data Visualization


##   Phase6- Act
###  Based on insights, here are the top 3 recommendations for Cyclistis:
  1.Creating personalized and targeted digital campaigns is one of the most effective ways to encourage 
     casual riders to become annual members. By leveraging data on their usage patterns and engagement, Cyclistic
     can design marketing messages that speak directly to their needs and habits.

2. A referral program is a powerful way to tap into the existing Cyclistic community and motivate
  current riders to encourage their friends and family to sign up for an annual membership.

3.  Offering time sensitive discounts and exclusive member benefits can create a sense of urgency
  and drive casual riders to take immediate action. These incentives make the membership feel like
   a "special deal" that’s too good to pass up.
![image](https://github.com/user-attachments/assets/a7d4c8e1-cc6e-4b48-9f1d-2b2385e8ae37)

 ## RStudio Programming Codes

 ### Installing and loading the necessary packages
 install.packages("readr")
      
  library(readr)

  install.packages("Tidyverse")

  library(Tidyverse)

  install.packages("ggplot2") 

  library(ggplot2)

  install.packages("dplyr")
  
  library(dplyr)

  install.packages("lubridata")
  
  library(lubridate)

  install.packages("janitor")
  
  library(janitor)

  ### Importing the year long data of bike share

j<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202201-divvy-tripdata”)

f<-read_cvs(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202202-divvy-tripdata”)

m<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202203-divvy-tripdata”)

a<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202204-divvy-tripdata”)

ma<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202205-divvy-tripdata”)

ju<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202206-divvy-tripdata”)

jul<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202207-divvy-tripdata”)

au<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\202208-divvy-tripdata”)

s<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202209-divvy-publictripdata”)

o<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202210-divvy-tripdata”)

n<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\ 202211-divvy-tripdata”)

d<-read_csv(“C:\\Users\\tinar\\Desktop\\Bike Data\\202112-divvy-tripdata")

### Importing the year long data of bikeshare

Total<-rbind(j,f)

Total2<-rbind(m,a,ma,ju,jul,au,s,o,n,d)

### Clean up and remove duplicates

Total1 <- na.omit(Total1)

Total1 <- distinct(Total1)

### Convert to Posixct format

Total$ended_at <- as.POSIXct (Total$ended_at, format = "%m/%d/%Y %H:%M")
Total$started_at <- as.POSIXct (Total$started_at, format = "%m/%d/%Y %H:%M")

### Adding columns for date,month,day,year,day of the week and ride length

Total1$date <- as.Date(Total1$started_at)

Total1$month <- format(as.Date(Total1$date), "%m")

Total1$day <- format(as.Date(Total1$date), "%d")

Total1$year <- format(as.Date(Total1$date), "%Y")

Total1$day_of_week <- format(as.Date(Total1$date), "%A")

### Identify rows where started_at is not less than ended_at

rows_to_replace <- Total1$started_at >= Total1$ended_at

### Replace the values in started_at with the corresponding values in ended_at

Total1$started_at[rows_to_replace] <- Total1$ended_at[rows_to_replace]

### Calculate ride_length in minutes

Total1$ride_length <- as.numeric(difftime(Total1$ended_at, Total1$started_at, units = "mins"))

### Removing ride_length <= 0

Total1_2 <- Total1[Total1$ride_length > 0, ]

### Order the day od the week for clean visualization

Total1_2$day_of_week <- ordered(Total1_2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

sum(is.na(Total1_2$ride_length))

### Basic calculations

mean(Total1_2$ride_length, na.rm = TRUE)

max(Total1_2$ride_length, na.rm = TRUE)

### Length of ride by member type mean calculation

aggregate(Total1_2$ride_length~Total1_2$member_casual, FUN = mean)

### Length of ride by member type median

aggregate(Total1_2$ride_length~Total1_2$member_casual, FUN = median)

### Length of ride by member type max

aggregate(Total1_2$ride_length~Total1_2$member_casual, FUN = max)

### Mean length of ride by member type by day of week

aggregate(Total1_2$ride_length~Total1_2$member_casual+ Total1_2$day_of_week, FUN = mean)

### Maximum length of ride by member type by day of week

aggregate(Total1_2$ride_length~Total1_2$member_casual+ Total1_2$day_of_week, FUN = max)

### Total trips by customer type

Total1_2 %>%

  group_by(member_casual) %>%

  summarize(number_of_rides = n()) %>%

  arrange(member_casual) %>%

  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual))+

  labs(title = "Number of trips by customer type") +

  geom_col(width= 0.5, position = position_dodge(width= 0.5)) +

  scale_y_continuous(labels=function(x) format(x, scientific= FALSE))+

  geom_text(aes(label= number_of_rides), vjust= -0.5)

  ### Average ride length

Total1_2 %>%

  group_by(member_casual) %>%

  summarise(average_ride_length = round(mean(ride_length), 3)) %>%

  ggplot(aes(x = member_casual, y = average_ride_length, fill = member_casual)) +

  labs(title = "Average ride length") +

  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +

  geom_text(aes(label = average_ride_length), vjust = -0.5)

  ### Total trips by customer type Vs Day of the week

  
Total1_2 %>%

  group_by(member_casual, day_of_week) %>%

  summarise(number_of_rides = n()) %>%

  arrange(member_casual, day_of_week) %>%

  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual))+

  labs(title = "Total trips by customer type Vs Day of the week")+

  theme(axis.text.x = element_text(angle = 25))+

  geom_col(width = 0.5, position = position_dodge(width = 0.5))+

  scale_y_continuous(labels= function(x) format(x, scientific = FALSE))

  ### Average trips by customer type by day of the week

  Total1_2 %>%

  group_by(member_casual, day_of_week) %>%

  summarise(average_ride_length = mean(ride_length)) %>%

  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual))+

  labs(title = "Average ride length by customer type Vs Day of the week")+

  theme(axis.text.x = element_text(angle = 25))+

  geom_col(width = 0.5, position = position_dodge(width = 0.5))

  ### Average ride length by customer type Vs Day of the week 

Total1_2 %>%

  group_by(member_casual, day_of_week) %>%

  summarise(average_ride_length = mean(ride_length)) %>%

  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual))+

  labs(title = "Average ride length by customer type Vs Day of the week")+

  theme(axis.text.x = element_text(angle = 25))+

  geom_col(width = 0.5, position = position_dodge(width = 0.5))

  ### Total ride lengths by customer type per month

  Total1_2 %>%

  group_by(member_casual, month) %>%

  summarize(ride_length = n()) %>%

  arrange(member_casual, month) %>%

  ggplot(aes(x= month, y = ride_length, fill = member_casual))+

  labs(title = "Total ride lengths by customer type per month")+

  theme(axis.text.x = element_text(angle = 25))+

  geom_col(width = 0.5, postion = position_dodge(width = 0.5))+

  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))













