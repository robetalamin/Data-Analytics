###############################################################
#ANALYSIS AND VISUALIZATION
###############################################################

# Loading libraries (set up the environment) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(janitor)
library(readr)
library(skimr)
library(hms)
library(lubridate)



# Import data to use in the project.
# daily
daily_activity <- read_csv("daily_activity.csv")
daily_sleep <- read_csv("daily_sleep.csv")
daily_calories <- read_csv("daily_calories_cleaned.csv")
daily_intensities <- read_csv("daily_intensities.csv")
daily_steps <- read_csv("daily_steps.csv")
# hourly
hourly_calories <- read_csv("hourly_calories_cleaned.csv")
hourly_intensities <- read_csv("hourly_intensities.csv")
hourly_steps <- read_csv("hourly_steps.csv")
hourly_MET <- read_csv("hourly_MET_cleaned.csv")
# User Info
weight_log_info <- read_csv("weight_log_info.csv")


# Transform column name format from pascal_case to snake_case.
daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
daily_calories <- clean_names(daily_calories)
daily_intensities <- clean_names(daily_intensities)
daily_steps <- clean_names(daily_steps)
# hourly
hourly_calories <- clean_names(hourly_calories)
hourly_intensities <- clean_names(hourly_intensities)
hourly_steps <- clean_names(hourly_steps)
hourly_MET <- clean_names(hourly_MET)
# User Info
weight_log_info <- clean_names(weight_log_info)

#1) Find out users' walking trends. Which days do they walk the most?
#Average daily steps grouped by by day of week.
daily_df <- daily_activity %>%
  select(-tracker_distance) %>% 
  mutate(activity_date = mdy(daily_activity$activity_date))
daily_df$day_of_week <- wday(daily_df$activity_date, label = TRUE)
daily_df$week_no <- week(daily_df$activity_date)


average_daily_steps <- group_by(daily_df, day_of_week) %>%
  summarise(average_steps = mean(total_steps))
  
glimpse(average_daily_steps)

#1b)Visualize the weekly average daily steps.
#Plot a bar graph with the data from Sunday to Saturday.
ggplot(average_daily_steps, aes(x = day_of_week, y = average_steps))+
  geom_bar(stat = "identity", 
           fill = "green")+
  geom_text(aes(label = round(average_steps,digits = 0)), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title = "Average daily steps", 
       caption = "Fitbit Fitness Tracker Data")+
  xlab("day of the week")+
  ylab("average steps")+
  ylim(0, 10000)+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))

#TAKEAWAYS
#People walk most on Saturdays closely followed by Tuesdays.
#people walk least on Sundays.
#the average number of steps is 7645
#the number of steps are likely to vary based on ones age, occupation,
#sex, height and stride. 






#2a) Average daily active minutes
#How active did people spend their minutes on average?
#were people very active, fairly active, lightly active or sedentary?

#############steps#####################
#convert the daily df data frame into a long format to have 
#very active, fairly active, lightly active, and sedentary minutes in one
#column called "active_level".
#transfer the values of active_levels into a column named active_minutes. 

# Pivot daily_df
pivot_daily_df <- daily_df %>%
  pivot_longer(c(very_active_minutes, fairly_active_minutes,
                 lightly_active_minutes, sedentary_minutes), 
               names_to = "active_level", values_to = "active_minutes")
# Summarize data
daily_active_df <- select(pivot_daily_df, id, day_of_week,
                          active_level, active_minutes)
average_daily_active_df <- summarise(group_by(daily_active_df, day_of_week,
                                              active_level), 
                                     average_active_minutes = mean(
                                       active_minutes))
#2b) Visualization

average_daily_active_df %>%
  drop_na() %>% 
  ggplot()+
  geom_bar(aes(day_of_week, average_active_minutes, fill = active_level), 
           stat = "identity", position = "stack")+
  labs(title = "How Active Are People In A Day")+
  xlab(NULL)+
  ylab("average active minutes")+
  ylim(0, 1500)+
  scale_color_discrete(name = "Active")+
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))




#2c) visualization comparison
#fairly active vs very active
filter(average_daily_active_df, active_level == "very_active_minutes" 
       |active_level == "fairly_active_minutes") %>% 
  drop_na() %>% 
  ggplot()+
  geom_bar(aes(day_of_week, average_active_minutes, fill = active_level), 
           stat = "identity", position = "stack")+
  labs(title = "How Active Are People In A Day")+
  xlab(NULL)+
  ylab("average active minutes")+
  ylim(0, 70)+
  scale_color_discrete(name = "Active")+
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))



#Hourly Intensities of a day analysis
##############steps##########################
#3a) How is the intensity during each hour of the day?

#use hourly cal, hourly intensity and hourly steps data frames.
#merge the three data frames into one using inner joins.
#merged data is easily accessible and compatible for analysis.
#the three data frames have id and activity hour in common.
#format the activity hour column to mdy_hms date format. Remove AM/PM.
#add new columns of date and time derived from activity_ hour column.


hourly_df <- inner_join(inner_join(hourly_calories, hourly_intensities, by = c(
  "id", "activity_hour")), hourly_steps, by = c(
    "id", "activity_hour"))
hourly_df$activity_hour <- mdy_hms(hourly_df$activity_hour)
hourly_df$date <- as_date(hourly_df$activity_hour)
hourly_df$time <- as_hms(hourly_df$activity_hour)
hourly_df$weekday <- wday(hourly_df$activity_hour, label = TRUE)

#create a new data frame with intensity data grouped by id and date
#create a new column showing days of the week
#regroup the new data frame into weeks
#find the average intensity grouped in each day of the week.


intensity_data <- hourly_df %>% 
  group_by(id,date) %>% 
  drop_na() %>% 
  summarise(sum_total_intensity = sum(total_intensity))
intensity_data$weekday <- weekdays(intensity_data$date)
intensity_weekdays <- intensity_data %>%
  group_by(weekday) %>%
  drop_na() %>%
  summarise(average_intensity = mean(sum_total_intensity))

## Cleaning the format: change data type from character to factor.
intensity_weekdays$weekday <- factor(intensity_weekdays$weekday,
                                     levels = c(
                                       "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


#Confirm the start date, end date , and total Id involved in hourly data
mindate <- min(hourly_df$date)
mindate
maxdate <- max(hourly_df$date)
maxdate
totalid <- n_distinct(hourly_df$id)
totalid

#3b) visualization displaying average intensity during the week 
ggplot(data=intensity_weekdays, aes(x = weekday, y = average_intensity)) +
  geom_col(mapping = aes(x = weekday, y = average_intensity,
                         fill = average_intensity), width = 0.8, 
           position = "dodge")+
  geom_text(aes(label = round(average_intensity,digits = 0)), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title="Intensity of Each Day of the Week",
       caption=paste0("Date from ", mindate," to ",maxdate, "; ", totalid, 
                      " users"),
       x="Day of the week",
       y="Average intensity",
       fill="Intensity")+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))


#TAKEAWAY
#most intensity on Saturday.
#least intensity on Sunday.



#4a)Intensity during the 24 hours analysis
intensity_hours <- hourly_df %>%
  group_by(weekday, time) %>%
  drop_na() %>%
  summarise(mean_total_intensity = mean(total_intensity))

# Grouping to by Day of the weeks:
intensity_week_hour <- intensity_hours %>%
  group_by(weekday) %>%
  drop_na() %>%
  summarise(mean_total_intensity = mean(mean_total_intensity))

#4b) visualization of intensity during the 24 hours of a day
ggplot(data=intensity_hours) +
  geom_col(mapping = aes(x = time, y=mean_total_intensity), fill = "blue") +
  labs(title="Hourly Intensity of a Day",
       caption=paste0("Date from ", mindate," to ",maxdate, ";
                      ", totalid, " users"),
       x="Time(hour)",
       y="Average intensity")+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))


#4c) visualization intensity of each hour of each day of the week. 
ggplot(data = intensity_hours) +
  geom_col(mapping = aes(x = time, y=mean_total_intensity, 
                         fill=mean_total_intensity)) +
  facet_wrap(~weekday) +
  labs(title="Intensity of Each Hour of Each Day of the Week",
       caption=paste0("Date from ", mindate," to ",maxdate, "; ", 
                      totalid, " users"),
       x="Time(hour)",
       y="Average intensity",
       fill="Intensity")



#5a)What was the effect of more steps on calories?
#total steps vs calories burnt analysis. 
steps_vs_calories_df <- daily_df %>% 
  select(id, total_steps, calories) %>% 
  group_by(id) %>% 
  summarise(average_steps = mean(total_steps),
            average_calories = mean(calories))

#5b)steps vs calories Visualization.
ggplot(data = steps_vs_calories_df)+
  geom_point(mapping = aes(x = average_steps, y = average_calories),
             color = "blue")+
  labs(title = "Steps and Calories",
       subtitle = "User average steps vs average calories")+
  ylab("average steps")+
  xlab("average calories")+
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))






#6)How fit are the users?
#6a)Find out what type of users use the fitness devices - Target market
#According to Centers for Disease Control and prevention
#You are under weight if your BMI(Body Mass Index) < 18.5
#Normal weight - B.M.1 is between (18.5 - 24.9)
#Overweight / Obese B.M.I > 30 

#Average B.M.I, Max B.M.I, Min B.M.I 
weight_df <- weight_log_info
weight_df$date <- mdy_hms(weight_df$date)
weight_df$dates <- as_date(weight_df$date)

mean(weight_df$bmi)
max(weight_df$bmi)
min(weight_df$bmi)


Weight_category <- weight_df %>% 
  select(id, bmi) %>% 
  mutate(category = case_when(
    .$bmi >30  ~ "obese",
    .$bmi >25 ~ "over weight",
    .$bmi >18.5 ~ "normal weight",
    .$bmi <18.5 ~ "underweight"
  ))



#A bar plot showing different average user B.M.I quantities
ggplot(data = Weight_category, mapping = aes(x = factor(id), y = bmi,
                                             fill = category))+
  geom_bar(stat = "summary", fun = "mean")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  ggtitle("Users average bmi")+
  ylab("Average B.M.I")+
  xlab("User Id")+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))


#KEY FINDINGS
#People walk most on Saturdays closely followed by Tuesdays.
#people walk least on Sundays.
#the average number of steps is 7645
#the number of steps are likely to vary based on ones age, occupation,
#sex, height and stride. The data should be added fo furhur analysis
#most time is sedentary which we presume they are a working class who do 
#most of their work sit in an office.


#on average, people are more active between 5ppm and 7pm in the evening.
#peoples are least active at night, which we assume they are asleep.
#there is a high intensity at noon meaning people might be exercise
#while going out to look for lunch.

#trends throughout the working weekdays are almost similar
#Monday, Tuesday has people active from 4 to 7 pm
#Saturday has many people active at around 1pm.