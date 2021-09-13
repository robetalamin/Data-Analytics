# Clean the datasets

# Calories
hourly_calories <- read.csv("hourly_calories.csv")
daily_calories <- read.csv("hourly_calories.csv")
daily_calories <- as.data.frame(daily_calories)
hourly_calories <- as.data.frame(hourly_calories)

# 1.Null data
colnames(hourly_calories)

hourly_calories %>%
  filter(is.na(hourly_calories$ActivityHour))

hourly_calories %>%
  filter(is.na(hourly_calories$Id))

hourly_calories %>%
  filter(is.na(hourly_calories$Calories))

# There is no NULL data in hourly_calories.

# 2. See the number of Id 
length(hourly_calories$Id[!duplicated(hourly_calories$Id)])

# 3. Check if the data range is reasonable

min(hourly_calories$Calories)
max(hourly_calories$Calories)

min(hourly_calories$ActivityHour)
max(hourly_calories$ActivityHour)

# 4. check data type
typeof(hourly_calories$ActivityHour)
typeof(hourly_calories$Id)  
typeof(hourly_calories$Calories)

# 5. Transform the data type into a correct data format

library(lubridate)
hourly_calories <- hourly_calories %>% arrange(mdy_hms(hourly_calories$ActivityHour)) %>%
  arrange(Id)

View(hourly_calories)

min(mdy_hms(hourly_calories$ActivityHour))
max(mdy_hms(hourly_calories$ActivityHour))

# 6. See if there's any duplicated row

hourly_calories[duplicated(hourly_calories),]

# There is no duplicated row in this dataset.

# 7. Check how many observations each user has provided.

Id_count <- hourly_calories %>%
  group_by(Id) %>%
  count()

View(Id_count %>% group_by(n) %>% count())

# There are only 6 users who have provided a full set of 736 obsservations. And 6 others provided more than
# 700 observations. 
# The completeness level of this dataset is 90.99 %. (22099 rows/ (33 Ids * 736))

# Now we know that this dataset contains the calories burned of 33 Fitbit users in each hour from 2016-04-12 00:00:00 to
# 2016-05-12 15:00:00.The completeness level of this dataset is 90.99 %.

# To save my cleaned datasets into a CSV file.
write.csv(hourly_calories, file = "hourly_calories_cleaned.csv")

# Now I will perform the same cleaning process for another dataset : daily_calories

# 1.Null data
colnames(daily_calories)

daily_calories %>%
  filter(is.na(daily_calories$ActivityDay))

daily_calories %>%
  filter(is.na(daily_calories$Id))

daily_calories %>%
  filter(is.na(daily_calories$Calories))

# There is no NULL data in daily_calories.

# 2. See the number of Id 

length(daily_calories$Id[!duplicated(daily_calories$Id)])

# we have 33 distinct Id involved in this study, which is consistent with the Id number in other datasets.

# 3. Check if the data range is reasonable

min(daily_calories$Calories)
max(daily_calories$Calories)

min(daily_calories$ActivityDay)
max(daily_calories$ActivityDay)

# 4. check data type
typeof(daily_calories$ActivityDay)
typeof(daily_calories$Id)  
typeof(daily_calories$Calories)

# 5. Transform the data type into a correct data format

library(lubridate)
daily_calories <- daily_calories %>% arrange(mdy(daily_calories$ActivityDay)) %>% arrange(Id)

View(daily_calories)


min(mdy(daily_calories$ActivityDay))
max(mdy(daily_calories$ActivityDay))

# 6.Check how many observations each user has provided.

Id_count_daily <- daily_calories %>%
  group_by(Id) %>%
  count()

View(Id_count_daily %>% group_by(n) %>% count())

# 21 Fitbit users have provided the full set of 31 days of observations.
# The completeness level of this dataset is 91.89%.(940 rows/(33 Ids *31))

# Now we know that this dataset contains the calories burned of 33 Fitbit users on each day 
# from 2016-04-12 to 2016-05-12.The completeness level of this dataset is 91.89%.

write.csv(daily_calories,file = "daily_calories_cleaned.csv")
