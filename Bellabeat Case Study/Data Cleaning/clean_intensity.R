# Clean the datasets in the Intensity category

# daily_intensities dataset
# 1.Get an overview of the dataset

str(daily_intensities <- as.data.frame(read.csv("daily_intensities.csv")))

# 2.Check if there is any NULLL data.

daily_intensities %>%
  filter(is.na(daily_intensities))

#No NULL data in this dataset.
# 3.See the number of Id

length(daily_intensities$Id[!duplicated(daily_intensities$Id)])

# There are 33 unique ID in the data

daily_intensity <- daily_intensities %>% 
  mutate(Total_min = SedentaryMinutes+LightlyActiveMinutes+FairlyActiveMinutes+VeryActiveMinutes) %>% 
  mutate(Total_distance = SedentaryActiveDistance+LightActiveDistance+ModeratelyActiveDistance+VeryActiveDistance)
daily_intensity %>% 
  summarise(max(Total_distance), min(Total_distance), max(Total_min), min(Total_min))

# The date range of the Total_min column is from 2 to 1440 and Total_distance column from 0 to 28.04

# 4.Convert the data format of ActivityDay to date and check the date range.

max(mdy(daily_intensity$ActivityDay))
min(mdy(daily_intensity$ActivityDay))

# The data range if the ActivityDay columnn is from 4/12/2016 to 5/12/2016.

# 5. See if there is any duplicate row in the dataset.

daily_intensity <- unique(daily_intensity)

glimpse(daily_intensity)

# 6.How many observations each user has provided. Check the completeness of the data.

intensity_count <- daily_intensity %>%  
  group_by(Id) %>%
  count()
View(intensity_count) %>% 
  group_by(n) %>% 
  count(name = 'records-of-each-id') 

# From the result we can see that 21 out of 33 users have provided the complete 31 daily sleep records.
# And 8 users have submitted more than 25 and only 1 user provided less than 18 results.
# Completeness level of this dataset is 91.89 %. (940 rows/ (33 Ids * 31 days))


# Conclusion:
# This intense_daily dataset contains the activity level of 33 users in each day measured by the 
# activity level of the minutes and the distance travelled from April 12 th 2016 to May 12th 2016.
# But this dataset is very incomplete because only around half of the observations have the full record 
# of activity level of the 1440 minutes in a day. And only the observation for one month is recorded.
# Completeness level of this dataset is 91.89 %.


# hourly_intensities dataset

# 1.Get an overview of the dataset

hourly_intensities <- as.data.frame(read.csv("hourly_intensities.csv"))

str(hourly_intensities)
hourly_intensities$AverageIntensity[!duplicated(hourly_intensities$AverageIntensity)]
head(hourly_intensities)

# 2.Check if there is any NULLL data.

hourly_intensities %>%
  filter(is.na(hourly_intensities))

# No NULL data in this dataset.

# 3.See the number of Id

length(hourly_intensities$Id[!duplicated(hourly_intensities$Id)])

# There are 33 unique ID in the data

hourly_intensities %>% 
  summarise(max(TotalIntensity), min(TotalIntensity), max(AverageIntensity), min(AverageIntensity))

# The Maximum and minimum of toal intensity is 180 and 0.
# The Maximum and minimum if Average intensity is 3 and 0.

# 4.Convert the data format of ActivityHour to datetime and check the date range.

max(mdy_hms(hourly_intensities$ActivityHour))
min(mdy_hms(hourly_intensities$ActivityHour))

# The data range if the ActivityHour columnn is from 4/12/2016 00:00:00 UTC to 5/12/2016 15:00:00 UTC.

# 5. See if there is any duplicate row in the dataset.

hourly_intensities <- unique(hourly_intensities)

glimpse(hourly_intensities)

# 6.How many observations each user has provided. Check the completeness of the data.

intensities_count <- hourly_intensities %>%  
  group_by(Id) %>%
  count()
View(intensities_count) %>% 
  group_by(n) %>% 
  count(name = 'records-of-each-id')

# Only 6 users have provided a full set of 736 records,6 others have 735 records. Others have less than that.
# Completeness level of this dataset is 90.99 %. (22099 rows/ (33 Ids * 736))

# Concluson:
# This dataset "intensity_hourly" contains the total intensity per day and the average intensity per minute
# in a day of those 33 fitbit users from 2016-04-12 00:00:00 to 2016-05-12 15:00:00.
# Completeness level of this dataset is 90.99 %.

write.csv(hourly_intensities, file = "hourly_intensities_cleaned.csv")