# Clean and organize the dataset seconds_heartrate

seconds_heartrate <- as.data.frame(read.csv("seconds_heartrate.csv"))

glimpse(seconds_heartrate)

# 1.Transform the dataset that shows the average heartrate each five seconds into a dataset
# that shows the average heart rate each hour for future analysis.

hour_heartrate <- seconds_heartrate %>% 
  mutate(HOUR = hour(mdy_hms(Time))) %>%
  group_by(Id, Date = date(mdy_hms(Time)), HOUR) %>%
  summarise(avg_hour_heartrate = mean(Value)) %>%
  as.data.frame() %>%
  arrange(Id,Date)


hour_heartrate <- unite(hour_heartrate,"Datetime",Date,HOUR, sep = " ")

hour_heartrate$Datetime <- paste0(hour_heartrate$Datetime,":00:00")

hour_heartrate$avg_hour_heartrate <- round(hour_heartrate$avg_hour_heartrate, digits = 0)

View(hour_heartrate)

# 2. Check NULL data

hour_heartrate[is.null(hour_heartrate),]

# Zero row of null data in the dataset.

# 3.Check if tehre is any duplicated row

hour_heartrate[duplicated(hour_heartrate),]

# Zero duplicated row in the hour_heartrate dataset.

# 4.Check how many unique Ids there are in the dataset.

length(hour_heartrate$Id[!duplicated(hour_heartrate$Id)])

# There are only data from 14 Ids in this dataset,which is incomplete and inconsistent from other datasets.

# Check the data range of the Datetime column and the avg_hour_heartrate column.

max(ymd_hms(hour_heartrate$Datetime))
min(ymd_hms(hour_heartrate$Datetime))

max(hour_heartrate$avg_hour_heartrate)
min(hour_heartrate$avg_hour_heartrate)

# The datetime range of this dataset is from 2016-04-12 00:00:00 to 2016-05-12 16:00:00.
# There is an interval of 736 hours in between.
# The value range of the hour_heartrate column is from 43 to 162.

# 5.Check how many observation each Id has.

hour_heartrate %>%
  group_by(Id) %>%
  count() %>%
  arrange(n)

# From the result we can see that each Id provided different number of observations.
# The completeness level of this dataset is 58.36% (6013 rows/(736 hours *14 Ids).

# Conclusion:
# This hour_heartrate dateset contains the average heart rate per hour of the 14 Fitbit users in this study from
# 2016-04-12 00:00:00 to 2016-05-12 16:00:00. But the dataset is very incomplete. 
# The complete level of this dataset is 58.36%

write.csv(hour_heartrate, file = "hour_heartrate_cleaned.csv")