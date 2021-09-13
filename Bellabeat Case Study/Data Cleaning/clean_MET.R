# Clean the dataset minute_MET

# Get an overview of the data structure and its content
minute_MET <- as.data.frame(read.csv("minute_METs_narrow.csv"))

head(minute_MET)
str(minute_MET)

# 1. Check how many Ids are involved in this dataset to see if the number is consistent with other datasets 
# and the dataset is complete.

length(minute_MET$Id[!duplicated(minute_MET$Id)])

# The result returned is 33, which is the correct number.

# 2.Check if there is any NULL data.

minute_MET %>%
  filter(is.na(minute_MET$Id))

minute_MET %>%
  filter(is.na(minute_MET$ActivityMinute))

minute_MET %>%
  filter(is.na(minute_MET$METs))

#No NULL data in this dataset.

# 3.Check the data range of the METs columnn

max(minute_MET$METs)
min(minute_MET$METs)

# The value of the METs column ranges from 0 to 157.

# 3. Since the ActivityMinute column now is in the string format, we need to 
# convert it into a datetime format and figure our the datetime range.

max(mdy_hms(minute_MET$ActivityMinute))
min(mdy_hms(minute_MET$ActivityMinute))

# The minimum datetime is 2016-04-12 00:00:00 and the maximum datetime is 2016-05-12 15:59:00.

# 4. Check how many observations each Id has, whether the dataset is complete.

Rows_per_Id <- minute_MET %>% group_by(Id) %>% count() %>% arrange(n)
View(Rows_per_Id %>% group_by(n) %>% count())

# From the result we can see that users have provided different number of observations.
# The completeness level of this data set is 90.96% (1325580 rows/ (44159 minutes * 33 Ids))

# Conclusion:
# The minute_METs data set contains the observation of the MET value of each of the 33 Fitbit users
# each minute during 2016-04-12 00:00:00 and 2016-05-12 15:59:00.
# The completeness level of this data set is 90.96%

minute_MET_cleaned <- minute_MET %>% arrange(mdy_hms(minute_MET$ActivityMinute)) %>% arrange(Id)
head(minute_MET_cleaned)

write.csv(minute_MET_cleaned, file = "minute_MET_cleaned.csv")


# Fitbit calculates active minutes through a metric called metabolic equivalent of tasks (METs), 
# also sometimes just called metabolic equivalents. 
# METs measure the intensity of a particular physical exercise by comparing against a base rate.

# Create a dataset that counts METs hourly so that it's consistent with other data sets for further analysis.

hourly_MET <- minute_MET_cleaned %>%
  group_by(Id,Date = date(mdy_hms(ActivityMinute)),Hour = hour(mdy_hms(ActivityMinute))) %>%
  summarise(mean(METs))

hourly_MET <- hourly_MET %>%
  unite("Datetime",Date,Hour, sep = " ")

hourly_MET$Datetime <- paste0(hourly_MET$Datetime, ":00:00")

View(hourly_MET)

write.csv(hourly_MET,file = "hourly_MET_cleaned.csv")
