# Clean the datasets in the Sleep category

# daily_sleep dataset
# 1.Get an overview of the dataset

str(daily_sleep <- as.data.frame(read.csv("daily_sleep.csv")))

# 2.Check if there is any NULLL data.

daily_sleep %>%
  filter(is.na(daily_sleep))

#No NULL data in this dataset.

# 3.Check the data range or data value of the column TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed.

daily_sleep$TotalSleepRecords[!duplicated(daily_sleep$TotalSleepRecords)]

# There are three values : 1,2,3 in the TotalSleepRecords column

max(daily_sleep$TotalMinutesAsleep)
min(daily_sleep$TotalMinutesAsleep)

# The date range of the TotalMinutesAsleep column is from 58 to 796.

max(daily_sleep$TotalTimeInBed)
min(daily_sleep$TotalTimeInBed)

# The data range if the TotalTimeInBed columnn is from 61 to 961.

# 4.Convert the data format of SleepDay from characters to datetime and check the datetime range.

max(mdy_hms(daily_sleep$SleepDay))
min(mdy_hms(daily_sleep$SleepDay))

# The datetime range of this date set is from 2016-04-12 14:00:00 to 2016-05-12 00:00:00.

# 5.The number of Ids involved.

length(daily_sleep$Id[!duplicated(daily_sleep$Id)])

# The result returned is 24, which means that there are only 24 users out of 33 have recorded their
# daily sleep records.

# 7. See if there is any duplicate row in the dataset.

daily_sleep[duplicated(daily_sleep),]

# There are three duplicate rows in the dataset and we need to delete them.

#          Id         SleepDay         TotalSleepRecords TotalMinutesAsleep TotalTimeInBed
# 162 4388161847  5/5/2016 12:00:00 AM                 1                471            495
# 224 4702921684  5/7/2016 12:00:00 AM                 1                520            543
# 381 8378563200 4/25/2016 12:00:00 AM                 1                388            402

daily_sleep <- unique(daily_sleep)

glimpse(daily_sleep)

# 6.How many observations each user has provided. Check the completeness of the data.

sleep_count <- daily_sleep %>%  
  group_by(Id) %>%
  count()
View(sleep_count) %>% 
  group_by(n) %>% 
  count(name = 'records-of-each-id')


# From the result we can see that only 3 out of 24 users have provided the complete 31 daily sleep records.
# And 9 users have submitted more than 20 but less than 30 records. Others provided less than 20 results.
# Completenes ratio of this dataset is 55.11% (410 observations/24 Ids * 31)


# Conclusion:
# The sleep daily dataset contains the how many times they sleep, how long they stay in bed
# and how long they are really sleeping of each of the 24 Fitbit users each day from 2016-04-12 to 2016-05-12.
# But the completeness of this data set is only around 55.11%.

# minute_sleep dataset

# 1.Get an overview of the dataset

minute_sleep <- as.data.frame(read.csv("minute_sleep.csv"))

str(minute_sleep)
minute_sleep$value[!duplicated(minute_sleep$value)]
head(minute_sleep)

# we can see from the results of the codes above that the minute_sleep dataset contains four 
# columns, which are Id, date, value and logId. And the values in the "value" column is 1,2 and 3.
# But I can't identify the relationship between value and sleep without further explanation of
# the data owner. So I've decided that this dataset won't be used in the analysis this time.