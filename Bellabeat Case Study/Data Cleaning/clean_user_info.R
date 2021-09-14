# Clean user_info dataset

# 1.Get an overview of the data structure and its content
user_info <- as.data.frame(read.csv("weight_log_info.csv"))

head(user_info)
str(user_info)

# 1. Check how many Ids are involved in this dataset to see if the number is consistent with other datasets 
# and the dataset is complete.

length(user_info$Id[!duplicated(user_info$Id)])

# Only the information of 8 users are collected in this dataset.

# 2. Check duplicated rows

user_info[duplicated(user_info)]

# No duplicated row in this dataset.

# 3. Check NULL data in the dataset

# From the viewer of the dataset, we can see that out of 67 rows, only 2 rows are complete in every column.
# Other rows don't have the value in Fat column.

user_info %>%
  filter(!is.na(user_info$Fat))

# 4.Check the datetime range of the dataset.

max(mdy_hms(user_info$Date))
min(mdy_hms(user_info$Date))

# The datetime range is this dateset is from 2016-04-12 06:47:11 to 2016-05-12 23:59:59.

# 5. Check how many observations there are for each Id.

user_info %>%
  group_by(Id) %>%
  count()

# Only one Id has 30 observation. One has 24 observations. But other users only provide 1, 2 or 5 observations.
# The dataset is highly incomplete.

# Conclusion:
# The user_info dataset contains the weight, fat and BMI information of 8 Fitbit users in this study.
# But the information is still quite incomplete.