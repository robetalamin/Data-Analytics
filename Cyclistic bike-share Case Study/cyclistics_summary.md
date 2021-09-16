# Summary <a id="summary"></a>

1.  [Ask](#ask)

    1.1. [Questions](#1.1)

    1.2. [Good to know](#1.2)

2.  [Prepare](#prepare)

    2.1. [Metadata](#2.1)

    2.2. [Load R packages](#2.2)

    2.3. [Merge raw data](#2.3)

    2.4. [Organize the data](#2.4)

    2.5. [General check of the data](#2.5)

3.  [Process](#process)

    3.1. [Create new variables](#3.1)

    3.2. [Check the data for misleading information](#3.2)

    3.3. [Clean the data for analysis](#3.3)

4.  [Analyze](#analyze)

    4.1. [Distribution of the trips by periods](#4.1)

    4.2. [Distribution of the trips by length](#4.2)

5.  [Share and Act](#share)

# 

## 1. Ask <a id="ask"></a>

### 1.1. Questions <a id="1.1"></a>

##### [\[Back\]](#summary)

1.  What is the problem that we are trying to solve?

    How to increase the number of annual memberships by converting
    casual riders into annual members. For this, we need to understand
    **how casual riders differs from annual members**.

2.  How can the insights drive business decisions?

    The insights can determine the **strategy behind the marketing
    plan** to increase the number of annual memberships.

3.  Who are the clients/stakeholders of the project?

    The direct clients are the **director of marketing and his
    manager**, and in a broader context, the **executive team** that
    will approve the marketing program.

### 1.2. Good to know <a id="1.2"></a>

##### [\[Back\]](#summary)

-   The company operates in the **Chicago** area, with around 5,800
    bikes and 600 stations

-   **Casual riders** are clients who purchase single-ride or full-day
    passes. The price includes, before charge extra per minute:

    -   single-ride: 30 minutes ride;
    -   full-day: unlimited rides of up to 3h, in 24-hours period.

-   **Annual members** are clients who purchase an annual pass
    membership. The price includes 45 minutes rides before starting to
    charge extra per minute.

## 2. Prepare <a id="prepare"></a>

### 2.1. Metadata <a id="2.1"></a>

##### [\[Back\]](#summary)

-   Source: Motivate International Inc. [Divvy
    Trip](https://divvy-tripdata.s3.amazonaws.com/index.html)
-   License: [public
    dataset](https://www.divvybikes.com/data-license-agreement)
-   Period: from 2020/June to 2021/May (12 months)
-   Extension of archives: *.csv*

### 2.2. Load R packages <a id="2.2"></a>

##### [\[Back\]](#summary)

``` r
# load packages
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.2     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
# set location for date and time standard
Sys.setlocale("LC_TIME", "C")
```

    ## [1] "C"

### 2.3. Merge raw data <a id="2.3"></a>

##### [\[Back\]](#summary)

The data from Motivate International Inc. [Divvy
Trip](https://divvy-tripdata.s3.amazonaws.com/index.html) was stored in
12 csv files, each one corresponding with one month of rides from the
company.

A first check after the download showed that each file has 13 columns
with equal names. However, as some columns could be of different
formats, the data was merged with all columns as characters:

``` r
### Set work directory
setwd("~/GitHub/google-data-analytics-certificate/data")

### Determine files path
files_path <-"~/GitHub/google-data-analytics-certificate/data"

### Merge the data with all columns formatted as character
cyclistic_trip_data <- 
  list.files(files_path, pattern = "*.csv" ) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c"))) 

### Summary check of the data
glimpse(cyclistic_trip_data)
```

    ## Rows: 4,073,561
    ## Columns: 13
    ## $ ride_id            <chr> "8CD5DE2C2B6C4CFC", "9A191EB2C751D85D", "F37D14B0B5~
    ## $ rideable_type      <chr> "docked_bike", "docked_bike", "docked_bike", "docke~
    ## $ started_at         <chr> "2020-06-13 23:24:48", "2020-06-26 07:26:10", "2020~
    ## $ ended_at           <chr> "2020-06-13 23:36:55", "2020-06-26 07:31:58", "2020~
    ## $ start_station_name <chr> "Wilton Ave & Belmont Ave", "Federal St & Polk St",~
    ## $ start_station_id   <chr> "117", "41", "81", "303", "327", "327", "41", "115"~
    ## $ end_station_name   <chr> "Damen Ave & Clybourn Ave", "Daley Center Plaza", "~
    ## $ end_station_id     <chr> "163", "81", "5", "294", "117", "117", "81", "303",~
    ## $ start_lat          <chr> "41.94018", "41.872077", "41.884241", "41.945529", ~
    ## $ start_lng          <chr> "-87.65304", "-87.629543", "-87.629634", "-87.64643~
    ## $ end_lat            <chr> "41.931931", "41.884241", "41.874053", "41.978353",~
    ## $ end_lng            <chr> "-87.677856", "-87.629634", "-87.627716", "-87.6597~
    ## $ member_casual      <chr> "casual", "member", "member", "casual", "casual", "~

### 2.4. Organize the data <a id="2.4"></a>

##### [\[Back\]](#summary)

First, change the format of the columns for each type of information:

``` r
### mutate the format of each column
cyclistic_trip_data<- 
  mutate(cyclistic_trip_data,
         ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type),
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at),
         start_station_name = as.character(start_station_name),
         start_station_id = as.character(start_station_id),
         end_station_name = as.character(end_station_name),
         end_station_id = as.character(end_station_id),
         start_lat = as.double(start_lat),
         start_lng = as.double(start_lng),
         end_lat = as.double(end_lat),
         end_lng = as.double(end_lng),
         member_casual =as.character(member_casual))
```

Then, rename columns for better description of the data:

``` r
### rename some columns
cyclistic_trip_data <-
  rename(cyclistic_trip_data,
         trip_id = ride_id,
         bike_type = rideable_type,
         started_time = started_at,
         ended_time = ended_at,
         user_type = member_casual)
```

Since we want to investigate the difference between the users, it will
not be necessary to analyze latitudes and longitudes. Also, we chose to
sort the information by the start date and time of the trip:

``` r
### exclude unnecessary information (latitudes and longitudes)
### sort the data by the start date and time of the trip
cyclistic_trip_data <-
  arrange(
    select(cyclistic_trip_data, -start_lat, -start_lng, -end_lat, -end_lng),
    started_time)
```

### 2.5. General check of the data <a id="2.5"></a>

##### [\[Back\]](#summary)

Check period of the imported data:

``` r
### check months imported from data
count(cyclistic_trip_data, format(started_time, '%Y-%m'))
```

    ## # A tibble: 12 x 2
    ##    `format(started_time, "%Y-%m")`      n
    ##    <chr>                            <int>
    ##  1 2020-06                         343005
    ##  2 2020-07                         551480
    ##  3 2020-08                         622361
    ##  4 2020-09                         532958
    ##  5 2020-10                         388653
    ##  6 2020-11                         259716
    ##  7 2020-12                         131573
    ##  8 2021-01                          96834
    ##  9 2021-02                          49622
    ## 10 2021-03                         228496
    ## 11 2021-04                         337230
    ## 12 2021-05                         531633

Check types of users and bikes:

``` r
### check users
distinct(cyclistic_trip_data, user_type)
```

    ## # A tibble: 2 x 1
    ##   user_type
    ##   <chr>    
    ## 1 member   
    ## 2 casual

``` r
### check bikes
distinct(cyclistic_trip_data, bike_type)
```

    ## # A tibble: 3 x 1
    ##   bike_type    
    ##   <chr>        
    ## 1 docked_bike  
    ## 2 electric_bike
    ## 3 classic_bike

## 3. Process <a id="process"></a>

### 3.1. Create new variables <a id="3.1"></a>

##### [\[Back\]](#summary)

For this, first we create a new data set called *seasons*:

``` r
### seasons of the year
seasons <- data.frame(season = c(rep("Winter", 3), rep("Summer", 3), 
                                rep("Spring", 3), rep("Fall", 3)),
                     month = c('12','01','02', '06', '07', '08', 
                               '03', '04', '05', '09', '10', '11'))
```

Then, we create new variables to help us analyse the data:

``` r
### create new variables
cyclistic_trip_data <- 
  distinct(
    mutate(cyclistic_trip_data,
           trip_length = as.double(ended_time - started_time),
           year_month = format(started_time, '%Y-%m'),
           month = format(started_time, '%m'),
           week_day = wday(started_time, label = TRUE),
           day_type = 
             ifelse(week_day == 'Sat' | week_day == 'Sun', 
                    'Weekend',
                    'Working day'),
           start_hour = format(started_time, '%H'),
           check_length = 
             ifelse(trip_length < 0, "neg.", "pos.")
           ))

### identify the season that the trip occurred
cyclistic_trip_data$season <- 
  seasons$season[match(cyclistic_trip_data$month, seasons$month)]
```

### 3.2. Check the data for misleading information <a id="3.2"></a>

##### [\[Back\]](#summary)

#### Negative length

First, let’s see if there are trips with negative length:

``` r
### check negative length trips
count(cyclistic_trip_data, check_length)
```

    ## # A tibble: 2 x 2
    ##   check_length       n
    ##   <chr>          <int>
    ## 1 neg.           10336
    ## 2 pos.         4063225

Since there are trips with negative length, let’s see if they are
relevant in a specific period:

``` r
### distribution of negative trip_length
ggplot(cyclistic_trip_data, aes(fill= check_length, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-11-1.png)

The conclusion is that the quantity is not relevant in any month, so we
can delete those trips from the database.

#### User type

Now, let’s understand the distribution of users:

``` r
### distribution of trips per users
ggplot(cyclistic_trip_data, aes(x = user_type, fill = user_type)) +
  geom_bar()
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-12-1.png)
We can see that, in the period considered, there were more trips from
members than from casual riders. We can understand the behavior per
month as well:

``` r
### distribution of trips per user per month
ggplot(cyclistic_trip_data, aes(x = year_month, fill= user_type)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-13-1.png)

In this case, we can see a lot of patterns that we can use for analysis.
However, the data doesn’t seem to demonstrate any error at this stage.

#### Bike type

One step further, we can also understand if we can use the bike type for
analysis:

``` r
### distribution of trips per bike
ggplot(cyclistic_trip_data, aes(x = bike_type, fill = bike_type)) +
  geom_bar()
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-14-1.png)

It seems that the bike type *“docked bike”* is the popular one. Let’s
verify the information by month:

``` r
### distribution of trips per bike per month
ggplot(cyclistic_trip_data, aes(x = year_month, fill= bike_type)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-15-1.png)

From the graph, we’re able to see that the *“electric bike”* began to be
used in August 2020 and the *“classic bike”* only in December 2020.
Also, the number of trips with the *“docked bike”* decreased
drastically, indicating two possibilities: \* the company might have
change the rules to store the data; \* or there might happened some
business decision that changed the data set.

Consequently, it would not be wise to use the information presented
about bike types in this analysis.

### 3.3. Clean the data for analysis <a id="3.3"></a>

##### [\[Back\]](#summary)

From what we discovered before, we can safe that it is safe to delete
some information from the data base, since we are not going to use it
for analysis: \* trips with negative length; \* the *“bike_type”*
information; \* trips that started in the “HQ QR” station.

The last one is due to quality assurance trips from the company.

``` r
### delete irrelevant data for analysis
cyclistic_trip_data <-
  select(
    filter(cyclistic_trip_data, 
           trip_length >= 0, start_station_name != "HQ QR"), 
    -bike_type, -month, -check_length)
```

## 4. Analyze <a id="analyze"></a>

### 4.1. Distribution of the trips by periods <a id="4.1"></a>

##### [\[Back\]](#summary)

#### Trips per season

Let’s understand the general distribution of the trips per season

``` r
### trips per season
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = season, fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = 'Season', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the trips by season')
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_season.png")
```

    ## Saving 7 x 5 in image

Here, we can see that the number of trips has its lowest point during
the winter, where predominantly the trips are made by members, and
highest in the summer. This can suggest that the number of trips can be
related with the weather in the Chicago area.

#### Trips per month

Now, let’s see the distribution of trips per month and compare it with
the average temperature in the Chicago area. For this, first we need to
upload the historical temperatures:

``` r
### average temperatures in the Chicago area
### font: 
chicago_temp <- data.frame(year_month = 
                             c('2020-06', '2020-07', '2020-08', 
                               '2020-09', '2020-10', '2020-11', 
                               '2020-12', '2021-01', '2021-02', 
                               '2021-03', '2021-04', '2021-05'), 
                           temperature = 
                             c(74.0, 79.2,  76.8,   66.3, 51.5, 47.4,   
                               32.8, 29.2,  20.2,   44.2, 51.9, 60.2))
```

Now, let’s compare:

``` r
### comparison of the number of trips and average temperature per month
ggplot() + 
  geom_bar(mapping = aes(x = cyclistic_trip_data$year_month, 
                         fill = cyclistic_trip_data$user_type),
           position = 'stack') +
  geom_point(mapping = aes(x = chicago_temp$year_month, 
                           y = chicago_temp$temperature * 8000)) +
  scale_y_continuous(name = "Number of Trips", 
                     labels = unit_format(unit = "K", scale = 1e-3),
                     sec.axis = sec_axis(~./8000, name = "Temperature (°F)")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  labs(x = 'Months', fill = 'User type', 
       title = 'Distribution of the trips vs average temperature in Chicago')
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_vs_temp.png")
```

    ## Saving 7 x 5 in image

As we suspected, the number of trips decreases with the decrease in the
temperature of the city. Also, we can see that the proportion of casual
rides per month increases with the temperature, suggesting our first
difference between the two types of users.

So, members have a tendency of using the bikes even when the temperature
is low, differently from the casual riders that prefer to use the bikes
when the temperature is higher. This can directly impact the revenue
during colder months of the year since the revenue from casual riders
decrease drastically in this period.

#### Trips per day of the week

We can go even further and analyse the behavior for different days of
the week:

``` r
### trips per day of the week
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = week_day, 
                         fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Weekday', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the trips by weekday')
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_weekday.png")
```

    ## Saving 7 x 5 in image

From this graph, we can’t extract many insights other than the number of
trips during the weekend is higher than in the working days. So, let’s
divide this information by season to see if we find any trend:

``` r
### trips per day of the week by season
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = week_day, 
                         fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Weekday', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the trips by weekday per season') +
  facet_wrap(~season)
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_weekday_season.png")
```

    ## Saving 7 x 5 in image

In this view, it’s clear that members predominantly use the bikes in the
winter. Also, we can see that the variation between weekend and working
days is most due to an increase in casual rides rather than member ones.
In consequence, we have our second difference between the users:
overall, members tend to use the bikes in the same amount all the days
of the week while casual users increase the use during weekends.

#### Trips per hour of the day

If we take our analysis to a last step, we can also see how the number
of trips varies through the day:

``` r
### trips per hour of the day
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), 
           position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the trips by start hour and day type') +
  facet_wrap(~day_type, ncol = 1)
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_hour.png")
```

    ## Saving 7 x 5 in image

From the graph, we can see that: \* the distribution of the trips during
the weekends is more consistent through the day, between 10am and 8pm;
\* the distribution during working days varies with a smooth peak in the
morning, between 7am and 9am, and a huge peak by the end of the day,
between 4pm and 8pm; \* in the weekends the proportion between members
and casual riders is more equilateral, differently from the working days
where we can see a predominance of member’s rides.

These observations can indicate that during working days, we have more
rides following the general working schedule of the users, especially
for members. This can lead to our third difference, because due to the
difference in the distribution, one might suggest a bigger variety of
motives for the casual rides than for the member ones.

We can also see if the distribution varies with seasons:

``` r
### trips per hour of the day per season
cyclistic_trip_data %>%
  filter(day_type == 'Weekend') %>%
  ggplot() +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), 
           position = 'stack') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 6),
        legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the weekend\'s trips by start hour per season') +
  facet_wrap(~season)
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_weekend_season.png")
```

    ## Saving 7 x 5 in image

``` r
### trips per hour of the day per season
cyclistic_trip_data %>%
  filter(day_type == 'Working day') %>%
  ggplot() +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), 
           position = 'stack') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 6),
        legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the working days trips by start hour per season') +
  facet_wrap(~season)
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_workday_season.png")
```

    ## Saving 7 x 5 in image

As we can see, the trend described before between weekends and working
days is the same for all seasons.

### 4.2. Distribution of the trips by length <a id="4.2"></a>

##### [\[Back\]](#summary)

First, let’s recall the rules for “free” rides (rides without extra
charge per minute) for members and casual riders: \* Members: 45 minutes
\* Casual riders: \* Single ticket: 30 minutes \* Day trip: 3 hours

With this information, let’s understand how the length of the trips
varies between members and casual riders.

``` r
### average of the trips by user type
cyclistic_trip_data %>%
  group_by(user_type) %>%
  summarise(mean(trip_length)/60)
```

    ## # A tibble: 2 x 2
    ##   user_type `mean(trip_length)/60`
    ##   <chr>                      <dbl>
    ## 1 casual                      43.9
    ## 2 member                      15.6

First, the average of the trips are 44 minutes for casual riders and 17
minutes for members.

If we plot the quartile for trips with less than three hours, we will
have:

``` r
### quartiles of the trips length with less than 3h
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = user_type, y = trip_length/60, 
                             fill = user_type)) +
  labs(x = 'User type', y = 'Trip length (minutes)',
       title = 'Trip length per user type - trips less than 3h') +
  theme(legend.title = element_blank(), legend.position = 'none')
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/boxplot_3h.png")
```

    ## Saving 7 x 5 in image

Here we can see that the majority of trips are less than 25 minutes for
members, while for casual riders the distribution is larger. Let’s see
this information in a bar chart:

``` r
### cutting the data into trip_length groups of 15 minutes
cyclistic_trip_data$trip_length_group <-
  cut(cyclistic_trip_data$trip_length, 
      c(0, 900, 1800, 2700, 3600, 10800, max(cyclistic_trip_data$trip_length)))

levels(cyclistic_trip_data$trip_length_group) = 
  c('0m - 15m', '15m - 30m','30m - 45m','45m - 1h', '1h - 3h', '>3h')

### bar plot of the trips
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = trip_length_group, fill = user_type), 
           position = 'dodge') +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  labs(x = 'Trip length', y = 'Number of trips', fill = 'User type',
       title = 'Distribution of the trips by duration')
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/trips_duration.png")
```

    ## Saving 7 x 5 in image

From the graph we understand that the majority of the member’s trips
fall in less than 30 minutes, with a concentration in trips with less
than 15 minutes. For the casual rides, we also see a concentration in
trips up to 30 minutes, but we can also see a concentration of trips
between 1h and 3h.

Although we could say that members use the bikes to run errands and
short trips in their routines, which can be different from casual
clients, we can’t analyse the information further because we neither
have the motives of the trips nor the type of ticket that the client
bought for the trip. But, the distribution is in accordance with the
limits of “free” rides for each type of user.

Also, we can see that trips with more than three hours represents a
minority in the data base.

We can also analyse the difference in the length of trips per season,
weekday and hour of the day:

``` r
### average of the trips by season
cyclistic_trip_data %>%
  group_by(user_type, season) %>%
  summarise(mean(trip_length)/60)
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the `.groups` argument.

    ## # A tibble: 8 x 3
    ## # Groups:   user_type [2]
    ##   user_type season `mean(trip_length)/60`
    ##   <chr>     <chr>                   <dbl>
    ## 1 casual    Fall                     35.6
    ## 2 casual    Spring                   39.9
    ## 3 casual    Summer                   52.2
    ## 4 casual    Winter                   32.3
    ## 5 member    Fall                     14.7
    ## 6 member    Spring                   14.6
    ## 7 member    Summer                   17.6
    ## 8 member    Winter                   13.9

``` r
### average of the trips by weekday
cyclistic_trip_data %>%
  group_by(user_type, week_day) %>%
  summarise(mean(trip_length)/60)
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the `.groups` argument.

    ## # A tibble: 14 x 3
    ## # Groups:   user_type [2]
    ##    user_type week_day `mean(trip_length)/60`
    ##    <chr>     <ord>                     <dbl>
    ##  1 casual    Sun                        50.1
    ##  2 casual    Mon                        43.5
    ##  3 casual    Tue                        39.1
    ##  4 casual    Wed                        39.5
    ##  5 casual    Thu                        41.1
    ##  6 casual    Fri                        41.7
    ##  7 casual    Sat                        45.6
    ##  8 member    Sun                        17.6
    ##  9 member    Mon                        15.0
    ## 10 member    Tue                        14.7
    ## 11 member    Wed                        14.9
    ## 12 member    Thu                        14.7
    ## 13 member    Fri                        15.3
    ## 14 member    Sat                        17.1

``` r
### average of the trips by hour
cyclistic_trip_data %>%
  group_by(user_type, start_hour) %>%
  summarise(mean(trip_length)/60)
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the `.groups` argument.

    ## # A tibble: 48 x 3
    ## # Groups:   user_type [2]
    ##    user_type start_hour `mean(trip_length)/60`
    ##    <chr>     <chr>                       <dbl>
    ##  1 casual    00                           61.1
    ##  2 casual    01                           71.4
    ##  3 casual    02                           75.1
    ##  4 casual    03                           91.6
    ##  5 casual    04                           95.8
    ##  6 casual    05                           46.9
    ##  7 casual    06                           35.3
    ##  8 casual    07                           27.5
    ##  9 casual    08                           30.2
    ## 10 casual    09                           38.9
    ## # ... with 38 more rows

In all the views, per season or per weekday or per hour, we can see that
the average duration for casual rides varies a lot more than for
members. This can reinforce the argument where the motive for the trip
is different for casual riders and members and can vary more for casual
clients. But it is and insight that should be validated with more
information, not available in this data set.

However, we can deny this huge difference between the users and it is
something to consider into the action plan for the marketing team.

Also, we can prove an expected tendency for shorter trips during periods
of low temperature (winter), as well as longer trips in the weekends for
both types of users.

We can also see this information distributing the trips into quartiles:

``` r
### quartiles of the trips length with less than 3h
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = season, y = trip_length/60, 
                             fill = season)) +
  labs(x = 'Seasons', y = 'Trip length (minutes)',
       title = 'Trip length per user type - trips less than 3h - per season') +
  theme(legend.title = element_blank(), legend.position = 'none') +
  facet_wrap(~user_type)
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/boxplot_3h_season.png")
```

    ## Saving 7 x 5 in image

``` r
### quartiles of the trips length with less than 3h
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = week_day, y = trip_length/60, 
                             fill = week_day)) +
  labs(x = 'Weekday', y = 'Trip length (minutes)',
       title = 'Trip length per user type - trips less than 3h - per weekday') +
  theme(legend.title = element_blank(), legend.position = 'none') +
  facet_wrap(~user_type, ncol = 1)
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/boxplot_3h_weekday.png")
```

    ## Saving 7 x 5 in image

``` r
### quartiles of the trips length with less than 3h
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = reorder(start_hour,desc(start_hour)),
                             y = trip_length/60, fill = start_hour)) +
  labs(x = 'Seasons', y = 'Trip length (minutes)',
       title = 'Trip length per user type - trips less than 3h - per hour') +
  theme(legend.title = element_blank(), legend.position = 'none') +
  facet_wrap(~user_type) +
  coord_flip()
```

![](project_documentation_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
ggsave("~/GitHub/google-data-analytics-certificate/graphs/boxplot_3h_hour.png")
```

    ## Saving 7 x 5 in image

## 5. Share and Act <a id="share"></a>

##### [\[Back\]](#summary)

The step-by-step of the project can be seen in this document. To sum up
the findings, we have:

1.  in the period analysed, the number of members trips were higher than
    casual ones;

2.  the number of trips and the duration of the trips can be influenced
    by the weather, since we could see big differences from colder
    months to hotter ones and between the seasons, especially for casual
    rides;

3.  we also could see a difference between days of the week and hour of
    the day, with different profiles for the distribution of the trips
    between weekends and working days.

In general, the main differences between the users are:

1.  the proportion of casual rides per month increases with the
    temperature. So, members have a tendency of using the bikes even
    when the temperature is low, differently from the casual riders;

2.  overall, members rides happened in the same amount through all days
    of the week, different from casual rides that tend to increase
    during weekends;

3.  the duration of members rides tend to stay almost constant, on
    average, in any of the parameters analysed. Also, there is a
    concentration in trips up to 15 minutes. In comparison, casual rides
    showed more variation in the length of the trips which can indicate
    different motives for the use of the bikes between the users.

The analysis would be better complemented if we have two more
information in the data set: the motive of trip and the type of ticket
bought. This could lead to better conclusions to the marketing team on
how members and casual riders differ.
