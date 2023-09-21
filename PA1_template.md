---
title: "Activity Monitoring Data"
author: "MI-ayon"
date: "2023-09-21"
output: 
  html_document:
    number_sections: true
    keep_md: true
---
# Reproducible Research Peer Review Assignment: Activity Monitoring Data 


# The Dataset
The data for this assignment can be downloaded from the course web site:
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip



###### Submission

#1 Code for reading in the dataset and/or processing the data

```r
library(ggplot2)
library("data.table")
```
## Get the current working directory


```r
path <- getwd()
```
## Download the data files

```r
download.file(
  url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
  destfile = file.path(path, "activity.zip")
)
```

## Unzip the data files if the CSV file doesn't exist

```r
if (!file.exists('activity.csv')) {
  unzip(zipfile = "activity.zip", exdir = path)
}
```
## Load the data from the CSV file

```r
activity_monitoring_data <- read.csv('activity.csv')
```
## Processing the data: Convert the 'date' column to the Date format

```r
activity_monitoring_data$date <- as.Date(activity_monitoring_data$date, "%Y-%m-%d")
```

## Processing the data: Convert the 'date' column to the Date format

```r
activity_monitoring_data$date <- as.Date(activity_monitoring_data$date, "%Y-%m-%d")
```

# What is mean total number of steps taken per day?
## Calculating daily Step Count

```r
daily_step_count <- aggregate(steps ~ date, activity_monitoring_data, FUN = sum)
```

#2 Histogram of the total number of steps taken each day

```r
ggplot(daily_step_count, aes(x = steps)) +
  geom_histogram(fill = "aquamarine3", binwidth = 1200) +
  labs(
    title = "Histogram of Daily Steps Count",
    x = "Steps Count",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),    # Bold main title
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold")   # Bold y-axis title
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
# Mean and median number of steps taken each day
#3 Calculate Mean of daily steps

```r
steps_Mean <- mean(daily_step_count$steps, na.rm=TRUE)
steps_Mean
```

```
## [1] 10766.19
```
## Calculate Medain of daily steps

```r
steps_Median <- median(daily_step_count$steps, na.rm=TRUE)
steps_Median
```

```
## [1] 10765
```

# Calculate average number of steps per 5-min interval

```r
average_steps_per_interval <- aggregate(steps ~ interval, activity_monitoring_data, mean)
```

#4 Time Series Plot of average setps taken per day.

```r
ggplot(average_steps_per_interval, aes(x = interval , y = steps)) +
  geom_line(color = "mistyrose4", size = 1) +
  labs(
    title = "Time Series Plot of Average Steps per 5-Minute Interval",
    x = "5-Minute Interval",
    y = "Daily Average Number of Steps Taken"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16), 
    axis.title = element_text(face = "bold", size = 14)    
  )
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


#5 Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?

```r
max_steps_per_interval <- average_steps_per_interval[which.max(average_steps_per_interval$steps), ] 
max_steps_per_interval
```

```
##     interval    steps
## 104      835 206.1698
```


# Number of NAs in the original dataset

```r
num_missing_values <- nrow(activity_monitoring_data[is.na(activity_monitoring_data$steps),])
num_missing_values
```

```
## [1] 2304
```
#6 Code to describe and show a strategy for imputing missing data
#Load the data

```r
og_data_with_NA <- read.csv("activity.csv", header=TRUE,sep=",")
```
# Create a variable column with weekdays name

```r
og_data_with_NA$day <- weekdays(as.Date(og_data_with_NA$date))
```
# create average number of steps per 5-min interval and day

```r
steps_average <- aggregate(steps ~ interval + day, og_data_with_NA, mean)
```
# Create dataset with all NAs for substitution

```r
misssing_data_dataset <- og_data_with_NA [is.na(og_data_with_NA$steps),]
```
# Merge NAs dataset with the average steps based on 5-min interval+weekdays, for substitutions

```r
merged_data <- merge(misssing_data_dataset, steps_average, by=c("interval", "day"))
```
# Pull data without NAs

```r
neat_data <- og_data_with_NA [!is.na(og_data_with_NA$steps),]
```
#Reorder the new substituted data in the same format as the clean data set (Leave out the NAs column which will be substituted by the average steps based on 5-min interval + day) 

```r
merged_data_formatted <- merged_data[,c(5,4,1,2)]
colnames(merged_data_formatted) <- c("steps", "date", "interval", "day")
```
# Merge the new average data (NAs) with the dataset without NAs

```r
merged_avg_data <- rbind (neat_data, merged_data_formatted)
```
# Calculate the total steps per day on the merged average data

```r
steps_per_day_merged <- aggregate(steps ~ date, merged_avg_data, FUN = sum)
```

#7 Histogram of the total number of steps taken each day after missing values are imputed

```r
ggplot (steps_per_day_merged, aes (x = steps)) +
  geom_histogram(fill = "bisque4", binwidth = 1000) +
  labs(
    title = " Histogram of Steps Taken Each Day ",
    x = "Steps",
    y = "Frequency"
  ) + 
  theme(
    plot.title = element_text(size = 20, face = "bold"),    # Bold main title
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold")   # Bold y-axis title
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png)<!-- -->


# Mean of total steps with imputed data

```r
steps_mean_merged <- mean(steps_per_day_merged$steps, na.rm=TRUE)
steps_mean_merged
```

```
## [1] 10821.21
```
# Median of total steps with imputed data

```r
steps_median_merged <- median(steps_per_day_merged$steps, na.rm=TRUE)
steps_median_merged
```

```
## [1] 11015
```
#create a new variable/column indicating weekday or weekend

```r
merged_avg_data$DayType <- ifelse(merged_avg_data$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```
# create table with average steps per time interval across weekday days or weekend days

```r
average_steps_per_interval_data <- aggregate(steps ~ interval+DayType, merged_avg_data, FUN = mean)
```

#8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends



```r
ggplot(average_steps_per_interval_data, aes(x = interval, y = steps)) +
  geom_line(color = "red") +  
  labs(
    title = "Time Series Plot of Average Steps per Interval: weekdays vs. weekends",
    x = "Interval",
    y = "Average Number of Steps"
  ) +
  facet_grid(DayType ~ .) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-30-1.png)<!-- -->











