---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data
    Loading Packages

```r
library(pacman)
p_load(dplyr, tidyr, stringr, lubridate, httr, ggplot2, shiny, rio, rmarkdown, knitr)
```
    Unzip the file and load it in

```r
unzip("activity.zip")
activity <- import("activity.csv")
```
## What is mean total number of steps taken per day?
    Histogram of the Total Steps per a day
        Aggregate the sums of steps by date

```r
TotalSteps <- aggregate(steps ~ date, activity, FUN = sum, na.rm = TRUE)
```
        Make a Histogram of the sum of step

```r
qplot(steps, data = TotalSteps, bins = 25)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
        Calculate the mean and median for steps each day 

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```
        The mean total steps taken per day is 1.0766189\times 10^{4}
        The median total steps taken per day is 10765
## What is the average daily activity pattern?
    Time Series plot 

```r
activity$interval <- as.numeric(activity$interval)
avg.step.int <- aggregate(steps ~ interval, data = activity, FUN = mean)
qplot(interval, steps, group = 1, data = avg.step.int, geom = "line", 
      main = "Daily Average Steps for each Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
    Interval with the peak average steps daily

```r
avg.step.int[which(avg.step.int$steps == max(avg.step.int$steps)), 1] 
```

```
## [1] 835
```
        The interval with the peak average steps is 835 
## Imputing missing values
  Strategy used to impute missing values is to replace them with the mean of that specific time   interval

```r
activity.filled <- activity
for(i in 1:nrow(activity.filled)){
  if(is.na(activity.filled[i, ]$steps)){
    activity.filled[i, ]$steps <- avg.step.int[avg.step.int$interval==activity[i, 3], 2]
  }
}
```
  Making a new Histogram and determining the mean & median of the new data

```r
total.steps.filled <- aggregate(steps ~ date, activity.filled, sum)
qplot(steps, data = total.steps.filled, bins = 25)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(total.steps.filled$steps)
```

```
## [1] 10766.19
```

```r
median(total.steps.filled$steps)
```

```
## [1] 10766.19
```
    The mean total steps is 1.0766189\times 10^{4}
    The median total steps is 1.0766189\times 10^{4}
## Are there differences in activity patterns between weekdays and weekends?
  First need to separate weekdays into weekends

```r
activity.filled <- activity.filled %>% mutate(day.code = wday(activity.filled$date), 
                                              day.name = wday(activity.filled$date, 
                                                              label = TRUE))
daytype <- "day.type"
activity.filled[which(activity.filled$day.code != 1|7), daytype] <- "weekday"
activity.filled[which(activity.filled$day.code == 1), daytype] <- "weekend"
activity.filled[which(activity.filled$day.code == 7), daytype] <- "weekend"
```
  Find the mean steps at each interval on weekends and weekdays seperately 

```r
avg.step.int.wday <- aggregate(steps ~ interval + day.type, 
                               data = activity.filled,
                               FUN = mean) 
```
  Graph the average steps for each interval of each day separated by weekday and weekend

```r
qplot(interval, steps, group = 1, data = avg.step.int.wday, geom = "line", 
      facets = day.type~., 
      main = "Daily Average Steps for each Interval by weekday and weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
