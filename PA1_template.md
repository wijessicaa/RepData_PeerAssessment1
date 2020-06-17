---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data<- read.csv(unz("activity.zip","activity.csv"), na.strings = "NA")
data2<-data[!is.na(data$steps),]
```


## What is mean total number of steps taken per day?
1. the following table consists of the total number of steps taken per day

```r
data2$date<- as.Date(data2$date)
total_steps<-data.frame(data2 %>% group_by(date) %>% summarize(sum(steps, na.rm=T)))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
names(total_steps)<- c("Date","Total_steps")
total_steps
```

```
##          Date Total_steps
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
## 11 2012-10-13       12426
## 12 2012-10-14       15098
## 13 2012-10-15       10139
## 14 2012-10-16       15084
## 15 2012-10-17       13452
## 16 2012-10-18       10056
## 17 2012-10-19       11829
## 18 2012-10-20       10395
## 19 2012-10-21        8821
## 20 2012-10-22       13460
## 21 2012-10-23        8918
## 22 2012-10-24        8355
## 23 2012-10-25        2492
## 24 2012-10-26        6778
## 25 2012-10-27       10119
## 26 2012-10-28       11458
## 27 2012-10-29        5018
## 28 2012-10-30        9819
## 29 2012-10-31       15414
## 30 2012-11-02       10600
## 31 2012-11-03       10571
## 32 2012-11-05       10439
## 33 2012-11-06        8334
## 34 2012-11-07       12883
## 35 2012-11-08        3219
## 36 2012-11-11       12608
## 37 2012-11-12       10765
## 38 2012-11-13        7336
## 39 2012-11-15          41
## 40 2012-11-16        5441
## 41 2012-11-17       14339
## 42 2012-11-18       15110
## 43 2012-11-19        8841
## 44 2012-11-20        4472
## 45 2012-11-21       12787
## 46 2012-11-22       20427
## 47 2012-11-23       21194
## 48 2012-11-24       14478
## 49 2012-11-25       11834
## 50 2012-11-26       11162
## 51 2012-11-27       13646
## 52 2012-11-28       10183
## 53 2012-11-29        7047
```

2. A histogram consisting the total number of steps taken each day is shown below

```r
hist(total_steps$Total_steps, breaks = 10, xlab = "Total steps", main= "Total steps each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean<- (total_steps$Total_steps)

median<- median(total_steps$Total_steps)
```
mean = 126, 11352, 12116, 13294, 15420, 11015, 12811, 9900, 10304, 17382, 12426, 15098, 10139, 15084, 13452, 10056, 11829, 10395, 8821, 13460, 8918, 8355, 2492, 6778, 10119, 11458, 5018, 9819, 15414, 10600, 10571, 10439, 8334, 12883, 3219, 12608, 10765, 7336, 41, 5441, 14339, 15110, 8841, 4472, 12787, 20427, 21194, 14478, 11834, 11162, 13646, 10183, 7047 and median = 10765

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval<- data.frame(data2 %>% group_by(interval) %>% summarize(mean(steps, na.rm=T)))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
names(interval)<- c("Interval","mean_steps")
head(interval)
```

```
##   Interval mean_steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
## 6       25  2.0943396
```

```r
with(interval, plot(interval$Interval, interval$mean_steps, type="l", ylab="average steps", xlab = "interval", main="Plot of average steps for each interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maximum_interval <- which.max(interval$mean_steps)
interval2<- interval$Interval[maximum_interval]
```
The maximum step will lies at the `interval2`th steps

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NAs<- sum(is.na(data$steps))
```
the number of missing values in the dataset is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data_impute <- data
data_impute$steps[is.na(data_impute$steps)]<- mean(data_impute$steps, na.rm=TRUE)
sum(is.na(data_impute$steps))
```

```
## [1] 0
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
head(data_impute)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
data_impute$date<- as.Date(data_impute$date)
total_steps_impute<-data.frame(data_impute %>% group_by(date) %>% summarize(sum(steps, na.rm=T)))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
names(total_steps_impute)<- c("Date","Total_steps")
hist(total_steps_impute$Total_steps, breaks = 10, xlab = "Total steps", main= "Total steps each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_impute<-mean(total_steps_impute$Total_steps)
median_impute<- median(total_steps_impute$Total_steps)
```
the mean of the filled data is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4}.

In this case, I am using the mean of the data to fill the missing value hence, the mean of the imputed data and the mean of the data with ignoring the missing data will be the same. However, the median is changing from 10765 to 10766.19 which means the data is increasing. 

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data_impute$date <- as.Date(data_impute$date)
data_weekdays <- mutate(data_impute, days = weekdays(date))
data_weekdays$days_type <- ifelse(data_weekdays$days == "Sabtu" | data_weekdays$days == "Minggu", "weekend", "weekdays")
head(data_weekdays)
```

```
##     steps       date interval  days days_type
## 1 37.3826 2012-10-01        0 Senin  weekdays
## 2 37.3826 2012-10-01        5 Senin  weekdays
## 3 37.3826 2012-10-01       10 Senin  weekdays
## 4 37.3826 2012-10-01       15 Senin  weekdays
## 5 37.3826 2012-10-01       20 Senin  weekdays
## 6 37.3826 2012-10-01       25 Senin  weekdays
```

2. Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
weeks <- data_weekdays %>% group_by(interval, days_type) %>% summarize(mean(steps,na.rm=TRUE))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
names(weeks)=c("interval","days_type","total_steps")

ggplot(data = weeks, aes(x=interval,y=total_steps))+ facet_grid(days_type~.)+geom_line() + ggtitle("Weekdays vs. Weekend (Avg Steps)")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
