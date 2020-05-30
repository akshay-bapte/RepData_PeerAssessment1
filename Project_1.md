Reproducible Research : Course Project 1
================
Akshay Bapte
29/05/2020

## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment : [Activity Monitoring
Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

  - **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as NA)

  - **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

  - **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

## Assignment Instructions

1.Code for reading in the dataset and/or processing the data

2.Histogram of the total number of steps taken each day

3.Mean and median number of steps taken each day

4.Time series plot of the average number of steps taken

5.The 5-minute interval that, on average, contains the maximum number of
steps

6.Code to describe and show a strategy for imputing missing data

7.Histogram of the total number of steps taken each day after missing
values are imputed

8.Panel plot comparing the average number of steps taken per 5-minute
interval across weekdays and weekends

9.All of the R code needed to reproduce the results (numbers, plots,
etc.) in the report

## Loading and preprocessing the data

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
data <- read.csv("activity.csv")
summary(data)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

``` r
data$date <- as.Date(as.character(data$date), format = "%Y-%m-%d")
class(data$date)
```

    ## [1] "Date"

## What is mean total number of steps taken per day?

``` r
steps_per_day <- data %>% group_by(date) %>% summarize(Total_steps = sum(steps))
head(steps_per_day)
```

    ## # A tibble: 6 x 2
    ##   date       Total_steps
    ##   <date>           <int>
    ## 1 2012-10-01          NA
    ## 2 2012-10-02         126
    ## 3 2012-10-03       11352
    ## 4 2012-10-04       12116
    ## 5 2012-10-05       13294
    ## 6 2012-10-06       15420

``` r
plot1 <- ggplot(steps_per_day, aes(Total_steps)) + geom_histogram(colour = "red") +
  labs(title = "Total number of steps taken per day", x = "Total steps taken per day")
print(plot1)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Project_1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Here is the mean of total number of steps taken per day :

``` r
mean(steps_per_day$Total_steps, na.rm = TRUE) 
```

    ## [1] 10766.19

Here is the median of total number of steps taken per day :

``` r
median(steps_per_day$Total_steps, na.rm = TRUE)
```

    ## [1] 10765

## What is the average daily activity pattern?

A time series plot of the 5-minute interval (x-axis) and the average
number of steps taken, averaged across all days (y-axis)
:

``` r
steps_per_interval <- data %>% group_by(interval) %>% summarize(Avg_steps = mean(steps, na.rm = TRUE))
head(steps_per_interval)
```

    ## # A tibble: 6 x 2
    ##   interval Avg_steps
    ##      <int>     <dbl>
    ## 1        0    1.72  
    ## 2        5    0.340 
    ## 3       10    0.132 
    ## 4       15    0.151 
    ## 5       20    0.0755
    ## 6       25    2.09

``` r
plot2 <- ggplot(steps_per_interval, aes(interval, Avg_steps)) + geom_line(colour = "red") + 
   labs(title = "Average number of steps per interval", x = "Interval", y = "Average number of steps")
print(plot2)
```

![](Project_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The 5-minute interval, on average across all the days in the dataset,
which contains the maximum number of steps is :

``` r
steps_per_interval[which.max(steps_per_interval$Avg_steps),]$interval
```

    ## [1] 835

## Imputing missing values

There are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.

The total number of missing values in the data :

``` r
sum(is.na(data$steps))
```

    ## [1] 2304

For imputing the missing values in the datasets, we are replacing the
missing values with the mean for that 5-minute interval
:

``` r
steps_no_NA <- steps_per_interval$Avg_steps[match(data$interval, steps_per_interval$interval)]
sum(is.na(steps_no_NA))
```

    ## [1] 0

Creating a new dataset that is equal to the original dataset but with
the missing data filled in
:

``` r
data_no_NA <- transform(data, steps = ifelse(is.na(data$steps), yes = steps_no_NA, no = data$steps))
Total_steps_no_NA <- data_no_NA %>% group_by(date) %>% summarize(Total_steps = sum(steps))
```

A histogram of the total number of steps taken each day
:

``` r
ggplot(Total_steps_no_NA, aes(Total_steps)) + geom_histogram(colour = "red") +
  labs(title = "Total number of steps taken per day", x = "Total steps taken per day")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Project_1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Mean and Median total number of steps taken per day :

``` r
mean(Total_steps_no_NA$Total_steps)
```

    ## [1] 10766.19

``` r
median(Total_steps_no_NA$Total_steps)
```

    ## [1] 10766.19

We can clearly see here that both these values are different from
earlier, when we calculated these values(without removing NAs). And a
result of removing NAs is that both mean and median are now the same
value, unlike when we first measured
them.

## Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels –
“weekday” and “weekend” indicating whether a given date is a weekday
or weekend day :

``` r
data$daytype <- sapply(data$date, function(x){
   if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday") {
     y <- "Weekend"
   } else {
     y <- "Weekday" 
   }
     
})
```

Making a panel plot containing a time series plot of the 5-minute
interval (x-axis) and the average number of steps taken, averaged across
all weekday days or weekend days (y-axis)
:

``` r
steps_per_daytype <- data %>% group_by(interval, daytype) %>% summarize(Avg_steps = mean(steps, na.rm = TRUE))
ggplot(steps_per_daytype, aes(interval, Avg_steps, col = daytype )) + geom_line() + facet_wrap(~daytype, ncol = 1, nrow=2) + labs(title = "Average daily steps by type of date", x = "Interval", y = "Average steps")
```

![](Project_1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
