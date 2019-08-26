---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

loading and reading the file.

ploting the graph by aggregating the steps by date.


```r
setwd("C:/Users/va18755/Documents/Version/reproducible reserarch week 2/")


a <- read.csv("activity.csv")

plot(aggregate(steps ~ date,a,sum, na.rm = TRUE), type = "h")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


## What is mean total number of steps taken per day?

The below graphs show the mean and median of steps aggregated by date.


```r
par(mfrow= c(1,2))
plot(aggregate(steps ~ date,a,mean, na.rm = TRUE), type = "h", main = "Mean Steps per day")

plot(aggregate(steps ~ date,a,median, na.rm = TRUE), type = "s", main = "Median Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Time series plot of the average number of steps taken


```r
par(mfrow = c(1,1))

plot(aggregate(steps ~ interval,a,mean, na.rm = TRUE), type = "l", main = "Mean Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


The 5-minute interval that, on average, contains the maximum number of steps

```r
b <- aggregate(steps ~ interval,a,mean, na.rm = TRUE)

 b[which(b$steps==max(b$steps)),]
```

```
##     interval    steps
## 104      835 206.1698
```
## What is the average daily activity pattern?

Counting the rows having null as steps.

Imputing startgy: imputing them with the mean of steps in that interval.

ploting the graph by aggregating the steps by date.



```r
nrow(a[is.na(a$steps),])
```

```
## [1] 2304
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.1
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
a <- a %>% group_by(interval) %>%
   mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))


plot(aggregate(steps ~ date,a,sum, na.rm = TRUE), type = "h")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

As per the imputing startegy the values are imputed based upon the interval mean, thereyby daily mean and median have changed due shift in the values.


```r
par(mfrow= c(1,2))
plot(aggregate(steps ~ date,a,mean, na.rm = TRUE), type = "h", main = "Mean Steps per day")

plot(aggregate(steps ~ date,a,median, na.rm = TRUE), type = "s", main = "Median Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

factoring the data based on weekday and weekend and aggregating them by day+ interval.

plotting the mean and median of the daily pattern as per weekend and weekday for panel plot


```r
a$week <- ifelse(weekdays(as.Date(a$date)) %in% c("Saturday", "Sunday"), "Weekend", "weekday")

c <-aggregate(steps ~ week+interval,a,mean)

par(mfrow= c(2,1))

plot(c[which(c$week == "weekday"),]$interval,c[which(c$week == "weekday"),]$steps, type = "l",
     main = "Mean Steps per day for weekday", xlab = "interval", ylab = "mean steps")

plot(c[which(c$week != "weekday"),]$interval,c[which(c$week != "weekday"),]$steps, type = "l",
     main = "Mean Steps per day for weekend", xlab = "interval", ylab = "mean steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



## Are there differences in activity patterns between weekdays and weekends?


Yes as per the above graph weekday activity is more than that of weekend
