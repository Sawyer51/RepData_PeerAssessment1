# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps
taken per day


```r
totalNumberPerday <- tapply(data$steps, data$date, sum)
hist(totalNumberPerday, breaks = 10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
m <- mean(totalNumberPerday, na.rm = TRUE)
med <- median(totalNumberPerday, na.rm = TRUE)
```

The mean of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>.
The median of total number of steps taken per day is 10765)

## What is the average daily activity pattern?


```r
data <- transform(data, interval = factor(interval))
averageStepsTaken <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
plot(levels(data$interval), averageStepsTaken, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxLocation <- which(averageStepsTaken == max(averageStepsTaken))
maxLocation <- (maxLocation - 1) * 5
```


515 is the 5-miniute interval, on average across all the days in the dataset, contains the maximum number of steps

## Imputing missing values


```r
totalNA <- sum(is.na(data$steps))
```

The total number of missing values in the dataset is 2304. 

Fill in the missing values in the dataset with mean for the mean total number of steps taken per day.

```r
data$steps[is.na(data$steps)] = m
totalNumberPerday <- tapply(data$steps, data$date, sum)
m <- mean(totalNumberPerday)
med <- median(totalNumberPerday)
```


After filling in the missing value with mean of the total number of steps taken per day:  
The mean of total number of steps taken per day is 4.16 &times; 10<sup>5</sup>.
The median of total number of steps taken per day is 1.1458 &times; 10<sup>4</sup>)

Do these values differ from the estimates from the first part of the assignment?  
Yes, these values differ from the estimates from the first part of the assignment. 

What is the impact of imputing missing data on the estimates of the total daily number of steps?  
Increasing these values. 


## Are there differences in activity patterns between weekdays and weekends?