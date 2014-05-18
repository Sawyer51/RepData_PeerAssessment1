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
averageStepsTaken <- tapply(data$steps, factor(data$interval), mean, na.rm = TRUE)
plot(levels(data$interval), averageStepsTaken, type = "l")
```

```
## Error: 'x' and 'y' lengths differ
```

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
averageStepsTaken <- tapply(data$steps, factor(data$interval), mean, na.rm = TRUE)
values <- averageStepsTaken[as.integer(factor(data$interval))]
data$steps[is.na(data$steps)] = values[is.na(data$steps)]
totalNumberPerday <- tapply(data$steps, data$date, sum)
hist(totalNumberPerday, breaks = 10)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
m <- mean(totalNumberPerday)
med <- median(totalNumberPerday)
```


After filling in the missing value with mean of the total number of steps taken per day:  
The mean of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>.
The median of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>)

Do these values differ from the estimates from the first part of the assignment?  
Since I filled in the missing value with the mean. Mean is not differ from first part.

What is the impact of imputing missing data on the estimates of the total daily number of steps?  
The mean became the median.


## Are there differences in activity patterns between weekdays and weekends?



```r
wd <- data$date
wd <- strptime(wd, "%Y-%m-%d")
wd <- weekdays(wd)
wd <- as.character(wd)
weekend <- (wd == "Sunday" | wd == "Saturday")
data$weekdays = "weekdays"
data$weekdays[weekend] = "weekend"
library(lattice)
averageStepsTaken <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
xyplot(averageStepsTaken ~ data$interval | factor(data$weekdays), type = "l", 
    layout = c(1, 2))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


