# Reproducible Research: Peer Assessment 1

## Introduction

> It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

> This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

> The data for this assignment can be downloaded from the course web site:
  
  > Dataset: Activity monitoring data [52K]
The variables included in this dataset are:
  
  > steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

> date: The date on which the measurement was taken in YYYY-MM-DD format

> interval: Identifier for the 5-minute interval in which measurement was taken

> The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

> This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

> Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

> For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

> Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

> NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data

> Show any code that is needed to

> Load the data (i.e. read.csv())


```r
unzip("activity.zip")
activity <- read.csv(file="activity.csv", header=TRUE, sep=",")
str(activity)
```

```
## 'data.frame':  17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

> Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

> For this part of the assignment, you can ignore the missing values in the dataset.
> Make a histogram of the total number of steps taken each day


```r
stepsPerDay <- aggregate(steps ~ date, data = activity, FUN = "sum", na.rm=TRUE)
head(stepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
hist(stepsPerDay$steps, 
     xlab="total number of steps taken each day",
     main="Histogram of the total number of steps taken each day.",col=5)
```

![plot of chunk stepsPerDay](figure/stepsPerDay.png) 

> Calculate and report the mean and median total number of steps taken per day


```r
mean(stepsPerDay$steps)
```

```
## [1] 10766
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

> Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalAverages <- aggregate(steps ~ interval, data = activity, FUN = "mean", na.rm=TRUE)
str(intervalAverages)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
plot(intervalAverages$interval, intervalAverages$steps,type="l", col="blue",
     xlab="5-minute interval", ylab="number of steps",
     main="Steps taken, averaged across all days.")
```

![plot of chunk intervalAveragesPlot](figure/intervalAveragesPlot.png) 


> Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalAverages[which.max(intervalAverages$steps),]
```

```
##     interval steps
## 104      835 206.2
```


## Imputing missing values

> Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

> Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(activity[!complete.cases(activity),])
```

```
## [1] 2304
```

```r
table(is.na(activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

> Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
> Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityFilled <- activity
for (i in 1:nrow(activityFilled)) {
  if (is.na(activityFilled$steps[i])) {
    activityFilled$steps[i] <- intervalAverages[ intervalAverages$interval==5,]$steps
  }        
}
nrow(activityFilled[!complete.cases(activityFilled),])
```

```
## [1] 0
```

> Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDayFilled <- aggregate(steps ~ date, data = activityFilled, FUN = "sum", na.rm=TRUE)
head(stepsPerDayFilled)
```

```
##         date    steps
## 1 2012-10-01    97.81
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(stepsPerDayFilled$steps, 
     xlab="total number of steps taken each day",
     main="Histogram of the total number of steps taken each day.",col=4)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


```r
mean(stepsPerDay$steps)
```

```
## [1] 10766
```

```r
mean(stepsPerDayFilled$steps)
```

```
## [1] 9367
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```

```r
median(stepsPerDayFilled$steps)
```

```
## [1] 10395
```

Here imputing new values gaves us lower mean and median.

## Are there differences in activity patterns between weekdays and weekends?

> For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
> Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# "lundi"    "mardi"    "mercredi" "jeudi"    "vendredi" "samedi"   "dimanche"
# are the days of my locales.
activityFilled$day<-as.factor(
  ifelse(weekdays(activityFilled$date) %in% c("samedi","dimanche"),
         "weekend",
         "weekday"))
table(activityFilled$day)
```

```
## 
## weekday weekend 
##   12960    4608
```

> Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
  
  
  ```r
library(lattice)
xyplot(steps ~ interval | day, 
       aggregate(steps ~ interval + day, activityFilled, FUN = mean), 
       layout = c(1, 2), type = "l", group=day, ylab="Number of steps",
       col=c(4,4))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
