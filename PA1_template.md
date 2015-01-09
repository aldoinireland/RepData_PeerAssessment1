
#Repdata-003 : Peer Assesment 1
##Loading and preprocessing the data
###Alan Walters
###Friday, January 09, 2015



##Load the data into R & Preprocess 
(1) The data is read from the csv file.
```{r}
        activityData <- read.csv(file="activity.csv");
```
and display the top reord set
```{r}
        head(activityData);
```


(2) We need to complete some conversion on this data.

- convert the date 
```{r}
        activityData$date <- as.Date(activityData$date)
        class(activityData$date)
```
- Dates with and without NA's
```{r}
        dataFiltered <- activityData[!is.na(activityData$steps),]
        dataRaw      <- activityData
```


##What is mean total number of steps taken per day?
(1) Make a histogram of the total number of steps taken each day
```{r}
        data <- dataFiltered 
        hist(data$steps, col="green", main="Steps per day" , xlab="steps");
```

(2) Calculate and report the mean and median total number of steps taken per day
```{r}
        c( mean(data$steps) , median(data$steps))
```

##What is the average daily activity pattern?

(1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

First we build a summary table:
```{r}
      intervalData <- data.frame(interval = unique(activityData$interval));
      for (i in 1:nrow(intervalData)) {
      subset <- data[data$interval == data[i,"interval"],];
      intervalData[i,"mean_all_days"] <- mean(subset$steps)
      };
      head(intervalData);
```
Then we plot it:
```{r}
      plot(intervalData$mean_all_days
      , type="l"
      , xlab="interval"
      , ylab="mean(steps per day"
      );
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
      intervalData[which.max(intervalData$mean_all_days),]
```
  
##Inputing missing values

(1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
      nrow(dataRaw[is.na(dataRaw$steps),])
```
(2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.   
We're going to normalize the results in a new copy of the raw data.
```{r}
      estimated <- data.frame(dataRaw[0,]);
      estimated <- rbind(estimated, dataRaw)
      head(estimated)
```
We'll fill the NA with the interval mean from before working with the new data.
```{r}
        for (i in 1:nrow(intervalData) ) {
        interval <- intervalData[i,"interval"]
        meanVal <- intervalData[i,"mean_all_days"]
        
        rows <- which(is.na(estimated$steps))
        estimated[rows, "steps"] <- meanVal
        }
```
(3) Create a new dataset that is equal to the original dataset but with the missing data filled in. 
```{r}
        data <- estimated
        
        head(data)
```
(4)Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  -Make a histogram
```{r} 
        hist(data$steps, col="green", main="Steps per day (estimated)" , xlab="steps");
```
  - Calculate and report the mean and median total number of steps taken per day
```{r} 
       c( mean(data$steps) , median(data$steps))
```
  - Do these values differ from the estimates from the first part of the assignment?

Including all that missing data had a significant effect on the mean (~1/6 the original value)

##  Are there differences in activity patterns between weekdays and weekends?

  -Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
        isWeekend <- function(x) {
        if( x == "Sunday" || x == "Saturday") "weekend" else "weekday";
        }
        data$day <- weekdays(data$date)
        data$dayType <- lapply(data$day, isWeekend)
        #make it a factor
        data$dayType <- sapply(data$dayType, as.factor)
        print(lapply(data, class))
```
  -display weekend and weekday

```{r}
        head(data[data$dayType == "weekday",])
        head(data[data$dayType == "weekend",])
```


  -Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
library("lattice")
dayType <- data$dayType
xyplot( steps~interval | dayType, data=data, type="l", layout=c(1,2))
```