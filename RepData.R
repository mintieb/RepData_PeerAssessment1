library(dplyr)
library(ggplot2)
setwd("~/Documents/Classes/Data Science JHU/Reproducible Research/RepData_PeerAssessment1")

### Loading and preprocessing the data
data <- read.csv("activity.csv")

## 2. Process/transform the data (if necessary) into a format suitable for your analysis


### What is mean total number of steps taken per day?
data_day_group <- group_by(data,date)
data_day_total <- summarise(data_day_group, total_steps=sum(steps, na.rm=TRUE))

## hisogram
hist(data_day_total$total_steps, main="Histogram of the Total Number of Steps Taken Each Day", 
     xlab="Total Number of Steps Each Day")

## mean and median total number of steps taken per day
summary(data_day_total)


### What is the average daily activity pattern?
data_interval_group <- group_by(data,interval)
data_interval_average <- summarise(data_interval_group, average_steps=mean(steps, na.rm=TRUE))

plot(data_interval_average$interval, data_interval_average$average_steps, type = "l",
     main = "Time Series Plot of the 5-minute Interval",
     xlab = "Interval", ylab = "Mean Steps Across All Days")

max_index <- which.max(data_interval_average$average_steps)
## interval
data_interval_average[max_index,]$interval
## max number of steps
data_interval_average[max_index,]$average_steps

        
### Imputing missing values

## 1. total number of missing values in the dataset (i.e. the total number of rows with `NA`s)
sum(is.na(data$steps))

## 2 Devise a strategy for filling in all of the missing values in the dataset. 
print("use the mean for that 5-minute interval")

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

data_new <- data
for (i in 1:nrow(data_new)) {
        if(is.na(data_new$steps[i])) {
                data_new$steps[i] <- data_interval_average$average_steps[
                        which(data_interval_average$interval == data_new$interval[i])]
        }
}

        
## 4. Make a histogram of the total number of steps taken each day 
data_steps_sum <- aggregate(steps ~ date, data_new, sum)
hist(data_steps_sum$steps, 
     main = "Histogram of Total Number of Steps Taken Per Day", xlab = "Steps per Day")

## Calculate and report the **mean** and **median** total number of steps taken per day. 
mean(data_steps_sum$steps)
median(data_steps_sum$steps)
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels 
        ## -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
week_day_check <- function(date_x) {
        wd <- weekdays(as.Date(date_x, '%Y-%m-%d'))
        if  (wd == 'Saturday' || wd == 'Sunday') {
                day_x <- 'weekend'
        } 
        else {
                day_x <- 'weekday'
        }
        day_x
}

data_new$day <- as.factor(sapply(data_new$date, week_day_check))


## 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
        ## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
data_average_steps <- aggregate(steps ~ interval+day, data_new, mean)
        
g <- ggplot(data_average_steps, aes(interval, steps)) +
        geom_line(stat = "identity", aes(colour = day)) +
        facet_grid(day ~ ., scales = "fixed", space = "fixed") +
        labs(x="Interval", y = expression("Average Number of Steps")) +
        ggtitle("Average Number of Steps per Interval by Days Type")
print(g)