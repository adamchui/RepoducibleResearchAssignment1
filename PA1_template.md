---
title: "Reproducible Research - Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## 1. Code for reading in the dataset and/or processing the data


```{r}

#Read Source File
activity<- read.csv("activity.csv")

#Convert the date to proper R date format
activity$date <- strptime(activity$date, format = "%Y-%m-%d")
activity$date <- as.Date(activity$date)

```

## 2. Histogram of the total number of steps taken each day


```{r}

#Group the steps by date
StepsByDate <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

library(ggplot2)
#Historgram of total number of steps taken each day
ggplot(activity, aes(x=date, weights=steps)) + geom_histogram(binwidth = 1 ) + ylab("Steps") + xlab("Date") + labs(title = "Mean total number of steps taken per day")
ggsave("./figure/plot1.png")

```

## 3. Mean and median number of steps taken each day 

```{r}
MeanStepsByDate <- aggregate(steps ~ date, activity, mean, na.rm = TRUE)
# Mean number of steps taken each day 
MeanStepsByDate

MedianStepsByDate <- aggregate(steps ~ date, activity, median, na.rm = TRUE)
# Median number of steps taken each day 
MedianStepsByDate

```

## 4. Time series plot of the average number of steps taken

```{r}
png("./figure/plot2.png")
with(MeanStepsByDate, plot(x = date, y = steps, type = "l", ylab = "Steps", xlab ="Date", main = "Means Steps"))
dev.off()
```


#5. The 5-minute interval that, on average, contains the maximum number of steps

```{r}

#Mean of the Steps by interval column
MeanStepsByDateAndInterval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

#the record with the maximum number of steps
MeanStepsByDateAndInterval[which.max(MeanStepsByDateAndInterval$steps),]

```

## 6. Code to describe and show a strategy for imputing missing data

### 6.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
nrow(subset(activity, is.na(steps)))
```

### 6.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

```{r}
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#mean for that 5-minute interval
GlobalMeanStepsByDateAndInterval <- mean(MeanStepsByDateAndInterval$steps)
GlobalMeanStepsByDateAndInterval

```

### 6.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
activity_fillNA <- activity
activity_fillNA[is.na(activity_fillNA$steps),]$steps <- GlobalMeanStepsByDateAndInterval
```

### 7.1 Make a histogram of the total number of steps taken each day 

```{r}
# Aggregate the steps by date ( after NA is filled with NA )
StepsByDate_fillNA <- aggregate(steps ~ date, activity_fillNA, sum, na.rm = TRUE)
ggplot(StepsByDate_fillNA, aes(x=date, weights=steps)) + geom_histogram(binwidth = 1 ) + ylab("Steps") + xlab("Date") + labs(title = "Mean total number of steps taken per day (Fill NA with mean)")
ggsave("./figure/plot3.png")

```

### 7.2 Calculate and report the mean and median total number of steps taken per day. 

```{r}

#calculate the mean steps by date after fill up NA with mean in interval
MeanStepsByDate_fillNA <- aggregate(steps ~ date, activity_fillNA, mean, na.rm = TRUE)

#Compare the mean steps by date before and after fill up NA with mean in interval
DiffMeanStepsByDate <- merge(x=MeanStepsByDate_fillNA, y=MeanStepsByDate, by = "date")
DiffMeanStepsByDate[DiffMeanStepsByDate$steps.x != DiffMeanStepsByDate$steps.y, ]
#Return nothing, they are completely the same      

#calculate the median steps by date after fill up NA with mean in interval
MedianStepsByDate_fillNA <- aggregate(steps ~ date, activity_fillNA, median, na.rm = TRUE)

#Compare the median steps by date before and after fill up NA with mean in interval
DiffMedianStepsByDate <- merge(x=MedianStepsByDate_fillNA, y=MedianStepsByDate, by = "date")
DiffMedianStepsByDate[DiffMedianStepsByDate$steps.x != DiffMedianStepsByDate$steps.y, ]
#Return nothing, they are completely the same      

#Do these values differ from the estimates from the first part of the assignment? 
#The Value of Mean and Median are the same

#What is the impact of imputing missing data on the estimates of the total daily number of steps?
DiffStepsByDate_fillNA<- merge(x=StepsByDate_fillNA, y = StepsByDate_fillNA, by = "date")
DiffStepsByDate_fillNA[DiffStepsByDate_fillNA$steps.x != DiffStepsByDate_fillNA$steps.y, ]
#All are the same


```


#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}

#Add Data with WeekDays info
activity_fillNA_WithWeekDays <- activity_fillNA
activity_fillNA_WithWeekDays$WeekDays <- ""

activity_fillNA_WithWeekDays[weekdays(activity_fillNA_WithWeekDays$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday","Friday"),]$WeekDays <- "weekday"

activity_fillNA_WithWeekDays[
  weekdays(activity_fillNA_WithWeekDays$date) %in% c("Saturday", "Sunday"),]$WeekDays <- "weekend"

activity_fillNA_WithWeekDays$WeekDays <- factor(activity_fillNA_WithWeekDays$WeekDays)


library(lattice)

#Mean of the Steps by interval & WeekDays column
MeanStepsByDateAndWeekDays <- aggregate(steps ~ interval + WeekDays, activity_fillNA_WithWeekDays, mean, na.rm = TRUE)


#Plot Planel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
trellis.device(device="png", filename="./figure/plot4.png")
print(xyplot(steps ~ interval  | WeekDays, data = MeanStepsByDateAndWeekDays, type = "l"))
dev.off()

```


