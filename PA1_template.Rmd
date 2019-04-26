---
title: "Reproducible Research - Week 2 - Assignment"
output: html_document
---

## 1. Read and summarize data
```{r ReadData, echo=TRUE}
# download file 
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip")

# unzip and read data 
unzip("activity.zip")
data <- read.csv("activity.csv", header = TRUE, sep=",")

# summarize data
head(data)
dim(data)
summary(data)
```

## 2. Histogram of the total number of steps taken each day
```{r Histogram, echo=TRUE}
# calculate sum of steps for each day
library(dplyr)
by_date <- group_by(data, date)
sum_day <- summarize(by_date, sum(steps))
colnames(sum_day) <- c("date", "sum_steps")

# plot histogram
hist(sum_day$sum_steps, col="green", breaks=10, main="Total Number of Steps per Day", xlab="Steps", ylab="Frequency")
```

## 3. Mean and median number of steps taken each day
```{r mean, echo=TRUE}
# calculate mean number of steps for each day
mean <- mean(sum_day$sum_steps, na.rm=TRUE)
```
The mean number of steps taken each day is `r mean`

```{r median, echo=TRUE}
# calculate median number of steps for each day
median <- median(sum_day$sum_steps, na.rm=TRUE)
```
The median number of steps taken each day is `r median`


## 4. Time series plot of the average number of steps taken
```{r averageInterval, echo=TRUE}
# calculate average number of steps across all days
dataNA <- is.na(data$steps)
data_noNA <- data[!dataNA,]
Av_data_by_interval <- aggregate(data_noNA$steps, by=list(data_noNA$interval), mean)
colnames(Av_data_by_interval) <- c("Interval", "Average_steps")
# plot average number of steps across all days versus interval
plot(Av_data_by_interval$Interval, Av_data_by_interval$Average_steps, type="l", xlab="Interval", ylab="Average steps across all days", main="Average Number of Steps across all Days versus Interval")
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps
```{r max averageInterval, echo=TRUE}
max_AvInt <- Av_data_by_interval[which.max(Av_data_by_interval$Average_steps),]
max_AvInt
```

## 6. Code to describe and show a strategy for imputing missing data
```{r imputingMissingData, echo=TRUE}
# Calculate and report the total number of missing values in the dataset 
missing <- sum(dataNA)
```
The number of missing values are `r missing`

```{r replace missing values, echo=TRUE}
# replace missing values by mean value of respective interval in a new dataset
mergeData <- merge(data, Av_data_by_interval, by.x="interval", by.y="Interval", all=TRUE)
mergeData <- arrange(mergeData,date)
mergeData_NA <- filter(mergeData, is.na(steps))
mergeData_NArepl <- mutate (mergeData_NA, steps=Average_steps)
mergeData_notNA <- filter(mergeData, !is.na(steps)) 
data_new <- rbind(mergeData_NArepl,mergeData_notNA )
head(data_new)
```

## 7. Histogram of the total number of steps taken each day after missing values based on imputed data
``` {r histogram_imputedValues, echo=TRUE}
# histogram of the total number of steps taken each day 
by_date_new <- group_by(data_new, date)
sum_day_new <- summarize(by_date_new, sum(steps))
colnames(sum_day_new) <- c("date", "sum_steps")

# plot histogram
hist(sum_day_new$sum_steps, col="blue", breaks=10, main="Total Number of Steps per Day", xlab="Steps", ylab="Frequency")
```


### Calculate Mean and median number of steps taken each day based on imputed data
```{r mean_new, echo=TRUE}
# calculate mean and median numbesr of steps for each day based on imputed data
mean_new <- mean(sum_day_new$sum_steps, na.rm=TRUE)
median_new  <- median(sum_day_new$sum_steps, na.rm=TRUE)
my_matrix <-matrix(c(mean, mean_new, median, median_new),2,2)
table<-data.frame(my_matrix)
rownames(table) <- c("with NA", "imputed NA")
colnames(table) <- c("mean", "median")
table
```
## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r plotweek_weekends, echo=TRUE}
# assign dates to weekdays
data_new$date <- as.Date(data_new$date)
data_new$weekday <- weekdays(data_new$date)
data_new$weekend <- ifelse(data_new$weekday=="zaterdag" | data_new$weekday=="zondag", "Weekend", "Weekday" )
# average data per 5-minute interval
Av_data_new_by_interval <- aggregate(data_new$steps, by=list(data_new$weekend, data_new$interval), mean)
colnames(Av_data_new_by_interval) <- c("weekend", "interval", "av_steps")
# make plot
library(ggplot2)
ggplot(Av_data_new_by_interval, aes(x=interval, y=av_steps, color=weekend)) + geom_line() + facet_grid(weekend ~.) + xlab("Interval") + ylab("average Steps") + ggtitle("Comparison of Average Number of Steps taken per 5-minute Interval")
```

