# Week 2 Course Project 1
Keith Webster  
May 29, 2017  



# Reproducible Research Week 2 Course Project 1
## Load required libraries and process activity data
### Unzip archive file and read data into data frame

```r
# Graphics library
library(ggplot2)

# Unzip zip file
unzip("repdata%2Fdata%2Factivity.zip")

# Read activity data into data frame "activity_data"
activity_data <- read.csv("activity.csv")
```
### Summarize Activity Data

```r
# Display head activity data
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
### Data dimensions of activity_data
dim(activity_data)
```

```
## [1] 17568     3
```

```r
### Data summary for activity_data
summary(activity_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
### NA and Date String Modification

```r
## Data processing NA and date formatting

### Generate NA Vector
activity_data_NA <- is.na(activity_data$steps)
### Remove NA data from file and store new file
activity_data_nona <- activity_data[!activity_data_NA,]
### Convert date type to date class
activity_data$date <- as.Date(as.character(activity_data$date))
```
## What is the mean total number of steps take per day?
### 1. Calculate the total number of steps taken per day

```r
# aggregate clean non NA steps per day (SUM)
daily_aggregate <- aggregate(activity_data$steps, by=list(activity_data$date), sum)
# adjust column names
names(daily_aggregate)[1] = "date"
names(daily_aggregate)[2] = "totalsteps"
# top 15 of summed steps by day
head(daily_aggregate,15)
```

```
##          date totalsteps
## 1  2012-10-01         NA
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08         NA
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## 11 2012-10-11      10304
## 12 2012-10-12      17382
## 13 2012-10-13      12426
## 14 2012-10-14      15098
## 15 2012-10-15      10139
```
### 2. Make a histogram of the total number of steps taken each day

```r
# Plot using ggplot
ggplot(daily_aggregate, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
# Mean of steps taken per day
mean(daily_aggregate$totalsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
# Mean of steps taken per day
median(daily_aggregate$totalsteps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
Mean_Data_ByInterval <- aggregate(activity_data_nona$steps, by=list(activity_data_nona$interval), mean)

#set the column names
names(Mean_Data_ByInterval)[1] = "interval"
names(Mean_Data_ByInterval)[2] = "steps"

ggplot(Mean_Data_ByInterval, aes(x = interval, y = steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="steelblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum numbers of steps?


```r
max_interval <- Mean_Data_ByInterval[which.max(Mean_Data_ByInterval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

## Impute missing values from activity data set
### 1. Calculate and report the total number of missing values in the dataset


```r
# Generate list of NAs
missing_values <- sum(activity_data_NA)
missing_values
```

```
## [1] 2304
```

### 2. Use mean interval steps for filling in all of the missing values in the activity dataset
### 3. Create a new dataset that replaces the na data in the original activity data set


```r
# Generate enhanced activity data
activity_data_2 <- activity_data
# NA's in the activity dataset
# dataset minus NAs for mean calc
NA_activity_data_2 <- activity_data_2[is.na(activity_data_2$steps),]
clean_activity_data_2 <- activity_data_2[!is.na(activity_data_2$steps),]
```

```r
# Generate mean data_enhanced by interval
mean_data2_byinterval <- aggregate(clean_activity_data_2$steps, by=list(clean_activity_data_2$interval), sum)
names(mean_data2_byinterval)[1] = "interval"
names(mean_data2_byinterval)[2] = "steps"
# Impute values
activity_data_2 <- activity_data
missingData <- is.na(activity_data_2$steps)
meanVals <- tapply(activity_data_nona$steps, activity_data_nona$interval, mean, na.rm=TRUE, simplify=TRUE)
activity_data_2$steps[missingData] <- meanVals[as.character(activity_data_2$interval[missingData])]
```

```r
# sum missing data count
sum(missingData)
```

```
## [1] 2304
```

```r
# count of NA values
sum(is.na(activity_data_2$steps))
```

```
## [1] 0
```
### 4.1 Make a histogram of the total steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
FullSummedDataByDay <- aggregate(activity_data_2$steps, by=list(activity_data_2$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
```

```
##          date totalsteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
## 11 2012-10-11   10304.00
## 12 2012-10-12   17382.00
## 13 2012-10-13   12426.00
## 14 2012-10-14   15098.00
## 15 2012-10-15   10139.00
```

```r
#Plot using ggplot
ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
# Mean on New Data
mean(FullSummedDataByDay$totalsteps)
```

```
## [1] 10766.19
```

```r
#Median on New Data
median(FullSummedDataByDay$totalsteps)
```

```
## [1] 10766.19
```
### 4.2 Do these values differ from the estimates in the first part of the assignment?

### Yes the mean is the same but the median has increased.

### 4.3 What is the impact of imputing the missing data on the estimates of the total number of daily steps?

### Use of mean data as the data input method skews data towards the mean

## Are there differences in activity patterns between weekdays and weekends?

```r
activity_data_2$weekday <- weekdays(activity_data_2$date)
activity_data_2$weekend <- ifelse (activity_data_2$weekday == "Saturday" | activity_data_2$weekday == "Sunday", "Weekend", "Weekday")
#activity_data_2$weekend <- as.factor(activity_data_2$weekend)
head(activity_data_2,5)
```

```
##       steps       date interval weekday weekend
## 1 1.7169811 2012-10-01        0  Monday Weekday
## 2 0.3396226 2012-10-01        5  Monday Weekday
## 3 0.1320755 2012-10-01       10  Monday Weekday
## 4 0.1509434 2012-10-01       15  Monday Weekday
## 5 0.0754717 2012-10-01       20  Monday Weekday
```

```r
MeanDataWeekendWeekday <- aggregate(activity_data_2$steps, by=list(activity_data_2$weekend, activity_data_2$interval), mean)
names(MeanDataWeekendWeekday)[1] ="weekend"
names(MeanDataWeekendWeekday)[2] ="interval"
names(MeanDataWeekendWeekday)[3] ="steps"

ggplot(MeanDataWeekendWeekday, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

### There appears to be higher steps per day on the weekend implying more physical activity which makes sense.




  


  

