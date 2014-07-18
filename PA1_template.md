# Reproducible Research:Peer Assessment 1
## Loading and preprocessing the data 
###### Definition of common functions

```r
library(plyr)
library(ggplot2)
mean.calculation <- function (a){
    meanPerInterval <- aggregate(x = a$steps, by = list(factor(a$interval)), FUN = "mean")
    # add column names
    colnames(meanPerInterval) <- c("interval","totalSteps")
    # convert interval to a numeric and split into hours and minutes
    meanPerInterval$interval <- as.numeric(as.character(meanPerInterval$interval))
    rownames(meanPerInterval) <- meanPerInterval$interval
    h <- as.integer(meanPerInterval$interval/100)
    m <- meanPerInterval$interval - h*100
    # calculate interval as fractional hours
    fInterval <- h+m/60
    meanPerInterval <- cbind(meanPerInterval,fInterval)
    return (meanPerInterval)
}
```
###### Read raw data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```
###### Create data frame with NA removed

```r
ac <- activity[!is.na(activity$steps),]
```

## What is mean total number of steps taken per day?
###### Create aggregate data frame with monthly totals

```r
totalPerDay <- aggregate(x = ac$steps, by = list(factor(ac$date)), FUN = "sum")
colnames(totalPerDay) <- c("Date","TotalSteps")
```
###### Total number of steps taken per day


```r
totalPerDay
```

```
##          Date TotalSteps
## 1  2012-10-02        126
## 2  2012-10-03      11352
## 3  2012-10-04      12116
## 4  2012-10-05      13294
## 5  2012-10-06      15420
## 6  2012-10-07      11015
## 7  2012-10-09      12811
## 8  2012-10-10       9900
## 9  2012-10-11      10304
## 10 2012-10-12      17382
## 11 2012-10-13      12426
## 12 2012-10-14      15098
## 13 2012-10-15      10139
## 14 2012-10-16      15084
## 15 2012-10-17      13452
## 16 2012-10-18      10056
## 17 2012-10-19      11829
## 18 2012-10-20      10395
## 19 2012-10-21       8821
## 20 2012-10-22      13460
## 21 2012-10-23       8918
## 22 2012-10-24       8355
## 23 2012-10-25       2492
## 24 2012-10-26       6778
## 25 2012-10-27      10119
## 26 2012-10-28      11458
## 27 2012-10-29       5018
## 28 2012-10-30       9819
## 29 2012-10-31      15414
## 30 2012-11-02      10600
## 31 2012-11-03      10571
## 32 2012-11-05      10439
## 33 2012-11-06       8334
## 34 2012-11-07      12883
## 35 2012-11-08       3219
## 36 2012-11-11      12608
## 37 2012-11-12      10765
## 38 2012-11-13       7336
## 39 2012-11-15         41
## 40 2012-11-16       5441
## 41 2012-11-17      14339
## 42 2012-11-18      15110
## 43 2012-11-19       8841
## 44 2012-11-20       4472
## 45 2012-11-21      12787
## 46 2012-11-22      20427
## 47 2012-11-23      21194
## 48 2012-11-24      14478
## 49 2012-11-25      11834
## 50 2012-11-26      11162
## 51 2012-11-27      13646
## 52 2012-11-28      10183
## 53 2012-11-29       7047
```


```r
png(file="figures/histogram.png",width=480,height=480)
phist <- ggplot(totalPerDay, aes(x=TotalSteps)) + geom_histogram()
phist+xlab("Steps")+ggtitle("Histogram of Total Steps Per Day")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```r
invisible(dev.off())
```
![histogram](figures/histogram.png)

```r
meanSteps <- sprintf("%0.f",mean(totalPerDay$TotalSteps))
medianSteps <- sprintf("%0.f",median(totalPerDay$TotalSteps))
```

###### Mean of number of steps per day : 10766
###### Median of number of steps per day : 10765

## What is the average daily activity pattern?
#### The average number of steps taken in 5-minute intervals, averaged across all days 

```r
meanPerInterval <- mean.calculation(ac)
```

```r
png(file="figures/average.png",width=480,height=480)
ggplot(meanPerInterval, aes(x=fInterval, y=totalSteps)) + geom_line()
# plot(meanPerInterval$fInterval,meanPerInterval$totalSteps,type='n',xlab="Time of Day",ylab="Average number of steps")
# lines(meanPerInterval$fInterval,meanPerInterval$totalSteps)
invisible(dev.off())
```
![average](figures/average.png)

#### Locate the maximum number of average steps and the interval in which it occurs

```r
maxAverageSteps <- max(meanPerInterval$totalSteps)
maxStepsAt <- subset(meanPerInterval,subset=(meanPerInterval$totalSteps==maxAverageSteps))$interval
```
###### Max average number of steps: 206.1698
###### Interval in which it occur: 835 



## Imputing missing values


```r
naindex <- which(is.na(activity)==TRUE)
nNa <- length(naindex)
```
###### The total number of missing values in the dataset: 2304



#### Create data frame with imputed missing values
###### Imputing strategy
Replace NA with the mean value of the corresponding 5 minute slot

```r
# Copied from post: https://class.coursera.org/repdata-004/forum/thread?thread_id=82
# Thank You, Greg Tyler
# Function to replace NA by value from a data frame
# technique to replace NA with mean by subset in R and the impute.mean function 
# described at http://stackoverflow.com/a/9322975/3657371

# create a new dataset that is equal to the original dataset, but with the 
# missing data filled in

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity.imputed <- plyr::ddply(activity[1:3], .(interval), transform,
                                steps = impute.mean(steps),
                                date = date,
                                interval = interval)
# sort by date and interval
activity.imputed <- activity.imputed[order(activity.imputed$date,
                                           activity.imputed$interval),]
```

```r
head(activity.imputed)
```

```
##       steps       date interval
## 1   1.71698 2012-10-01        0
## 62  0.33962 2012-10-01        5
## 123 0.13208 2012-10-01       10
## 184 0.15094 2012-10-01       15
## 245 0.07547 2012-10-01       20
## 306 2.09434 2012-10-01       25
```

## What is mean total number of steps taken per day?

```r
ai <- activity.imputed
totalPerDayi <- aggregate(x = ai$steps, by = list(factor(ai$date)), FUN = "sum")
colnames(totalPerDayi) <- c("Date","TotalSteps")
```
#### Total number of steps taken per day


```r
totalPerDayi
```

```
##          Date TotalSteps
## 1  2012-10-01      10766
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08      10766
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## 11 2012-10-11      10304
## 12 2012-10-12      17382
## 13 2012-10-13      12426
## 14 2012-10-14      15098
## 15 2012-10-15      10139
## 16 2012-10-16      15084
## 17 2012-10-17      13452
## 18 2012-10-18      10056
## 19 2012-10-19      11829
## 20 2012-10-20      10395
## 21 2012-10-21       8821
## 22 2012-10-22      13460
## 23 2012-10-23       8918
## 24 2012-10-24       8355
## 25 2012-10-25       2492
## 26 2012-10-26       6778
## 27 2012-10-27      10119
## 28 2012-10-28      11458
## 29 2012-10-29       5018
## 30 2012-10-30       9819
## 31 2012-10-31      15414
## 32 2012-11-01      10766
## 33 2012-11-02      10600
## 34 2012-11-03      10571
## 35 2012-11-04      10766
## 36 2012-11-05      10439
## 37 2012-11-06       8334
## 38 2012-11-07      12883
## 39 2012-11-08       3219
## 40 2012-11-09      10766
## 41 2012-11-10      10766
## 42 2012-11-11      12608
## 43 2012-11-12      10765
## 44 2012-11-13       7336
## 45 2012-11-14      10766
## 46 2012-11-15         41
## 47 2012-11-16       5441
## 48 2012-11-17      14339
## 49 2012-11-18      15110
## 50 2012-11-19       8841
## 51 2012-11-20       4472
## 52 2012-11-21      12787
## 53 2012-11-22      20427
## 54 2012-11-23      21194
## 55 2012-11-24      14478
## 56 2012-11-25      11834
## 57 2012-11-26      11162
## 58 2012-11-27      13646
## 59 2012-11-28      10183
## 60 2012-11-29       7047
## 61 2012-11-30      10766
```


```r
png(file="figures/histogrami.png",width=480,height=480)
#hist(totalPerDayi$TotalSteps,main ="Histogram of Total Steps Per Day", xlab="Steps",breaks=10)
phist <- ggplot(totalPerDayi, aes(x=TotalSteps)) + geom_histogram()
phist+xlab("Steps")+ggtitle("Histogram of Total Steps Per Day")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```r
invisible(dev.off())
```
![histogram](figures/histogrami.png)

```r
meanStepsi <- sprintf("%0.f",mean(totalPerDayi$TotalSteps))
medianStepsi <- sprintf("%0.f",median(totalPerDayi$TotalSteps))
```

###### Mean of number of steps per day : 10766
###### Median of number of steps per day : 10766
#### ANALYSIS 
No significant difference between data with NA removed and data with imputed values in place of NA 



## Are there differences in activity patterns between weekdays and weekends?

```r
wd <- factor(as.POSIXlt(ai$date)$wday %in% c(0,6),labels=c("weekday","weekend"))
ai1<-cbind(ai,dayType = wd)
a.weekend <- subset(ai1,subset=ai1$dayType=="weekend")
a.weekday <- subset(ai1,subset=ai1$dayType=="weekday")
meanPerInterval.weekend <- mean.calculation(a.weekend)
meanPerInterval.weekday <- mean.calculation(a.weekday)
meanPerInterval.weekend <- cbind(meanPerInterval.weekend,c("weekend"))
meanPerInterval.weekday <- cbind(meanPerInterval.weekday,c("weekday"))
colnames(meanPerInterval.weekend)<-c("interval","steps","fInterval","dayType")
colnames(meanPerInterval.weekday)<-c("interval","steps","fInterval","dayType")
meanPerInterval.both <- rbind(meanPerInterval.weekday,meanPerInterval.weekend)
png(file="figures/week.days.end.png",width=480,height=480)
p<-ggplot(meanPerInterval.both, aes(x=fInterval, y=steps)) + geom_line()+geom_smooth(method=loess)+ggtitle("Comparison of Activites on Weekdays and Weekends")
p+facet_grid(dayType~.)

invisible(dev.off())	
```
![week.comparison](figures/week.days.end.png)  

#### ANALYSIS
The comparison between activity on weekdays versus on weekends is shown in the graphs above along with the smoothed curves, It is clear that on weekdays the activity is most in the early hours (before 10:00 am) where as on weekends it is more distributed through out the day.


