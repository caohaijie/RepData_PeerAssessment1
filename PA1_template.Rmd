---
title: "RepData Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**Background of this project: **

This project makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
### Assignment
Show any code that is needed to  

1. Load the data (i.e. read.csv())  
2. Process/transform the data (if necessary) into a format suitable for your analysis  

### Answers
You will find the data link here : [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

```{r loadData}
filename<-"Activity_monitoring_data.zip"
if(!file.exists(filename)){
        urlfile<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(urlfile,destfile = filename,method = "curl")
}

## zipped the data file
if(!file.exists("activity.csv")){ 
        unzip(filename)
}
```

First, we have to unzip the data, then read it with 'read.csv()'. 

```{r readCSV}
activity<-read.csv("activity.csv")
```

The variables included in this dataset are:  
1. **steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)  
2. **date:** The date on which the measurement was taken in YYYY-MM-DD format  
3. **interval:** Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

After that, let's transform dates to the date format and remouve the missing data

```{r processing}
activity$date<-as.Date(activity$date)

## remove the NA 
activityN<-activity[!is.na(activity$steps),]
```

## Question: What is mean total number of steps taken per day ? 
### Assignment
For this part of the assignment, you can ignore the missing values in the dataset.  

1. Calculate the total number of steps taken per day  
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

### Answers
We are going to use the 'tapply()' function to count total steps per day, then plot those using the base plotting systerm. The mean and median will be displayed following the plot.

```{r histogramWithOutNA}
stepDate<-tapply(activityN$steps, activityN$date, sum)
hist(stepDate,
     breaks = 50,
     main="histogram of the total number of step per day",
     xlab="the numbers of steps per day ")
```


```{r mean_and_median_withoutNA,results="asis"}
library(xtable)
Mean<-round(mean(stepDate),digits = 0)
Median<-round(median(stepDate),digits = 0)
table<-cbind(Mean,Median)
rownames(table)<-"numbers of steps"
xt<-xtable(table)
print(xt,type="html")
```


## What is the average daily activity pattern?
### Assignment
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

### Answers
1. We use 'tapply()' to count average number of steps per interval, Then use the base plotting system.
```{r timeSeries}
Interval<-round(tapply(activityN$steps, activityN$interval, mean),digits = 0)

## make the time series plot
plot(names(Interval),
     Interval,
     type=("l"),
     lwd=3,
     main="time series plot with 5-minute interval",
     xlab="interval",
     ylab="steps"
     )
```

2. Interval with maximum number of steps on average
```{r maxStepInterval}
## calcul the max numbers of steps in the 5-minute interval
maxInterval<-round(tapply(activityN$steps, activityN$interval, max),digits = 0)

MaxStep<-max(maxInterval)
nameInterval<-names(maxInterval[maxInterval==MaxStep])
```

The interval of `r nameInterval`, on average across all the days in the dataset, contains the maiximum number of steps, which is `r MaxStep`.

## Imputing missing values
### Assignment
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

### Answers
1. Total number of missing values in the dataset
```{r missingValue}
na<-sum(is.na(activity$steps))
```

The total number of missing values in the dataset is `r na`.

2. We will use the the mean for that 5-minute interval as the strategy to fill the missing values. 
```{r fillMissingValue}
activity2<-activity
## We have already calculed the average steps per the 5-minute interval (Interval)
steps<-as.vector(Interval)
inter<-as.integer(names(Interval))
lsAverageSteps<-as.data.frame(cbind(inter,steps))

## make a function that find the number of steps coresponds to the interval
find<-function(choix){
        for (i in 1:nrow(lsAverageSteps)){
                if (lsAverageSteps[i,1]==choix){
                        value<-lsAverageSteps[i,2]
                }
        }
        value
}

## subset les NA data
dataNA<-activity2[is.na(activity2$steps),]
## replace the NA data by the mean for that 5-minute interval
i<-1
while (i <= nrow(dataNA)){
        dataNA[i,1]<-find(dataNA$interval[i])
        i<-i+1
}
## replace the na data in the activity by the dataNA
activity2$steps[is.na(activity2$steps)]<-dataNA$steps
```

Now all the missing values in the "steps" rows are imputed. 

Histogram of total steps per day with this new dataset, using the same method as before.
```{r histogram}
stepTotal<-tapply(activity2$steps, activity2$date, sum)
hist(stepTotal,
     breaks = 50,
     main="histogram of the total number of step per day",
     xlab="the numbers of steps per day ")
```

Calcul the mean and median number of the total steps taken each day after missing values are inputed.

```{r mean_and_median,results="asis"}
library(xtable)
Mean<-round(mean(stepTotal),digits = 0)
Median<-round(median(stepTotal),digits = 0)
tableSansNA<-cbind(Mean,Median)
rownames(table)<-"numbers of steps"
xtable<-xtable(tableSansNA)
print(xtable,type="html")
```


## Are there differences in activity patterns between weekdays and weekends?
### Assignment
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.  

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

### Answers
1. To create the new variable, we will not use the 'weekdays()' function beacuse it depends on the system language.
```{r weekdays}
activity2$weekdays<-weekdays(activity$date)
activity2$weekdays<-gsub(pattern = "Lundi",replacement = "weekdays",activity2$weekdays) 
activity2$weekdays<-gsub(pattern = "Mardi",replacement = "weekdays",activity2$weekdays) 
activity2$weekdays<-gsub(pattern = "Mercredi",replacement = "weekdays",activity2$weekdays) 
activity2$weekdays<-gsub(pattern = "Jeudi",replacement = "weekdays",activity2$weekdays) 
activity2$weekdays<-gsub(pattern = "Vendredi",replacement = "weekdays",activity2$weekdays)
activity2$weekdays<-gsub(pattern = "Samedi",replacement = "weekends",activity2$weekdays)
activity2$weekdays<-gsub(pattern = "Dimanche",replacement = "weekends",activity2$weekdays)
activity2$weekdays<-factor(activity2$weekdays,levels = c("weekdays","weekends"))
```

2. Comparing average steps taken per 5 minute interval during weekdays and weekend.
```{r compare}
avgSteps<-tapply(activity2$steps,list(activity2$interval,activity2$weekdays),mean,na.rm=TRUE)
par(mfcol=c(2,1),mar=c(3,4,1,0))
plot(names(avgSteps[,1]),avgSteps[,1],type='l',xlab='interval',ylab='steps')
title('Weekday')
plot(names(avgSteps[,2]),avgSteps[,2],type='l',xlab='interval',ylab='steps')
title('Weekend')
```

Subject is more active in the morning during weekdays, while his/her activity is more uniform during weekends.