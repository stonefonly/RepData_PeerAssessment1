## Loading and preprocessing the data
data=read.csv('./activity.csv')

## What is mean total number of steps taken per day?
totals=aggregate(steps~date,data,sum)
hist(totals$steps,xlab='Steps/Day',
     main = 'Total Steps Per Day')
totals.steps.mean=mean(totals$steps)
totals.steps.median=median(totals$steps)

## What is the average daily activity pattern?
avgs=aggregate(steps~interval,data,mean)
plot(avgs$interval,avgs$steps,type='l',
     xlab='Interval',ylab='#Steps',
     main='Average Steps Per Interval')
idx = which(avgs$steps==max(avgs$steps))
imax = avgs$interval[idx]

## Imputing missing values with mean for that interval
total.nas = sum(is.na(data$steps))
data2=merge(data,avgs,by='interval',suffixes=c('','.mean'))
data2=data2[order(data2$date),]
rownames(data2)=NULL

idx = is.na(data2$steps)
data2$steps[idx]=data2$steps.mean[idx]

totals2=aggregate(steps~date,data2,sum)
hist(totals2$steps,xlab='Steps/Day',
     main = 'Total Steps Per Day')
totals2.steps.mean=mean(totals2$steps)
totals2.steps.median=median(totals2$steps)

## Are there differences in activity patterns between weekdays and weekends?
data2$date=as.Date(data2$date,format = "%Y-%m-%d")
library(timeDate)
data2$whatday=ifelse(isWeekday(data2$date),'weekday','weekend')
data2$whatday=as.factor(data2$whatday)

avgs2=aggregate(steps~interval+whatday,data2,mean)
library(lattice)
xyplot(steps~interval|whatday,avgs2,type='l',layout=c(1,2),
       xlab='Interval',ylab='Number of Steps')
