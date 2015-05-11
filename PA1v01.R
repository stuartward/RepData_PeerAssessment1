

# Assumes reader has loaded the data file into working directory


activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")

totalByDay <-aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

hist(totalByDay$x, xlab="Total Steps", main="Histogram - Total Number of Steps per Day")

mean(totalByDay$x)

median(totalByDay$x)


meanByInterval <-aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)

plot(meanByInterval$x, type="l", xlab="Interval Number", ylab="Mean Number of Steps")

maxInterval <- which.max(meanByInterval$x)

maxInterval

#there are 2304 missing values
summary(activity)

#utlize mean for entire dataset for imputed values
mean(activity$steps)

require(Hmisc)
imputedActivity <- activity
imputedActivity$steps <- with(imputedActivity, impute(steps, mean))

imputedTotalByDay <-aggregate(imputedActivity$steps, by=list(imputedActivity$date), FUN=sum)
hist(imputedTotalByDay$x, xlab="Total Steps", main="Histogram - Total Number of Steps per Day (Imputed)")

mean(totalByDay$x)
mean(imputedTotalByDay$x)
median(totalByDay$x)
median(imputedTotalByDay$x)


activityCompare <- data.frame(date=activity$date, dayofweek=tolower(weekdays(activity$date)), steps=activity$steps, interval=activity$interval)

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")

activityCompare$dayType = as.factor(ifelse(is.element(weekdays(as.Date(activityCompare$date)),weekdays), "weekday", "weekend"))

head(activityCompare)

activityCompareDayType <- aggregate(steps ~ interval + dayType, activityCompare, mean)

library(lattice)

activityCompareDayType

xyplot(activityCompareDayType$steps ~ activityCompareDayType$interval | activityCompareDayType$dayType, type="l", layout=c(1,2))



