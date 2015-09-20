#Reproducible Research Project 1
##Linda Bien 9/19/15
Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
Data

The data for this assignment can be downloaded from the course web site:

    Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

    date: The date on which the measurement was taken in YYYY-MM-DD format

    interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

##Part 1: Loading and Preprocessing the Data
###Load the data file

```r
# Read activity.csv file into variable "activity"
activity <- read.csv(file="C:/Users/Linda/Documents/Data Science/activity.csv")

# Convert date field to date form
activity$date <- as.Date(activity$date)
```

##Part 1A: What is the mean total number of steps taken per day?
###Process data

```r
# Load reshape2 library to get melt and dcast functions
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.1.3
```

```r
# Melt data frame to prep for casting by date
actMeltDate <- melt(activity, id.vars="date", measure.vars="steps", na.rm=FALSE)

# Cast data frame to obtain steps per day
actCastDate <- dcast(actMeltDate, date ~ variable, sum)
```

###Plot histogram of data

```r
# Plot histogram of total number of steps take each day
plot(actCastDate$date, actCastDate$steps, type="h", main="Histogram of Daily Steps", xlab="Date", ylab="Steps per Day", col="blue", lwd=8)
abline(h=mean(actCastDate$steps, na.rm=TRUE), col="red", lwd=2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

###Calculate mean and median of steps by day

```r
# Calculate mean and median of daily steps
paste("Mean Steps per Day =", mean(actCastDate$steps, na.rm=TRUE))
```

```
## [1] "Mean Steps per Day = 10766.1886792453"
```

```r
paste("Median Steps per Day =", median(actCastDate$steps, na.rm=TRUE))
```

```
## [1] "Median Steps per Day = 10765"
```

##Section 2: What is the average daily activity pattern?
###Reprocess data to calculate by interval instead of day

```r
# Melt data frame to prepare to cast by interval; remove NA values
actMeltInterval <- melt(activity, id.vars="interval", measure.vars="steps", na.rm=TRUE)

# Cast data frame to obain mean steps ber interval
actCastInterval <- dcast(actMeltInterval, interval ~ variable, mean)
```

###Create a time series plot of average steps by interval

```r
# Plot line chart with average frequency of steps by interval and add line to indicate mean
plot(actCastInterval$interval, actCastInterval$steps, type="l", main="Average Frequency of Steps at Each Interval", xlab="Interval", ylab="Steps", col="blue", lwd=2)
abline(h=mean(actCastInterval$steps,na.rm=TRUE), col="red", lwd=2)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


```r
# Find the interval that has the max value
paste("Interval with Max Value", actCastInterval$interval[which(actCastInterval$steps == max(actCastInterval$steps))])
```

```
## [1] "Interval with Max Value 835"
```


```r
# Find the max value 
paste("Maximum mean steps", max(actCastInterval$steps))
```

```
## [1] "Maximum mean steps 206.169811320755"
```

##Part 3: Imputing missing values
###Calculate total number of missing values in dataset

```r
# Total number of missing values
sum(is.na(activity$steps))
```

```
## [1] 2304
```

###Document strategy of dealing with missing values
I will replace NA values with the mean for that particular interval.

###Create new data set with imputed NA values as stated in strategy

```r
# Renaming data frame
stepsPerInt <- actCastInterval

# Create data frame that will remove NAs from
actNoNAs <- activity

# Merge activity data set with stepsPerInt data set
actMerge = merge(actNoNAs, stepsPerInt, by="interval", suffixes=c(".act", ".spi"))

#Get list of indexes where steps value = NA
naIndex = which(is.na(actNoNAs$steps))

# Replace NA values with value from steps.spi
actNoNAs[naIndex, "steps"] = actMerge[naIndex, "steps.spi"]
```
###Plot histogram and calculate mean and median of total steps/day with new dataset and compare wit original


```r
# Melt new data frame to prepare to cast by date
actMeltDateNoNA <- melt(actNoNAs, id.vars="date", measure.vars="steps", na.rm=FALSE)

# Cast data frame to obtain steps per day
actCastDateNoNA <- dcast(actMeltDateNoNA, date ~ variable, sum)

# Plot histogram
plot(actCastDateNoNA$date, actCastDateNoNA$steps, type="h", main="Histogram of Daily Steps (with Imputted NA Values)", xlab="Date", ylab="Steps", col="gray", lwd=8)
abline(h=mean(actCastDateNoNA$steps), col="red", lwd=2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


```r
# Calculate mean and median 
paste("Mean daily steps =", mean(actCastDateNoNA$steps, na.rm=TRUE))
```

```
## [1] "Mean daily steps = 10889.7992576554"
```

```r
paste("Median daily steps =", median(actCastDateNoNA$steps, na.rm=TRUE))
```

```
## [1] "Median daily steps = 11015"
```
Compare:

Original Date Set (with NA values):
  Mean daily steps = 10,766.19
  Median daily steps = 10,765

New Data Set (NAs imputed with mean value for that interval):
  Mean daily steps = 10,890
  Median daily steps = 11,015
  
##Part 4: Are there differences in activity patterns between weekdays and weekends?
###Create a factor variable that states whether each day is a weekday or weekend

```r
# Create new factor variable
for (i in 1:nrow(actNoNAs)) {
  if (weekdays(actNoNAs$date[i]) == "Saturday" | weekdays(actNoNAs$date[i]) == "Sunday") { actNoNAs$dayOfWeek[i] = "weekend" }
  else {
    actNoNAs$dayOfWeek[i] = "weekday"
  }
}
```

###Subset, process and plot two charts to compare weekday vs. weekend activity 

```r
# To create a plot, we must first subset the data
actWeekday <- subset(actNoNAs, dayOfWeek=="weekday")
actWeekend <- subset(actNoNAs, dayOfWeek=="weekend")

# Next, we need to process the data for our needs
actMeltWeekday <- melt(actWeekday, id.vars="interval", measure.vars="steps")
actMeltWeekend <- melt(actWeekend, id.vars="interval", measure.vars="steps")
actCastWeekday <- dcast(actMeltWeekday, interval ~ variable, mean)
actCastWeekend <- dcast(actMeltWeekend, interval ~ variable, mean)

# Load plot packages necessary to continue
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.1.3
```


```r
# Set plot area to two rows and one column, and then plot charts with mean line in each
plot1 <- qplot(actCastWeekday$interval, actCastWeekday$steps, geom="line", data=actCastWeekday, type="bar", main="Steps by Interval - Weekday", xlab="Interval ID", ylab="Number of Steps")
plot2 <- qplot(actCastWeekend$interval, actCastWeekend$steps, geom="line", data=actCastWeekend, type="bar", main="Steps by Interval - Weekend", xlab="Interval ID", ylab="Number of Steps")
grid.arrange(plot1, plot2, nrow=2)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
