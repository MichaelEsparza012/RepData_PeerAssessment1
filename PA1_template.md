Git Hub URL: <https://github.com/MichaelEsparza012/RepData_PeerAssessment1>
---------------------------------------------------------------------------

Specfied Tasks for this Assignment:
-----------------------------------

### Loading and preprocessing the data

#### 1. Show any code that is needed to Load the data

#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

##### Download the data frame from the course website: <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")

#### Unzip the datafiles, extract and read for confirmation

    unzip("activity.zip")
    stepdata <- read.csv("activity.csv", header = TRUE)
    head(stepdata)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

    library(magrittr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

#### 2. Make a histogram of the total number of steps taken each day (a Bar Chart assigns Categorical Values, a Histogram assigns Quantitative Values)

    databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
    hist(databydate$tsteps, xlab = "Total Daily Steps",main="Histogram of Total Number of Steps Taken Each Day", breaks = 20)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

#### 3. Calculate and report the **mean** and **median** of the total number of steps taken per day

#### Mean

    mean(databydate$tsteps)

    ## [1] 10766.19

#### Median

    median(databydate$tsteps)  

    ## [1] 10765

### What is the average daily activity pattern?

#### 1. Make a time series plot (i.e., type = 1) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    library(ggplot2)
    databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
    ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]

    ## # A tibble: 1 x 2
    ##   interval tsteps
    ##      <int>  <dbl>
    ## 1      835   206.

### Imputing Missing Values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    missingVals <- sum(is.na(data))

    ## Warning in is.na(data): is.na() applied to non-(list or vector) of type
    ## 'closure'

    missingVals

    ## [1] 0

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. **The startegy to be used for this project to fill in the missing values in the dataset is based on using the mean of the 5-minute interval**

#### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in

    library(magrittr)
    library(dplyr)

    replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
    head(meandata)

    ## # A tibble: 6 x 3
    ## # Groups:   interval [6]
    ##    steps date       interval
    ##    <dbl> <fct>         <int>
    ## 1 1.72   2012-10-01        0
    ## 2 0.340  2012-10-01        5
    ## 3 0.132  2012-10-01       10
    ## 4 0.151  2012-10-01       15
    ## 5 0.0755 2012-10-01       20
    ## 6 2.09   2012-10-01       25

    FillSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

    names(FillSummedDataByDay)[1] ="date"
    names(FillSummedDataByDay)[2] ="totalnumberofstepsperday"
    head(FillSummedDataByDay,15)

    ##          date totalnumberofstepsperday
    ## 1  2012-10-01                 10766.19
    ## 2  2012-10-02                   126.00
    ## 3  2012-10-03                 11352.00
    ## 4  2012-10-04                 12116.00
    ## 5  2012-10-05                 13294.00
    ## 6  2012-10-06                 15420.00
    ## 7  2012-10-07                 11015.00
    ## 8  2012-10-08                 10766.19
    ## 9  2012-10-09                 12811.00
    ## 10 2012-10-10                  9900.00
    ## 11 2012-10-11                 10304.00
    ## 12 2012-10-12                 17382.00
    ## 13 2012-10-13                 12426.00
    ## 14 2012-10-14                 15098.00
    ## 15 2012-10-15                 10139.00

#### 4. Make a histogram of the total number of steps taken each day

    hist(FillSummedDataByDay$totalnumberofstepsperday, xlab = "Steps", ylab = "Frequency", main = "Histogram of Total Number of Steps Per Day", breaks = 20)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

#### Calculate and report the mean and median total number of steps taken per day

    mean(FillSummedDataByDay$totalnumberofstepsperday)

    ## [1] 10766.19

    median(FillSummedDataByDay$totalnumberofstepsperday)

    ## [1] 10766.19

#### Do these values differ from the estimates from the first part of the assignment?

    oldmean <- mean(databydate$tsteps, na.rm = TRUE)
    newmean <- mean(FillSummedDataByDay$totalnumberofstepsperday)

    oldmean

    ## [1] 10766.19

    newmean

    ## [1] 10766.19

#### **Based on the mean values returned, *oldmean and newmean*, the values DO NOT differ from the estimates from the first part of the assignment**

#### What is the impact of imputing missing data on the estimates of the total daily number of steps? **The impact of imputing missing data on the estimates on the total daily number of steps is NEGLIGIBLE**

### Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    meandata$date <- as.Date(meandata$date)
    meandata$weekday <- weekdays(meandata$date)
    meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday")

#### Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    library(ggplot2)
    meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
    names(meandataweekendweekday) <- c("weekend", "interval", "steps")

    ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
    facet_grid(weekend ~.) + xlab("5-Min Interval") + ylab("average across all weekdays and weekends") +
        ggtitle("Time Series Plot - Average Number of Steps Taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-18-1.png)
