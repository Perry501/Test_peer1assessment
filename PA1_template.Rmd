#Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Loading and Preprocessing the data

Make sure the required packages are loaded into R, then read in the dataset. Finally, convert the data frame into data table format.

```{r}

library(dplyr)
library(ggplot2)
library(data.table)

setwd("~/Data Science Course/Reproducible Research/Week 2")
file <- "activity.csv"
df <- read.csv(file, header = TRUE, colClasses = c("integer", "Date", "factor"))
dt <- as.data.table(df)

```

##What is mean total number of steps taken per day

First, calculate the total number of steps taken per day. Then plot a histogram of the results, bucketing the data by each 1,000 steps.

```{r}

dt[ ,':='(TotalStepsPerDay = sum(steps)), by = date]
plot1 <- distinct(dt[, .(date, TotalStepsPerDay)])

        p1 <- ggplot(plot1, aes(x = TotalStepsPerDay))
        p1 + geom_histogram(binwidth = 1000, color = "black", fill = "red") +
                labs(title = "Histogram of Total Steps Taken Each Day", 
                     x = "Total Steps in the Day", y = "Frequency") +
                theme_bw()

```

Finally, calculate the mean and the median of the total number of steps per day

```{r}

mean1 <- mean(plot1$TotalStepsPerDay, na.rm = TRUE)

median1 <- median(plot1$TotalStepsPerDay, na.rm = TRUE)

```

The mean average number of steps per day is `r mean1`. The median number of steps per day is `r median1`.

##What is the average daily activity pattern?

First, calculate the average steps taken at each 5-minute interval across all the days. Then produce a line graph that'll display the results.

```{r}

dt[ , ':='(AverageStepsPerInterval = mean(steps, na.rm = T)), by = interval]
plot2 <- distinct(dt[, .(interval, AverageStepsPerInterval)])
plot2$interval <- factor(plot2$interval, levels = plot2$interval[1:288] )


        p2 <- ggplot(plot2, aes(x = interval, y = AverageStepsPerInterval))
        p2 + geom_point(stat = "identity") +
                geom_line(group = 1) +
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                labs(title = "Average Daily Activity Pattern", 
                     x = "5-minute Interval", y = "Average Number of Steps") +
                scale_x_discrete(breaks = seq(0, 2355, 100))
        
```

Which interval has the maximum average number of steps?

```{r}

plot2[plot2$AverageStepsPerInterval == max(plot2$AverageStepsPerInterval), ]

```

##Imputing missing values

How many missing values are there?

```{r}

sum(is.na(dt$steps))

```

As the missing values are 8 full days, I will fill these missing values with the average daily activity pattern that was previously calculated.

```{r}

FullData <- as.data.table(df)
for (i in 1:nrow(FullData)) {
        if(is.na(FullData$steps[i])) {
                FullData$steps[i] <- plot2[which(FullData$interval[i] == plot2$interval), ]$AverageStepsPerInterval
        }
}

```

Now a new dataset exists with no missing values. I'll reproduce the histogram from earlier, but use the FullData dataset.

```{r}

FullData[ ,':='(TotalStepsPerDay = sum(steps)), by = date]
plot3 <- distinct(FullData[ , .(date, TotalStepsPerDay)])

        p3 <- ggplot(plot3, aes(x = TotalStepsPerDay))
        p3 + geom_histogram(binwidth = 1000, color = "black", fill = "red") +
                labs(title = "Histogram of Total Steps Taken Each Day", 
                     x = "Total Steps in the Day", y = "Frequency") +
                theme_bw()
        
```

Lets check to see if the mean and median has been affected.

```{r}

mean2 <- mean(plot3$TotalStepsPerDay, na.rm = TRUE)
        
median2 <- median(plot3$TotalStepsPerDay, na.rm = TRUE)

meanDiff <- mean2 - mean1

medianDiff <- median2 - median1

```

The difference between the mean values is `r meanDiff`. The difference between the median values is an increase of `r medianDiff`.

##Are there differences in activity patterns between weekdays and weekends?

To see if there is a difference in activity patterns between weekdays and weekends, we'll first have to determine which days are weekdays/weekends.

```{r}

FullData$weekdays <- factor(format(FullData$date, "%A"))
levels(FullData$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday"),
                                  weekend = c("Saturday", "Sunday"))

```

Once the numbers have been re-calculated, a plot is produced that'll illustrate any differences in the patterns.

```{r}

FullData[ , ':='(AverageStepsPerInterval = mean(steps)), by = .(interval, weekdays)]
plot4 <- distinct(FullData[ , .(weekdays, interval, AverageStepsPerInterval)])
plot4$interval <- factor(plot4$interval, levels = plot4$interval[1:288] )

        p4 <- ggplot(plot4, aes(x = interval, y = AverageStepsPerInterval))
        p4 + geom_point(stat = "identity") +
               geom_line(group = 1) +
                facet_grid(weekdays ~ . ) +
               theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
              labs(title = "Average Daily Activity Pattern", 
                  x = "5-minute Interval", y = "Average Number of Steps") +
            scale_x_discrete(breaks = seq(0, 2355, 100))
        
```
