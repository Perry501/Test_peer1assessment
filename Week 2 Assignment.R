library(dplyr)
library(ggplot2)
library(data.table)

setwd("~/Data Science Course/Reproducible Research/Week 2")
file <- "activity.csv"
df <- read.csv(file, header = TRUE, colClasses = c("integer", "Date", "factor"))
dt <- as.data.table(df)

######################

dt[ ,':='(TotalStepsPerDay = sum(steps)), by = date]
plot1 <- distinct(dt[, .(date, TotalStepsPerDay)])

        p1 <- ggplot(plot1, aes(x = TotalStepsPerDay))
        p1 + geom_histogram(binwidth = 1000, color = "black", fill = "red") +
                labs(title = "Histogram of Total Steps Taken Each Day", 
                     x = "Total Steps in the Day", y = "Frequency") +
                theme_bw()
        ggsave("p1.png", height = 12, width = 12, dpi = 600)


####################

mean(plot1$TotalStepsPerDay, na.rm = TRUE)

median(plot1$TotalStepsPerDay, na.rm = TRUE)

####################

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
        ggsave("p2.png", height = 12, width = 12, dpi = 600)
        
###################
        
plot2[plot2$AverageStepsPerInterval == max(plot2$AverageStepsPerInterval), ]

###################

sum(is.na(dt$steps))

FullData <- as.data.table(df)
for (i in 1:nrow(FullData)) {
        if(is.na(FullData$steps[i])) {
                FullData$steps[i] <- plot2[which(FullData$interval[i] == plot2$interval), ]$AverageStepsPerInterval
        }
}

FullData[ ,':='(TotalStepsPerDay = sum(steps)), by = date]
plot3 <- distinct(FullData[ , .(date, TotalStepsPerDay)])

        p3 <- ggplot(plot3, aes(x = TotalStepsPerDay))
        p3 + geom_histogram(binwidth = 1000, color = "black", fill = "red") +
                labs(title = "Histogram of Total Steps Taken Each Day", 
                     x = "Total Steps in the Day", y = "Frequency") +
                theme_bw()
        ggsave("p3.png", height = 12, width = 12, dpi = 600)
        
        
####################
        
        mean(plot3$TotalStepsPerDay, na.rm = TRUE)
        
        median(plot3$TotalStepsPerDay, na.rm = TRUE)
        
##############
        
FullData$weekdays <- factor(format(FullData$date, "%A"))
levels(FullData$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday"),
                                  weekend = c("Saturday", "Sunday"))

#############

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
        ggsave("p4.png", height = 12, width = 12, dpi = 600)


        