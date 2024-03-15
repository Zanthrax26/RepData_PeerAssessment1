excel_data <- read.csv(file="activity.csv", header=TRUE, nrows=17569)
head(excel_data)
names(excel_data)
new_excel_data <- excel_data[complete.cases(excel_data),]
head(new_excel_data)

new_excel_data$date <- as.Date(new_excel_data$date, "%Y-%m-%d")
head(new_excel_data)
new_excel_data$interval <- as.factor(new_excel_data$interval)
head(new_excel_data)



dailysteps <- tapply(new_excel_data$steps, new_excel_data$date, sum)

qplot(dailysteps, geom="histogram", xlab='Total steps per day', ylab='Frequency of Occurrence')

MeanDailySteps <- mean(dailysteps)
MedianDailySteps <- median(dailysteps)




interval_steps <- aggregate(steps ~ interval, data = new_excel_data, FUN = mean)
interval_steps$interval <- as.integer(levels(interval_steps$interval)[interval_steps$interval])
ggplot(interval_steps, aes(x = interval, y = steps)) + geom_line(col = "red", size = 1) + labs(x = "5-minute interval", y = "Average Steps")

Interval_max_steps <- interval_steps[which.max(interval_steps$steps),]





missing_values <- sum(is.na(excel_data$steps))

updated_excel_data <- excel_data
na_index <- which(is.na(updated_excel_data$steps))
  for (i in na_index) {
    index <- updated_excel_data$interval[i]
   updated_excel_data$steps[i] <- interval_steps$steps[interval_steps$interval == index]
  }
updated_missing_values <- sum(is.na(updated_excel_data$steps))
updated_missing_values

updated_daily_steps <- aggregate(steps ~ date, data = updated_excel_data, FUN=sum)
ggplot(updated_daily_steps, aes(x = steps)) + geom_histogram(fill = "red") + labs(x = "Steps Per Day", y = "Frequency of Occurrence")

mean_updated_daily_steps <- mean(updated_daily_steps$steps)
median_updated_daily_steps <- median(updated_daily_steps$steps)



updated_excel_data$dateType <-  ifelse(as.POSIXlt(updated_excel_data$date)$wday %in% c(0,6), 'weekend', 'weekday')

aggregated_updated_excel_data <- aggregate(steps ~ interval + dateType, data=updated_excel_data, mean)
ggplot(aggregated_updated_excel_data, aes(interval, steps)) + geom_line() + facet_grid(dateType ~ .) +xlab("5-minute interval") + ylab("avarage number of steps")


