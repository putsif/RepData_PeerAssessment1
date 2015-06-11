## reade the data and parse the date
library(lubridate)
data <-read.csv("./data/activity.csv")
data <- transform(data, date = ymd(as.character(date)))

# ## group by day and calculate total steps per day.
# Plot a historgram of total steps per day.
# Calculate the mean and median of the total steps across all the days.

library(dplyr)
data_by_date <- group_by(data, date)
total_steps <- summarise(data_by_date, total_steps = sum(steps, na.rm = TRUE))
plot(total_steps, type = "h", xlab = "Date", ylab = "Total Steps")
mean_steps <- mean(total_steps$total_steps)
median_steps <- median(total_steps$total_steps)
print(c("The mean total steps per day is ", mean_steps), quote = FALSE)
print(c("The median total steps per day is ", median_steps), quote = FALSE)

# group by interval and average the number of steps per interval across the
# days. 
data_by_int <- group_by(data, interval)
interval_avg <- summarise(data_by_int, mean = mean(steps, na.rm = TRUE))
plot(interval_avg, type = "l", ylab = "Mean steps", xlab = "Interval")
print(c("The interval with the highest average number of steps is ", most_active)
      quote = FALSE)

# calculate the average number of steps taken across the days
# during each interval. Add it to the dataset in a column called 
# "abisteps", which means average by interval steps. 
#Calcuate which interval has the highest mean steps. 
data <- transform(data, interval = as.factor(interval))
data <- transform(data, abisteps = ave(data$steps, interval,
                                       FUN = function(x) mean (x, na.rm = TRUE)))
most_active <- interval_avg[which(interval_avg$mean == 
                                    max(interval_avg$mean)),1]

# calulate and report the number of missing values in the dataset.
print(c("The number of missing values in the dataset is ",sum(is.na(data$steps))),
        quote = FALSE)

#create an interpolated dataset where NA values in the steps column
#are replaced with the average number of steps taken during that interval.
interpolated_data <- transform(data, steps = ifelse(is.na(steps), 
                                                    abisteps, steps))

# plot histograms of the average number of steps per day for both the
# original data and the interpolated data. 
