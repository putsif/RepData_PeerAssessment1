## read the data and parse the date
library(lubridate)
data <-read.csv("./activity/activity.csv")
data <- transform(data, date = ymd(as.character(date)))

# ## group by day and calculate total steps per day.
# Plot a historgram of total steps per day.
# Calculate the mean and median of the total steps across all the days.

library(dplyr)
library(lattice)
data_by_date <- group_by(data, date)
total_steps <- summarise(data_by_date, total_steps = sum(steps, na.rm = TRUE))
histogram(total_steps$total_steps, xlab = "Total Steps")
mean_steps <- mean(total_steps$total_steps)
median_steps <- median(total_steps$total_steps)
print(c("The mean total steps per day is ", mean_steps), quote = FALSE)
print(c("The median total steps per day is ", median_steps), quote = FALSE)

# group by interval and average the number of steps per interval across the
# days. 
data_by_int <- group_by(data, interval)
interval_avg <- summarise(data_by_int, mean = mean(steps, na.rm = TRUE))

plot(interval_avg, type = "l", ylab = "Mean steps", xlab = "Interval")


# calculate the average number of steps taken across the days
# during each interval. Add it to the dataset in a column called 
# "abisteps", which means average by interval steps. 
#Calcuate which interval has the highest mean steps. 
data <- transform(data, interval = as.factor(interval))
data <- transform(data, abisteps = ave(data$steps, interval,
                                       FUN = function(x) mean (x, na.rm = TRUE)))
most_active <- interval_avg[which(interval_avg$mean == 
                                    max(interval_avg$mean)),1]
print(c("The interval with the highest average number of steps is ", most_active),
      quote = FALSE)

# calulate and report the number of missing values in the dataset.
print(c("The number of missing values in the dataset is ",sum(is.na(data$steps))),
        quote = FALSE)

#create an interpolated dataset where NA values in the steps column
#are replaced with the average number of steps taken during that interval.
interpolated_data <- transform(data, steps = ifelse(is.na(steps), 
                                                    abisteps, steps))
interpolated_data <- select(interpolated_data, steps, date, interval)


# plot histograms of the average number of steps per day for both the
# original data and the interpolated data. 
interpolated_by_date <- group_by(interpolated_data, date)
interpolated_total_steps <- summarise(interpolated_by_date, 
                                      total_steps = sum(steps, na.rm = TRUE))
interpolated_total_steps <- mutate(interpolated_total_steps, int = "Interpolated")
total_steps <- mutate(total_steps, int = "No Interpolation")
hist_data <- rbind(total_steps, interpolated_total_steps)
histogram(~ hist_data$total_steps | hist_data$int, xlab = "Total Steps")


int_mean_steps <- mean(interpolated_total_steps$total_steps)
int_median_steps <- median(interpolated_total_steps$total_steps)
print(c("The mean total steps per day is ", mean_steps), quote = FALSE)
print(c("The mean total steps per day for the interpolated data is ", 
        int_mean_steps), quote = FALSE)
print(c("The difference is ", mean_steps - int_mean_steps), quote = FALSE)
print(c("The median total steps per day is ", median_steps), quote = FALSE)
print(c("The median total steps per day for the interpolated data is ", 
        int_median_steps), quote = FALSE)
print(c("The difference is ", median_steps - int_median_steps), quote = FALSE)

## compare weekday and weekend activity levels.
## Add a factor to the interpolated data that labels each reading as "weekday
## or "weekend". Group the data by that factor and by interval. Sum the steps.

w_int_data <- transform(interpolated_data, 
                        w = as.factor(ifelse(wday(date)== 7 | wday(date)== 1,
                                             "weekend", "weekday")))
wdata_grouped <- group_by(w_int_data, w, interval)
wdata_sum <- summarise(wdata_grouped, steps = mean(steps, na.rm = TRUE))

## graph the two subsets using lattice xyplot. 

p <- xyplot(steps ~ interval | w, wdata_sum, layout = c(1,2), type = "l")
print(p)
