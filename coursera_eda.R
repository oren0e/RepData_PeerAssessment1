setwd("/Users/corel/Dropbox/Docs/Data analysis/coursera data science/reproduicabale_research/RepData_PeerAssessment1")

library("dplyr")
library("ggplot2")
# data reading
mydata <- read.csv("./activity.csv", stringsAsFactors = F, header = T)
str(mydata)
mydata$date <- as.Date(mydata$date)

# total number of steps per day
mydata %>% group_by(date) %>% summarise(steps_per_day = sum(steps, na.rm = T))
mydata %>% group_by(date) %>% summarise(steps_per_day = sum(steps, na.rm = T)) %>% 
  ggplot(aes(x = steps_per_day)) +
    geom_histogram(stat = "bin",fill = "brown", color = "black")

mydata %>% group_by(date) %>% summarise(steps_per_day = sum(steps, na.rm = T)) %>% ungroup() %>% summarise(mean = mean(steps_per_day),
                                                                                                           median = median(steps_per_day))
# time series plot
mydata %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm = T)) %>% ungroup() %>% 
  ggplot(aes(x = interval, y = avg_steps)) +
    geom_line() +
    scale_x_continuous(breaks = seq(0,2500,100)) +
    labs(title = "Average Daily Activity", x = "Interval", y = "Average Steps")

# Imputing missing values
sum(is.na(mydata$steps))

# fill NA with median of that interval
mydata_no_missing <- mydata %>% group_by(interval) %>% mutate(med_day = median(steps, na.rm = T)) %>% ungroup()
mydata_no_missing$steps[is.na(mydata_no_missing$steps)] <- mydata_no_missing$med_day[is.na(mydata_no_missing$steps)]

mydata_no_missing %>% group_by(date) %>% summarise(total_steps = sum(steps),
                                                   mean_steps = mean(steps),
                                                   med_steps = median(steps)) %>% 
  ggplot(aes(x = total_steps)) +
    geom_histogram(fill = "brown", color = "black")
mydata_no_missing$med_day <- NULL

# differences in activity
mydata_no_missing <- mydata_no_missing %>% mutate(day_of_week = weekdays(date)) %>%
                                           mutate(week_time = ifelse(day_of_week %in% c("Friday","Saturday","Sunday"),"weekend","weekday"))
mydata_no_missing$week_time <- as.factor(mydata_no_missing$week_time)
mydata_no_missing$day_of_week <- NULL

mydata_no_missing %>% group_by(week_time,interval) %>% summarise(avg_steps = mean(steps)) %>% 
  ggplot(aes(x = interval, y = avg_steps)) +
    geom_line() +
    facet_grid(rows = vars(week_time)) +
    labs(title = "Weekend vs. Weekday", subtitle = "Are there any differences?", x= "Interval", y = "Average Steps")




