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

