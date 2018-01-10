#Script for Reproducible Research Project Week1 from FVB.

#The script uses the "dplyr" and "ggplot2" packages and so it checks their 
#presence and installs them in case they're absent.
if (!("dplyr" %in% installed.packages()[,1])) {install.packages("dplyr")}
if (!("ggplot2" %in% installed.packages()[,1])) {install.packages("ggplot2")}
library(dplyr)
library(ggplot2)

#It's seems that the script should run in our directory before the raw data
#has been downloaded in our working directory, and its file hasn't been 
#extracted yet, so we download it, unzip it, and then we extract its values.

download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              destfile = "./monitordata.zip")
unzip(zipfile = "monitordata.zip")
monitordata<-read.csv(file = "./activity.csv",col.names = c("steps","date","interval"))

# You can check that the way the interval its registered is unnatural since 
# every hour it restars a 0-55 count and adds 100 to its values. We correct this
# so we get values of interval that look more natural: the accumulation of
# consecutive increases of 5 min, from 0 (=00:00) to 1435 (23:55).
monitordata$interval <-monitordata$interval-(40*trunc(monitordata$interval/100))

# The first question to be addressed is :
# "What is mean total number of steps taken per day?"
# For that we group our data by day and then we get a histogram of the total 
# number of steps done each day, and calculate their means and medians. In this 
# step we remove NA values

monitdat.day<-group_by(.data = monitordata,date)
steps.summ.d<-summarise(monitdat.day, steps.tot=sum(steps,na.rm = TRUE),
   steps.mean=mean(steps,na.rm = TRUE),steps.median= median(steps,na.rm = TRUE))

#To make our histogram have a more easy to understand format, we choose the
#breaks for separate the different columns to correspond approximately with 
#the equivalent of a km (around 1300 steps according to google search).

hist(steps.summ.d$steps.tot,breaks = seq(0,21194,21194/17), 
  main = "Histogram of the total number of steps taken each day",
  ylab = "Number of days",xlab = "Total steps (each division approx 1km)")


# The second question to address is:
# What is the average daily activity pattern?
# For this we group our data by their interval values and we make an activity 
# time line, averaged across all days.

monitdat.inter<-group_by(.data = monitordata,interval)
steps.summ.inter<-summarise(monitdat.inter, steps.tot=sum(steps,na.rm = TRUE),
   steps.mean=mean(steps,na.rm = TRUE),steps.median= median(steps,na.rm = TRUE))

#Again to give a much easily understandable approach to our data we plot our data
#against the time in hours and not in minutes.

plot(x = steps.summ.inter$interval/60, y=steps.summ.inter$steps.mean,type = "l",
    xlab = "Time (h)", ylab = "Average Number of Steps",
    main = "Daily Evolution of the Average Number of Steps",xaxp=c(0,24,12))

#We calculate the maximal interval according to the averaged number of steps value
max.inter.val<-steps.summ.inter$interval[steps.summ.inter$steps.mean==max(steps.summ.inter$steps.mean)]


#We have seem so far that many missing values (NA) were present in our data,
#when we check if their presence follows some pattern we can see that they 
#correspond to eight specific days and so we can not use the daily average as a
#way to substitute them instead we will use the interval average.
#
#For this we first locate the values of our NA

total.na<-sum(is.na(monitordata$steps))
na.pos<-which(is.na(monitordata$steps))
unique(as.Date(monitordata$date)[na.pos])
na.pos.s<-which(steps.summ.d$steps.tot==0)
steps.summ.d$date[na.pos.s]
monitdat.rev<-monitordata

#Once we got the positions we substitute the NA by their corresponding average 
#interval values

for( i in na.pos){
        monitdat.rev$steps[i]<-steps.summ.inter$steps.mean[steps.summ.inter$interval == monitordata$interval[i]]
}

#With the new values we follow an identical process to address the previous 
#first question, and we compare the two histograms to see their differences.

monitdat.day.rev<-group_by(.data = monitdat.rev,date)
steps.summ.d.rev<-summarise(monitdat.day.rev, steps.tot=sum(steps,na.rm = TRUE),
   steps.mean=mean(steps,na.rm = TRUE),steps.median= median(steps,na.rm = TRUE))

hist(steps.summ.d$steps.tot,breaks = seq(0,21194,21194/17), 
  main = "Histogram of the total number of steps taken each day",
  ylab = "Number of days",xlab = "Total steps (each division approx 1km)",
  ylim = c(0,20), col=rgb(1,0,0,0.5))

hist(steps.summ.d.rev$steps.tot,breaks = seq(0,21194,21194/17), 
     col=rgb(0,1,0,0.5),add=TRUE)

legend("topright", legend = c("Without NA Filling","With NA Filling"),
       lty= c(1,1),lwd = 6, col = c(rgb(1,0,0,0.5),rgb(0,1,0,0.5)))

# For review the second question again we decide to group the new values 
# according to another parameter: if the day belongs to the weekday or the weekend.
#For this we add a new factor column to our data and then group our data by the 
#kind of day they belong to and their intervals. After that we calculate their 
#new means and medians, averaged now only among their kind of day.

Sys.setlocale("LC_TIME",locale = "C")
monitdat.rev$day.kind <- factor(ifelse(weekdays(as.Date(monitdat.rev$date))==
  "Saturday"|weekdays(as.Date(monitordata$date))=="Sunday","weekend","weekday"))

monitdat.inter<-group_by(.data = monitordata,interval)
monitdat.rev.kind.inter<-group_by(.data = monitdat.rev,day.kind,interval)

steps.summ.inter.rev<-summarise(monitdat.rev.kind.inter, 
steps.tot=sum(steps,na.rm = TRUE),steps.mean=mean(steps,na.rm = TRUE),
steps.median= median(steps,na.rm = TRUE))

#We plot the daily evolution for the two kind of days.

ggplot(data = steps.summ.inter.rev,aes(x=interval/60,y=steps.mean, colour=day.kind))+
geom_line()+facet_grid(day.kind~.)+
labs(title="Daily Evolution of the Average Number of Steps",x="Time(h)",y="Average Number of Steps")+
theme(plot.title = element_text(hjust = 0.5))+
scale_x_continuous(breaks = seq.int(0,24,2),limits = c(0,24))

#We might be suspicious if their differences only come due to our method for 
#substitution, but if we check the days with NA presence we can see they are 
#present in both types of days.

na.days <- list (unique(as.Date(monitdat.rev$date)[na.pos]),unique(weekdays(as.Date(monitdat.rev$date))[na.pos]))