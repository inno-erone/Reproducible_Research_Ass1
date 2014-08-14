## Reproducible research assignment 1
remove(list=ls())


###### ---  part 1
## Loading and preprocessing the data

check.files <- function(){  
        files <- c("activity.csv", "activity.zip")
        
        if ( !any(file.exists(files)) ) {       
                cat("Required files are missing in your working directory.\n")        
                resp <- readline("Download the nessesary file [y/n]? ")
                required.file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
                
                if(tolower(resp)=="y"){
                        download.file(url = required.file, destfile = "activity.zip")
                        unzip("activity.zip", overwrite=T, exdir=".")           
                }
                else
                        stop("Ensure this file resides in our working directory.")   
        }     
}       

check.files()
library(foreign)
par(cex.main = 1, cex.sub=.7, cex.axis=.8, las=2)

activity <- read.csv(file="activity.csv", header=T, as.is=T, na.strings = "NA")
activity$date <- as.Date(activity$date)





###### ---  part 2: What is mean total number of steps taken per day?

data1 <- tapply(activity$steps, activity$date, sum)

hist(data1, 
     main="Total steps taken per day", 
     ylab="frequency", 
     xlab="number of steps", 
     breaks=length(data1), 
     xlim=range(0, 25000), 
     ylim=range(0,10)
)

## What is mean total number of steps taken per day?
sum(data1, na.rm=T)


## What is the average daily activity pattern?
mean(data1, na.rm=T)
median(data1, na.rm=T)



###### ---  part 3: What is the average daily activity pattern?
## part A
df <- aggregate.data.frame(activity$steps, 
                           by=list(activity$interval), FUN=mean, na.rm=T)

names(df) <- c("interval", "average")
plot(df$interval, df$average, 
     type="l", 
     main = "Time limit interval vs. Average number of steps taken",
     ylab = "Average number of steps", 
     xlab = "5-second time interval",
     sub = "(averaged across all days)"
     )

## part B
df[which.max(df$average), "interval"]






###### ---  part 4: Imputing missing values
## part A
sum(is.na(activity$steps))


## part B and C
## I shall replace NA values with the mean obtained from interval.means

activity2 <- activity
agg.means <- aggregate(activity2$steps, 
                       by=list(activity2$interval), FUN=mean, na.rm=T)

names(agg.means) <- c("interval", "interval.means")
agg.means$interval.means <- signif(agg.means$interval.means, digits=2)

i <-1
for(i in 1:nrow(activity2)){     
        if(is.na(activity2$steps[i]))  
                activity2$steps[i] <- agg.means[i,2]
}



## part D
data2 <- with(activity, tapply(steps, date, sum))
hist(data2, 
     main="Total steps taken per day NA values filled", 
     ylab="frequency", 
     xlab="number of steps", 
     breaks=length(data2), 
     xlim=range(0, 25000), ylim=range(0,10)
     )


###### ---  part 5: Are there differences in activity patterns between weekdays and weekends?
mean(data2, na.rm=T)
median(data2, na.rm=T)
## The mean does not change with or without missing data. However, there 
## is a very slight change on the median od the data.



activity2$dayGroup <- weekdays(activity2$date)

index1 <- grep("^(mon|tue|wed|thur|fri)", activity2$dayGroup, ignore.case = T)
index2 <- grep("^(sat|sund)", activity2$dayGroup, ignore.case = T)
activity2[index1, 4] <- "Weekday"
activity2[index2, 4] <- "Weekend"
activity2$dayGroup <- factor(activity2$dayGroup)



## part B: Pannelplot
# agg.means2 <- data.frame(
#         tapply(activity2$steps, 
#             INDEX=list(activity2$interval, activity2$dayGroup),
#             FUN=mean, na.rm=T)
#         )
# plot(ts(agg.means2), yax.flip=T, las=1)


agg.means2 <- aggregate(activity2$steps, 
               by=list(activity2$interval, activity2$dayGroup),
               FUN=mean, na.rm=T)
names(agg.means2) <- c("interval", "dayGroup", "means")

library(lattice)
xyplot(agg.means2$means ~ agg.means2$interval | agg.means2$dayGroup, 
       layout=c(1,2),
       type="l",
       xlab = "interval",
       ylab = "Average number of steps",
       main = "5-minute interval vs. average number of steps")



