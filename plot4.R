plot4 <- function(first.date = "1/2/2007", last.date = "3/2/2007"){

## The arguments for this code is the interval of days we want to analyze.
## first.date has to sent as character string in form "day/month/year".
## last.date has to be sent in form "day/month/year".
## Default values set to the same of requested in Coursera task.


## Following code reads the "household_power_consumption.txt" file.
## Important note: only requested days data are read.

## Enabling use of fread function
library(data.table)

## Convertion of first.date and last.date in Date class
first<-strptime(first.date,"%d/%m/%Y")
last<-strptime(last.date,"%d/%m/%Y")

## Evaluation of size, in days, of interval of interest
diff<-as.POSIXlt(last)-as.POSIXlt(first)
diff.numeric<-as.numeric(diff)

## Evaluation of size, in minutes, of interval of interest
size<-diff.numeric*1440+1


## Creation of variable hpc in interval desired
## Variable begins in first.date at time 00:00:00 and finishes in last.date at time 00:00:00
hpc<-fread("household_power_consumption.txt",skip=first.date,nrows=size,na.strings="?")

## Creation of variables dates and variable times
dates<-hpc$V1
times<-hpc$V2

## Merging of two variables created above in only one variable
datesandtimes<-paste(dates, times)

## Converting the last variable to Date class
adjusted.time<-strptime(datesandtimes, "%d/%m/%Y %H:%M:%S")


## The following code creates the plot3 (Sub metering x time)

## Creates the plot3.png file
png(file="plot4.png")

## Adjust the plotting area via par() function
par(mfrow=c(2,2))

## Adds the plot#1 in topleft position
plot(adjusted.time,hpc$V3, type="n",ylab="Global Active Power", xlab="")
lines(adjusted.time,hpc$V3)

## Adds the plot #2 in topright position
plot(adjusted.time,hpc$V5, type="n",ylab="Voltage", xlab="datetime")
lines(adjusted.time,hpc$V5)

## Adds the plot#3 in bottomleft position
plot(adjusted.time,hpc$V7,type="n",ylab="Energy sub metering",xlab="")
lines(adjusted.time,hpc$V7,col="black")
lines(adjusted.time,hpc$V8,col="red")
lines(adjusted.time,hpc$V9,col="blue")
legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

## Adds the plot #4 in bottomright position
plot(adjusted.time,hpc$V4, type="n",ylab="Global_reactive_power", xlab="datetime")
lines(adjusted.time,hpc$V4)

## Stops adding to the file
dev.off()

}
