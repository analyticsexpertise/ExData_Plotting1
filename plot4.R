## Mark Stephens
## Exploratory Data Analysis
## 03/08/2015
## Project 1
## Plot4.R
## Plotting electric power consumption from UC Irvine Machine Learning Repository data
## using the "Individual household electric power consumption Data Set"


## This script downloads data set from the following site
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

## The output assembles 4 plots into 1 view (Plot 2, Plot 3, and 2 additional plots as documented below)

## The plot is saved to a png file plot4.png in the working directory

require(lubridate)

## Download data file into working directory 
downloaddata <- function(){
  
  ## file url
  fileURL = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  
  ## file destination is working directory with filename Fhousehold_power_consumption.zip
  dest = "./Fhousehold_power_consumption.zip"
  
  if(file.exists(dest)==FALSE){
    ## Download file then extract data set from archive
    download.file(url = fileURL,destfile = dest)
    
    ## unzip file
    unzip(zipfile = dest, exdir = "./Fhousehold_power_consumption")
    
    message("file downloaded and unzipped in working directory")
  }
  else{
    message("file exists in working directory")  
  }
  
  
}

## Assemble plots into single view
plot4 <- function(){
  
  ## download file
  downloaddata()
  
  ## Read data 
  datafile = "./Fhousehold_power_consumption/household_power_consumption.txt"
  
  if(exists("datatable", mode="data.frame")==FALSE){
    datatable <<- read.table(file=datafile,header=TRUE,sep=";",na.strings="?",stringsAsFactors = FALSE) 
  }
  
  ## Extract days of interest
  plotdata <<- datatable[datatable$Date == "1/2/2007" | datatable$Date == "2/2/2007", ]
  
  ## clear plot window
  plot.new()
  
  ## open png graphics device
  png(file="./plot4.png",width=480,height=480)
  
  ## Minimize margins
  par(mar = c(4,4,2,2))
  
  ## Create Global Active Power plot
  par(mfrow = c(2,2))
  plot4a("Global Active Power")
  
  ## Create Voltage plot
  plot4a("Voltage")
  
  ## Create Energy sum metering plot
  plot3a()
  
  ## Create Global reactive power plot
  plot4a("Global_reactive_power")
  
  ## close png graphics device
  dev.off()
  
}


## Create plots

## Create Voltage or Global_reactive_power plots based on plotname parameter set to either 
## "Voltage" (default), "Global_reactive_power", "Global Active Power"
plot4a <- function(plotname="Voltage"){
  
  ## Extract data for y axis
  
  yaxis <- plotdata$Voltage  
  xlabel = "datetime"
  
  if(plotname=="Global_reactive_power"){
    yaxis <- plotdata$Global_reactive_power
    xlabel = "datetime"
  }
  
  if(plotname=="Global Active Power"){
    yaxis <- plotdata$Global_active_power
    xlabel = ""
  }
  
    
  ## Extract data for x axis
  
  xaxis_date <- plotdata$Date
  
  xaxis_time <- plotdata$Time
  
  ## combine dates and times for date time format
  xaxis <- paste(xaxis_date,xaxis_time)
  
  ## convert to date time format
  xaxis <- parse_date_time(xaxis,"dmy hms")
  
  
  
  
  ## create plot 
  
  plot(x=xaxis,y=yaxis,type="l",col="black",xlab=xlabel,ylab=plotname, pch=20) 
  
  
  
  
}

## Create Plot 3
plot3a <- function(){
  
  ## Extract data for y axis
  yaxis1 <- plotdata$Sub_metering_1
  yaxis2 <- plotdata$Sub_metering_2
  yaxis3 <- plotdata$Sub_metering_3
  
  ## Extract data for x axis
  
  xaxis_date <- plotdata$Date
  
  xaxis_time <- plotdata$Time
  
  ## combine dates and times for date time format
  xaxis <- paste(xaxis_date,xaxis_time)
  
  ## convert to date time format
  xaxis <- parse_date_time(xaxis,"dmy hms")
  
  
  ## create plot 
  
  plot(x=xaxis,y=yaxis1,type="l",col="black",xlab="",ylab="Energy sub metering",pch=20) ## plot Sub_metering_1
  lines(x=xaxis,y=yaxis2,type="l",col="red") ## plot Sub_metering_2
  lines(x=xaxis,y=yaxis3,type="l",col="blue") ## plot Sum_metering_3
  
  ## Add legend
  legend(x="topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1),lwd=c(2.5,2.5),
         col=c("black","red","blue"),cex=1, bty = "n")
  
  
  
}
