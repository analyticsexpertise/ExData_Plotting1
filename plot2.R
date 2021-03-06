## Mark Stephens
## Exploratory Data Analysis
## 03/08/2015
## Project 1
## Plot2.R
## Plotting electric power consumption from UC Irvine Machine Learning Repository data
## using the "Individual household electric power consumption Data Set"


## This script downloads data set from the following site
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

## Creates a line plot showing Global Active Power by hour
## The plot contains black lines
## The plot contains a blank title line
## The plot contains y-axis label "Global Active Power (kilowatts)"
## The plot contains day of the week as x-axis label

## The plot is saved to a png file plot2.png in the working directory

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

## Read data and create plot

plot2 <- function(){
  
  ## download file
  downloaddata()
  
  ## Read data 
  datafile = "./Fhousehold_power_consumption/household_power_consumption.txt"
  
  if(exists("datatable", mode="data.frame")==FALSE){
    datatable <<- read.table(file=datafile,header=TRUE,sep=";",na.strings="?",stringsAsFactors = FALSE) 
  }
  
  ## Extract days of interest
  plot2data <- datatable[datatable$Date == "1/2/2007" | datatable$Date == "2/2/2007", ]
  
  ## Extract data for y axis
  yaxis <- plot2data$Global_active_power 
  
  ## Extract data for x axis
  
  xaxis_date <- plot2data$Date
  
  xaxis_time <- plot2data$Time
  
  ## combine dates and times for date time format
  xaxis <- paste(xaxis_date,xaxis_time)
  
  ## convert to date time format
  xaxis <- parse_date_time(xaxis,"dmy hms")
  
  
  ## Create plot and save to png file in working directory
  
  ## open png graphics device
  png(file="./plot2.png",width=480,height=480)
  
  ## create plot 
  plot(x=xaxis,y=yaxis,type="l",xlab="",ylab="Global Active Power (kilowatts)")
  
  ## close png graphics device
  dev.off()
   
 
}
