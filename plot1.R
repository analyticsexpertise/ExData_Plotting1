## Mark Stephens
## Exploratory Data Analysis
## 03/08/2015
## Project 1
## Plot 1.R
## Plotting electric power consumption from UC Irvine Machine Learning Repository data
## using the "Individual household electric power consumption Data Set"


## This script downloads data set from the following site
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

## Creates the Global Active Power histogram showing Frequency as a function of Global Active Power
## The histogram contains orange red bars
## The histogram contains the title "Global Active Power" centered at the top
## The histogram contains x-axis label "Global Active Power (kilowatts)"

## The plot is saved to a png file plot1.png in the working directory

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

plot1 <- function(){
  
  ## download file
  downloaddata()
  
  ## Read data 
  datafile = "./Fhousehold_power_consumption/household_power_consumption.txt"
  
  if(exists("datatable", mode="data.frame")==FALSE){
    datatable <<- read.table(file=datafile,header=TRUE,sep=";",na.strings="?",stringsAsFactors = FALSE) 
  }
  
  ## Extract days of interest
  plot1data <- datatable[datatable$Date == "1/2/2007" | datatable$Date == "2/2/2007", ]
  
  ## Get data for x axis
  xaxis <- plot1data$Global_active_power
  
  ## Create histogram and save to png file in working directory
  
  
  ## open png graphics device
  png(file="./plot1.png",width=480,height=480)
  
  ## create histogram
  hist(x = xaxis, col="orangered1",main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  
  ## close png graphics device
  dev.off()
  
  
}
