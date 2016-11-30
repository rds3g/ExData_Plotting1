
#load the dataset, I call it twoDayDataSet

if(! exists("twoDayDataSet")) {
  twoDayDataSet = loadTwoDayDataSetFromFull()
}

#this function encapsulates the base R graphics calls to make the desired figure
makeFig1 <- function() {
  hist(twoDayDataSet$Global_active_power,breaks=seq(0,8,0.5),col="red",
       main="Global Active Power",ylab="Frequency",xlab="Global Active Power (kilowatts)")
  return(TRUE)
}
makeFig2 <- function() {
  plot(twoDayDataSet$dateTime, twoDayDataSet$Global_active_power, type="n",
       main="",ylab="Global Active Power (kilowatts)",xlab="")
  lines(twoDayDataSet$dateTime, twoDayDataSet$Global_active_power, type="l")
  return(TRUE)
}

dev.set(2)  #set to screen
result <- makeFig2()

# now plot to the .png output file
# open the graphics file
pngFileOutput <- png(filename = "plot1.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white")

result <- makeFig2()
dev.off() # close the PNG file



####################################################################
# Helper Function to Load the Data
##################################################################
loadTwoDayDataSetFromFull <- function() {
  if(file.exists("twoDay.RData")) {
    load("twoDay.RData")
    if(exists("twoDayDataSet")){
      return(twoDayDataSet)
    }
  }
  if(file.exists("FullDataSet.RData") ){
    load("FullDataSet.RData")
  }
  else {
    dataSourceFile <- "household_power_consumption.txt"
    myColClasses = rep("character",9)
    fullDataSet <- read.table(dataSourceFile, header=TRUE, sep=";",colClasses=myColClasses, na.strings=c("?"))
    save(fullDataSet,file="FullDataSet.RData")
  }
  
  fullDataSet <- fullDataSet %>% mutate(dateTimeAsChar = paste(as.character(Date),as.character(Time)))
  fullDataSet <- fullDataSet %>% mutate(dateTime = dmy_hms(dateTimeAsChar))
  #fullDataSet <- fullDataSet %>% mutate(Sub_metering_1=as.numeric(Sub_metering_1))
  windowStart = ymd_hms("2007-02-01 00:00:00")
  fullDataSet <- fullDataSet %>% mutate(minutesAfterWindowStart=difftime(dateTime,windowStart,units="mins"))
  twoDayDataSet <- fullDataSet %>% filter(minutesAfterWindowStart>=0, minutesAfterWindowStart<2*24*60)
  #minutesAfterWindowStart < 2*24*60 selects the two day window
  twoDayDataSet <- twoDayDataSet %>% select(-dateTimeAsChar, -Date, -Time) %>%
    select(dateTime,minutesAfterWindowStart,everything())
  for ( ii in seq(3,ncol(twoDayDataSet)) ) {
    tmpCol <- as.numeric(twoDayDataSet[,ii])
    twoDayDataSet[,ii]<-tmpCol
  }
  
  return(twoDayDataSet)
}
