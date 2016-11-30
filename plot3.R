
#load the dataset, I call it twoDayDataSet

if(! exists("twoDayDataSet")) {
  twoDayDataSet = loadTwoDayDataSetFromFull()
}

#this function encapsulates the base R graphics calls to make the desired figure


makeFig3 <- function() {
  plot(twoDayDataSet$dateTime,twoDayDataSet$Sub_metering_1,type="n",xlab="Energy Sub-Metering",
       ylab="Energy sub metering")
  lines(twoDayDataSet$dateTime,twoDayDataSet$Sub_metering_1,col="black")
  lines(twoDayDataSet$dateTime,twoDayDataSet$Sub_metering_2,col="red")
  lines(twoDayDataSet$dateTime,twoDayDataSet$Sub_metering_3,col="blue")
  #lines(twoDayDataSet$dateTime,twoDayDataSet$Sub_metering_1,col="black")
  legend("topright", 
         c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"),
         lty=1, lwd=1)
  
  return(TRUE)
}


dev.set(2)  #set to screen
result <- makeFig3()

# now plot to the .png output file
# open the graphics file
pngFileOutput <- png(filename = "plot3.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white")

result <- makeFig3()
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
