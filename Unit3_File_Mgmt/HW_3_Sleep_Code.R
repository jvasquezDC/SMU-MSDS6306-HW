
#Set sleep data working directory
SleepDIR <- setwd("C:/Users/James/Documents/SMU/MSDS_6306_Doing_Data_Science/HW_Data/Unit3_File_Mgmt/")

#Read CSV file into R
SleepData <- read.csv("sleep_data_01.csv", header=TRUE, sep=",")

#get column Names
colnames(SleepData)

#create vectors
AgeVector <- as.vector(SleepData$Age)
DurationVector <- as.vector(SleepData$Duration)
RSESVector <- as.vector(SleepData$RSES)

#Create metric objects
MedianAge <- mean(AgeVector, na.rm=TRUE)
DurationMin <- min(DurationVector, na.rm=TRUE)
DurationMax <- max(DurationVector, na.rm=TRUE)
SelfEsteem <- mean(RSESVector, na.rm=TRUE)
SE_SD <- sd(RSESVector, na.rm=TRUE)

#Create data frame and divide results by 5, limit to 2 sig figs
DF_AgeDurRSES <- round(data.frame(cbind(MedianAge, DurationMin, DurationMax, SelfEsteem, SE_SD))/5, digits = 2)

##############################
#######FUNCTION BUILDING######
##############################
HW3FileMgmt <- function(x) {

  #create vectors
  AgeVector <- as.vector(SleepData$Age)
  DurationVector <- as.vector(SleepData$Duration)
  RSESVector <- as.vector(SleepData$RSES)
  
  #Create metric objects
  MedianAge <- mean(AgeVector, na.rm=TRUE)
  DurationMin <- min(DurationVector, na.rm=TRUE)
  DurationMax <- max(DurationVector, na.rm=TRUE)
  SelfEsteem <- mean(RSESVector, na.rm=TRUE)
  SE_SD <- sd(RSESVector, na.rm=TRUE)
  
  #Create data frame and divide results by 5, limit to 2 sig figs
  Report <- round(data.frame(cbind(MedianAge, DurationMin, DurationMax, SelfEsteem, SE_SD))/5, digits = 2)
  
  return(Report)
}

#function(data frame name)
HW3FileMgmt(SleepData)