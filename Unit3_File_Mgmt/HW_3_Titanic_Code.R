library(plyr)

#Set working directory Titanic directory
TitanicDIR <- setwd("C:/Users/James/Documents/SMU/MSDS_6306_Doing_Data_Science/gitprojects/awesome-public-datasets/Datasets/titanic/")

#Read CSV file into R
TitanicData <- read.csv("titanic.csv", header=TRUE, sep=",")
MatrixTitanicData <- matrix(TitanicData)

#check data table class
class(TitanicData)
class(MatrixTitanicData)

#Get frequency table of the Sex column
SexFreq <- count(TitanicData, 'Sex')
countsSEX <- table(TitanicData$Sex)

#show results
SexFreq
countsSEX

#Make bar chart
barplot(countsSEX, 
        main="Gender Frequency Count", 
        xlab="Gender",
        ylab = "Counts",
        col = c("darkblue"))


#Extract columns needed to find means
AgeVector <- as.vector(TitanicData$Age)
FareVector <- as.vector(TitanicData$Fare)
SurvivalVector <- as.vector(TitanicData$Survived)

#Combine vectors into matrix
MATRIXAgeFareSurvived <- cbind(AgeVector, FareVector, SurvivalVector)

#output means of Age, Fare, Survival. Output should be real number
MeanAgeFareSurvived <- apply(MATRIXAgeFareSurvived, 2, mean, na.rm=TRUE)

#shows output is numeric
class(MeanAgeFareSurvived)

#Show Means
MeanAgeFareSurvived

##########################
##########################
#####  Sleep Data Set ####
##########################
##########################

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