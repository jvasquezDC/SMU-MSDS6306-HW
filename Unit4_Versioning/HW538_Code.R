#install.packages("urltools")


library(fivethirtyeight)
library(plyr)
library(dplyr)
library(vcdExtra)
library(data.table)
library(urltools)
library(ggplot2)
###############################################
#######        1a                     #########
#######   Install 538 package         #########
#######                               #########
###############################################

#install.packages("fivethirtyeight")

###############################################
#######        1b                     #########
#######   Identify 18th data set      #########
#######   Set the data to df          #########
###############################################

#Find the 18th data set in 538
DataSet18 <- tail(as.data.frame(head(vcdExtra::datasets("fivethirtyeight"), 22)),1)
DataSet18

#Get just the Item name by itself
Testing <- as.character(tail(as.data.frame(head(vcdExtra::datasets("fivethirtyeight"), 22)),1)[1,1])
Testing

#set the 18th data set to a data frame called df
df <- as.data.frame(college_recent_grads)

###############################################
#######        1c                     #########
#######   Get URL from the item       #########
#######                               #########
###############################################

#Shows the help menu for the packages
vignette("fivethirtyeight", package = "fivethirtyeight")
data(package = "fivethirtyeight")

#Show URL for Data Set 18
DataSet18_address <- url_parse("https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/")
url_compose(DataSet18_address)

###############################################
#######        1d                     #########
#######    Get dimensions             #########
#######   of data frame               #########
###############################################

#### Function that list column names and na values
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      na_count=sum(is.na(x)),
      Obs=length(x), 
      perc_missing=sum(is.na(x))/length(x)*100
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$col_name <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$na_count, decreasing=TRUE), ])
}

#Basic info on df
dim(df)
propmiss(df)

###############################################
#######        2a                     #########
####### Column names of data frame    #########
#######   Count of columns            #########
###############################################

#List column names vertically
df_ColNames <- matrix(colnames(df))
colnames(df_ColNames) <- c("col_name")
df_ColNames

#Count number of columns in data frame
ncol(df)

###############################################
#######        2b                     #########
#######   count of each unique        #########
#######     major_category            #########
###############################################

#Get the most frequent variable from 'major_category'
Major_Count <- as.data.frame(df %>% group_by(major_category) %>% summarize(count=n()))
Major_Count <- Major_Count[complete.cases(Major_Count),]
Major_Count <- Major_Count[order(Major_Count$count,decreasing=TRUE),c(1,2)]
Major_Count

###############################################
#######        2c                     #########
#######   Plot Major_Count as         #########
#######     Bar Graph                 #########
###############################################
Major_CountBarPlot <- ggplot(data=Major_Count, 
                             aes(x=reorder(major_category, count),
                             y=count,
                             fill=major_category)) + 
                             geom_bar(stat="identity") +
                             coord_flip() +
                             labs(x="Majors", 
                                  y="Number of occurence", 
                                  title = "Occurence counts of Majors")

Major_CountBarPlot

###############################################
#######        2d                     #########
#######   Write df as .CSV file       #########
#######                               #########
###############################################

write.csv(df, "college_recent_grads.csv", row.names = FALSE)
