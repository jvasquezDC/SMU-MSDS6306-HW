library(data.table)
library(sqldf)
library(RSQLite)
library(DBI)
library(gsubfn)
library(proto)

####################################
######        1a
######   Read yob2016.txt
######   Add column names
####################################

#Read in the yob2016.txt data set
df <- read.table("yob2016.txt", header = FALSE, sep = ";", strip.white = TRUE)
colnames(df)[colnames(df)=='V1'] <- 'Name'
colnames(df)[colnames(df)=='V2'] <- 'Gender'
colnames(df)[colnames(df)=='V3'] <- 'Count_of_Name'


####################################
######        1b
######   Display summary 
######   & Struture df
####################################

#Get dimensions of df
dim(df)
#show class
class(df)
#show summary
summary(df)
#Show Structure of df
str(df)


####################################
######        1c
######   Find misspelled Name
######       %yyy
####################################

#find misspelled names
sqldf("Select * 
      FROM df 
      WHERE Name like '%yyy';")

####################################
######        1d
######   Remove Fionayyy
######       from df
####################################

y2016 <- df[!(df$Name=="Fionayyy"),]
dim(df)
dim(y2016)

####################################
######        2a
######   Read yob2015.txt
######   Add column names
####################################

y2015 <- read.table("yob2015.txt", header = FALSE, sep = ",", strip.white = TRUE)

#rename column names
colnames(y2015)[colnames(y2015)=='V1'] <- 'Name'
colnames(y2015)[colnames(y2015)=='V2'] <- 'Gender'
colnames(y2015)[colnames(y2015)=='V3'] <- 'Count_of_Name'

#Show table
head(y2015, 2)

####################################
######        2b
######   Show last 10 rows
######   Describe observations
####################################

tail(y2015,10)

#They are all males and have 5 obs each

####################################
######        2c
######   Merge y2015 & y2016
######   
####################################

#merge data frames on name and gender
final <- merge(y2015, y2016, by.x=c("Name","Gender"), by.y=c("Name","Gender"), all=FALSE)

#rename column names
colnames(final)[colnames(final)=='Count_of_Name.x'] <- 'Count_of_Name_2015'
colnames(final)[colnames(final)=='Count_of_Name.y'] <- 'Count_of_Name_2016'

#Show table
head(final, 2)

####################################
######        3a
######   Add total column
######   Add y2015 and y2016
######     Report sum of counts
####################################

#Add the counts and creates Total column
#sorts by Total in descending order
final <- sqldf("Select 
          Name,
          Gender,
          Count_of_Name_2015,
          Count_of_Name_2016,
          Count_of_Name_2015+Count_of_Name_2016 as Total
      FROM final
      ORDER by Total desc;")

#Report sum total names
sqldf("Select SUM(Count_of_Name_2015+Count_of_Name_2016) as TotalCountofNames FROM final;")

####################################
######        3b
######   Sort final
######   Show top 10 names
######     
####################################

#show top 10 results
head(final, 10)

####################################
######        3c
######   Sort final
######   Show top 10 female names
######     
####################################

final_Top10_female_names <- sqldf("Select 
                        Name,
                        Count_of_Name_2015+Count_of_Name_2016 as Total
                        FROM final
                        WHERE Gender = 'F'
                        ORDER by Total desc limit 10;")
final_Top10_female_names

####################################
######        3d
######   Write top 10 femal names
######        to CSV
######     
####################################

write.csv(final_Top10_female_names, file="Top_10_Popular_Female_Names.csv", row.names = FALSE)

