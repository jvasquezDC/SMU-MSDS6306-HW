library(rvest)
library(tidyr)
library(stringr)
library(ggplot2)

####################################
####
####  1 Harry Potter cast 
####     Web Scrapping Harry Potter
####
####################################

#Specifying the url for desired website to be scrapped
DeathlyHallowsPartII <- read_html("https://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1")

#Extract  full cast list
DeathlyHallowsPartII <- DeathlyHallowsPartII %>%
  html_nodes(".character , .character, td.itemprop") %>%
  html_text()

####################################
####
#### Clean up the data table
####
####################################

#Tidy Data, get name and charater on one line
DeathlyHallowsPartII<-gsub("\n ","",DeathlyHallowsPartII)
DeathlyHallowsPartII<-gsub("         ","",DeathlyHallowsPartII)

#Create data frame
DeathlyHallowsPartII <- as.data.frame(DeathlyHallowsPartII)

#Seperating out charaters and actor
Character <- DeathlyHallowsPartII[ !c(TRUE,FALSE), ]  #Get chacter name, odd rows
Actor <- DeathlyHallowsPartII[ !c(FALSE,TRUE), ]      #Get actor name, even rows

#Removing spaces between names
Character <- gsub("\\s+"," ",Character)
Actor <- gsub("\\s+"," ",Actor)

#Removing leading and trailing white spacs
Character <- trimws(Character,"both")
Actor <- trimws(Actor,"both")

#Merge the factors as a data frame
CastList <- cbind.data.frame(as.character(Actor),as.character(Character))

#Rename Column Names
colnames(CastList)[colnames(CastList)=='as.character(Actor)'] <- 'Actor'
colnames(CastList)[colnames(CastList)=='as.character(Character)'] <- 'Character'

####################################
####
#### Split Actor Name into first & sur names
####
####################################

#Split Actor name into first and sur name, middle name is included in first name
CastList <- extract(CastList, Actor, c("FirstName", "SurName"), "(.*) ([^ ]+)$")

#Show first 10 rows
head(CastList, 10)

#################################################################
######
######                     2 SportsBall NBA Statas
######                        Web Scrapping
######
#################################################################


###################       B            ##############################
#Specifying the url for desired website to be scrapped
SpursShooting <- read_html("http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs")

#Extract  numbers from webpage
SpursShooting <- SpursShooting %>%
  html_nodes("td") %>%
  html_text()

#create a data frame of spurs stats
SpursShooting <-  as.data.frame(SpursShooting)

#Print only shooting stats table
SpursShooting <- SpursShooting[243:452,]

#Convert to data frame
SpursShooting <- as.data.frame(SpursShooting)

###################       C            ##############################
#https://stackoverflow.com/questions/33235400/split-a-single-column-into-multiple-columns-based-on-rows
#Split single column into mulltiple columns
SpursShooting <- as.data.frame(matrix(SpursShooting[,1], byrow=TRUE, ncol = 15))

#Rename columns
colnames(SpursShooting)[colnames(SpursShooting)=='V1'] <- 'PLAYER'
colnames(SpursShooting)[colnames(SpursShooting)=='V2'] <- 'FGM'
colnames(SpursShooting)[colnames(SpursShooting)=='V3'] <- 'FGA'
colnames(SpursShooting)[colnames(SpursShooting)=='V4'] <- 'FG_percent'
colnames(SpursShooting)[colnames(SpursShooting)=='V5'] <- 'x3PM'
colnames(SpursShooting)[colnames(SpursShooting)=='V6'] <- 'x3PA'
colnames(SpursShooting)[colnames(SpursShooting)=='V7'] <- 'x3P_percent'
colnames(SpursShooting)[colnames(SpursShooting)=='V8'] <- 'FTM'
colnames(SpursShooting)[colnames(SpursShooting)=='V9'] <- 'FTA'
colnames(SpursShooting)[colnames(SpursShooting)=='V10'] <- 'FT_percent'
colnames(SpursShooting)[colnames(SpursShooting)=='V11'] <- 'x2PM'
colnames(SpursShooting)[colnames(SpursShooting)=='V12'] <- 'x2PA'
colnames(SpursShooting)[colnames(SpursShooting)=='V13'] <- 'x2P_percent'
colnames(SpursShooting)[colnames(SpursShooting)=='V14'] <- 'PPS'
colnames(SpursShooting)[colnames(SpursShooting)=='V15'] <- 'AFG_percent'

#create data frames on players and shooting stats
SpursShooting_Player <- as.data.frame(SpursShooting[,c(1)])
colnames(SpursShooting_Player)[colnames(SpursShooting_Player)=='SpursShooting[, c(1)]'] <- 'PLAYER'

#Split out player name and position
SpursShooting_Player <- as.data.frame(do.call(rbind, str_split(SpursShooting_Player$PLAYER, ',')))
colnames(SpursShooting_Player)[colnames(SpursShooting_Player)=='V1'] <- 'Player'
colnames(SpursShooting_Player)[colnames(SpursShooting_Player)=='V2'] <- 'Position_Abv'

#strip white spaces
SpursShooting_Player <- data.frame(lapply(SpursShooting_Player, trimws), stringsAsFactors = FALSE)

#Spurs stats table
SpursShooting_Stats <- SpursShooting[,c(2:15)]

#Convert all columns to numeric
indx <- sapply(SpursShooting_Stats, is.factor)
SpursShooting_Stats[indx] <- lapply(SpursShooting_Stats[indx], function(x) as.numeric(as.character(x)))

#combine player & stats objects
SpursShooting_Analysis <- data.frame(SpursShooting_Player, SpursShooting_Stats)

#Adding new column for the full position name
SpursShooting_Analysis$Position_Full <- sapply(SpursShooting_Analysis$Position_Abv, switch, 
                  C = 'Center', 
                  PG = 'Point Guard', 
                  SG = 'Shooting Guard', 
                  SF = 'Small Forward',
                  PF = 'Power Forward')

#Check stats are numeric
str(SpursShooting_Analysis)
head(SpursShooting_Analysis,3)

###################       D           ##############################

#Make bar plot of FGP by Player
FG_Player <- ggplot(data=SpursShooting_Analysis, 
                              aes(x=reorder(Player, FG_percent), 
                                  y=FG_percent,
                                  fill=Position_Full)) +
                              geom_bar(stat="identity")+
                              coord_flip()+
                              labs(x="Player",
                                   y="Field Goal Percentage",
                                   title = "Field Goal Percentage by Player")

FG_Player

