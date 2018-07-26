#install.packages('fpp2')
library(fpp2)

#########################################
####
####    1B Create basic plot of data
####          Add red vertical 
####             line at 1997
####
#########################################


#Plot stock indices in 4 seperate panels
plot.ts(EuStockMarkets,col = 'blue')

#Add red vertical line at year 1997
abline(v = 1997,col='red')

#########################################
####
####    1C Decompose data set
####       Add red vertical 
####        line at 1997
####
#########################################

#Set up time series data
EuStockMarkets_DC <- EuStockMarkets[, 1]

#Decompose data set, muse as multiplicative problem
decomposedRes <- decompose(EuStockMarkets_DC, type="mult") 

#Plot decomposed data set
plot(decomposedRes, col='blue')
abline(v = 1997,col='red')


#########################################
####
####         2B limit data set 
####          to 1990 and after 
####        
####
#########################################

#Subset the data to 1990 and after
maxtemp1990 <- window(maxtemp, start = 1990)

#Plot the data to make sure only 1990 and after
#https://stackoverflow.com/questions/38949694/how-to-subset-a-time-series-in-r
plot.ts(maxtemp1990,col = 'blue')

#########################################
####
####         2C Forecast next 5 years in 
####          Melbourne 
####        
####
#########################################

plot(ses(maxtemp1990, h=5,  initial="simple"))

AIC(ses(maxtemp1990, h=5,  initial="simple"))

?ses
