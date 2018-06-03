#1a
#Log of 5
  log(5)

#1b.1
#default base of log function is 10

#1b.2
#Log of 5 with base of 2
  log(5,2)

#1c
#Calculate log of a negative number
#natural log is only for numbers greater than 0, therefore you cannot log a negative number

#1d
#Calculate log of square root of a positive number
  sqrt(4)

#2a
#Create a vector of 15 numbers
  RandomNumbers <- floor(runif(15, min=0, max=100))

#2a.1
#Calculate mean from previous
  mean(RandomNumbers)

#2a.2
#Calculate SD from previous
  sd(RandomNumbers)

#2b Change the mean to 10 and SD to 2 recalculate vector of 15 random numbers
  RandomNumbersSetMeanSD <- rnorm(15, mean=10, sd=2)
  mean(RandomNumbersSetMeanSD)
  sd(RandomNumbersSetMeanSD)

#2c Why are the means and and SD's not exactly the same as defined in the function?
  #FILL IN LATER
  
#3 Vector Operations
#3a, 3b, 3c Input the vectors into R
  weightINDIV_kg <- c(60, 72, 57, 90, 95, 72)
  heightINDIV_m <- c(1.80, 1.85, 1.72, 1.90, 1.74, 1.91)
  weightINDIV_kg
  heightINDIV_m

#3d create scatter plot weight vs height
  plot(weightINDIV_kg, heightINDIV_m, main="Weight kg vs Height m",
       xlab="Weight kg", ylab="Height m", pch=19)
  #Interpertation
    #As the weight of the indivudal increases so does the indivual height, with the exception of the indidvual who weighs 95kg

#3e calculate the BMI for each individual weight/height^2
  BMI <- weightINDIV_kg/heightINDIV_m^2
  BMI

#3f Calcuate mean for weight
  meanWeight_kg <- mean(weightINDIV_kg)
  meanWeight_kg
  
#3g Subtract mean weight from weight
  Weight_Minuts_MeanWeight <- weightINDIV_kg-meanWeight_kg
  Weight_Minuts_MeanWeight
  
#3h Sum results from 3g
  sum(Weight_Minuts_MeanWeight)
  

#4 Enter Data Science Profile as a data frame
  Category = c("Programming", "Math", "Statistics", "Machine Learning", 
               "Domain Expertise", "Communication and Presentation Skills", 
               "Data Visualization")
  Ranking = c(2, 2, 2, 1, 4, 4, 4)
  
  JamesVasquez = data.frame(Category, Ranking)
  JamesVasquez
  
  
  
  