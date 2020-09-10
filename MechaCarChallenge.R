# Load Libraries
library(ggplot2)
library(tidyverse)

#Load Data from CSV files
MPGdata <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
CoilData <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)


# Part One of Challenge - MPG Regression
# Create Matrix to examine Correlation Matrix to determine correlation coefficients of all parameters to MPG
MPGMatrix <- as.matrix(MPGdata[,c("vehicle length","vehicle weight","spoiler angle","ground clearance","AWD","mpg")]) #convert MPG data frame into numeric matrix
cor(MPGMatrix)

# Multiple Linear Regression of 3 of the 5 parameters in the data
lm(mpg ~ `vehicle length` + `ground clearance` + `vehicle weight`, data = MPGdata)
summary(lm(mpg ~ `vehicle length` + `ground clearance` + `vehicle weight`, data = MPGdata))

# Part Two of Challenge - Suspension Coil Summary Table

CoilSummary <- as.data.frame(list(Mean = mean(CoilData$PSI),
                    Median = median(CoilData$PSI),
                    Variance = var(CoilData$PSI),
                    StDeviation = sd(CoilData$PSI)))

# Part Three of Challenge - Suspension Coil T-Test

t.test(CoilData$PSI, mu=1500)


