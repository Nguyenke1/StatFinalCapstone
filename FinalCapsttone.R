# Libraries ---------------------------------------------------------------
library(dplyr)#for aggregating, data manipulation, etc.
library(ggplot2)#professional graphics
library(lubridate)#for working with dates
library(readxl) #we were given a master excel sheet
library(randomForest)
library(pROC) #for ROC curves
library(flipTime) #install by running install.packages("remotes") and remotes::install_github("Displayr/flipTime") in terminal
library(logistf)

# Run a chunk on Mac: cmd + option + t
# Run a chunk on Windows: ctrl + alt + t
# This way you don't have to keep highlighting and running code when you open
# R studio each time

#format the Pickup + arrive as time, then find difference

# Set Up ------------------------------------------------------------------
#start with a clean slate!
rm(list = ls())

# Set working directory
setwd("D:/school/stat/ProjectData")

# Read in Data ------------------------------------------------------------
Area <- read.csv("By Date - Dec 2019, Jan 2020, Feb 2020.csv", stringsAsFactors=TRUE) #all cols
Drivers <- read.csv("By Driver - Dec 2019, Jan 2020, Feb 2020.csv", stringsAsFactors=TRUE) #Drop from model name col perhaps
Rides <- read.csv("By Individual Ride - Dec 2019, Jan 2020, Feb 2020.csv",stringsAsFactors=TRUE) #issue row 2548, 2549 Remove those
                                                                    #ignore InvoiceNum, Trip ID and Created
                                                                    #Can ignore 1st Pickup + Dropoff address for redundancy

# Data Cleaning -----------------------------------------------------------
# Rename column names to get rid of weird characters
colnames(Area) <- gsub("[\r\n - .]","",colnames(Area))
colnames(Drivers) <- gsub("[\r\n - .]","",colnames(Drivers))
colnames(Rides) <- gsub("[\r\n - .]","",colnames(Rides))

Rides = Rides[-c(2548,2549,1449,2121,4276),] #remove the improperly formatted rows


#make the unperformed ride column
Rides$PerformedDrive <- ifelse(Rides$DropoffPerform == "", 0, 1)
Rides$PerformedDrive <- factor(Rides$PerformedDrive)

#make zipcodes factors instead of numeric varibles
Rides$PickupZip <- factor(Rides$PickupZip)
Rides$DropoffZip <- factor(Rides$DropoffZip)

Rides$PickupAddress1 <- as.character(Rides$PickupAddress1)
Rides$DropoffAddress1 <- as.character(Rides$DropoffAddress1)

Rides$Distance <- as.numeric(Rides$Distance)
Rides$Charge <- as.numeric(Rides$Distance)

#Rides$ReqPickup <- time(Rides$ReqPickup)

#Space, Charge, Type, FundingSource, Distance, Run,
#Difference between Req. Pickup Time and Pickup Perform Time, Pickupcity, Pickupstate, Pickupzip, Dropoffcity, dropoffzip, dropoffstate, Routeddistance, importdistance. 
# Target Variable = PerformedDrive

#Rides$Req.Pickup <- AsDateTime(char(Rides$Req.Pickup))

sapply(Rides, function(x) sum(is.na(x)))

str(Rides)

###-------BASELINE FOREST----------------------
#set the seed
RNGkind(sample.kind = "default")
set.seed(1728)
#train.idx will contain a random sample of row indices
train.idx <- sample(x = 1:nrow(Rides), size = floor(.8*nrow(Rides)))
#make training data
train.df <- Rides[train.idx,]
#the rest will be for testing
test.df <- Rides[-train.idx,] 
myforest = randomForest(PerformedDrive ~ Space + Charge + Type
                         + Distance + Run
                        + PickupState
                        + DropoffState + RoutedDistance + ImportDistance,#recall notes on this syntax
                        data = train.df, #TRAINING DATA
                        ntree = 1000, #fit B = 1000 separate classification trees
                        mtry = 4, #choose m - sqrt(16) = 4
                        importance = TRUE) #importance can help us identify important predictors (later)

# Descriptive Models -----------------------------------------------------------
m1 = glm(Rides$PerformedDrive ~  Rides$Space + Rides$Charge + Rides$Type + Rides$FundingSource + Rides$Distance
         + Rides$Run + Rides$PickupCity + Rides$PickupState
         + Rides$PickupZip + Rides$DropoffCity
         + Rides$DropoffZip + Rides$DropoffState + Rides$RoutedDistance
         + Rides$ImportDistance, data = Rides, family = "binomial"))
summary(m1) #AIC = 15305

