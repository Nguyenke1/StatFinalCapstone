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

###-------TUNING FOREST----------------------
mtry <- c(1:16)

#make room for B, OOB error
Rides2 <- data.frame(m = rep(NA,length(mtry)),
                     OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  tempforest<- randomForest(PerformedDrive ~ Space + Charge + Type
                            + Distance + Run
                            + PickupState
                            + DropoffState + RoutedDistance + ImportDistance,
                            data = train.df, 
                            ntree = 1000, # fixed B value
                            mtry = mtry[idx]) # mtry is varying
  #record iteration's m value
  Rides2[idx, "m"] <- mtry[idx]
  #record what our OOB error rate was
  #note code that grabs OOB error rate - this approximates out of sample error
  Rides2[idx,"OOB_err_rate"] <- mean(predict(tempforest)!= train.df$PerformedDrive)
  
}


#plot you can use to justify your chosen tuning parameters
qplot(m, OOB_err_rate, geom = c("line", "point"), data = Rides2) + 
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate") +
  scale_x_continuous(breaks=seq(0,16), by=1)
# Plot shows mtry = 4 results in lowest OOB error

#Final forest based on tuning:
final_forest<- randomForest(PerformedDrive ~ Space + Charge + Type
                            + Distance + Run
                            + PickupState
                            + DropoffState + RoutedDistance + ImportDistance,
                            data = train.df, 
                            ntree = 1000, 
                            mtry = 2,#based on tuning
                            importance = TRUE)

       
#make a column of predictions on the test set
test.df$forest_pred <- predict(final_forest, test.df, type = "class")
#confusion matrix where pi* = 0.5, as with trees
table(test.df$forest_pred, test.df$PerformedDrive)
#Create ROC Curve assuming 'positive event' is W
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Yes"]
rocCurve <- roc(response = test.df$PerformedDrive, #truth
                predictor = pi_hat, #probabilities of positive event W
                levels = c("0", "1")) #(negative event, positive event)

plot(rocCurve,print.thres = TRUE, print.auc=TRUE)

# Descriptive Models -----------------------------------------------------------
m1 = glm(Rides$PerformedDrive ~  Rides$Space + Rides$Charge + Rides$Type + Rides$FundingSource + Rides$Distance
         + Rides$Run + Rides$PickupCity + Rides$PickupState
         + Rides$PickupZip + Rides$DropoffCity
         + Rides$DropoffZip + Rides$DropoffState + Rides$RoutedDistance
         + Rides$ImportDistance, data = Rides, family = "binomial"))
summary(m1) #AIC = 15305

