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

########------Area Dataframe (By Date wkst) Graphs------
#Graph 1: Trips vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = Trips, y = TotalRevenue)) +
  ggtitle("Relation between Trips and Total Revenue (from Area dataframe") +
  labs(x = "Number of Trips",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 2: Performed vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = Performed, y = TotalRevenue)) +
  ggtitle("Relation between Number of Performed Trips and Total Revenue (from Area dataframe") +
  labs(x = "Number of Performed Trips",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 3: Unperformed vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = Unperformed, y = TotalRevenue)) +
  ggtitle("Relation between Number of Unperformed Trips and Total Revenue (from Area dataframe") +
  labs(x = "Number of Unperformed Trips",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 4: AdvanceCancels vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = AdvanceCancels, y = TotalRevenue)) +
  ggtitle("Relation between Number of Advanced Cancels and Total Revenue (from Area dataframe") +
  labs(x = "Number of Advanced Cancels",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()
#Graphs 3 & 4 are very similar in terms of actual data

#Graph 5: DriverCancels vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = DriverCanceles, y = TotalRevenue)) +
  ggtitle("Relation between Number of Driver Canceles and Total Revenue (from Area dataframe") +
  labs(x = "Number of Driver Canceles",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()
#Graph 5 has a typo. Cancels vs Canceles

#Graph 6: RevenuePerMile vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = RevenuePerMile, y = TotalRevenue)) +
  ggtitle("Relation between Revenue Per Mile and Total Revenue (from Area dataframe") +
  labs(x = "Revenue Per Mile",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 7: RevenuePerCapacitatedMile vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = RevenuePerCapacitatedMile, y = TotalRevenue)) +
  ggtitle("Relation between Revenue Per Capacitated Mile and Total Revenue (from Area dataframe") +
  labs(x = "Revenue Per Capacitated Mile",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 8: TotalMiles vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = TotalMiles, y = TotalRevenue)) +
  ggtitle("Relation between Total Miles and Total Revenue (from Area dataframe") +
  labs(x = "Total Miles",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 9: CapacitatedMiles vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = CapacitatedMiles, y = TotalRevenue)) +
  ggtitle("Relation between Capacitated Miles and Total Revenue (from Area dataframe") +
  labs(x = "Capacitated Miles",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 10: EmptyMiles vs Total Revenue (Area dataframe)
ggplot(data = Area) +
  geom_point(aes(x = EmptyMiles, y = TotalRevenue)) +
  ggtitle("Relation between Empty Miles and Total Revenue (from Area dataframe") +
  labs(x = "Empty Miles",
       y = "Total Revenue") + 
  scale_fill_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("TotalRevenue") + # use only greyscale (always colorblind friendly)
  theme_bw()


########------Rides Dataframe (By Individual Ride wkst) Graphs-------

#Graph 1: Space vs PerformedDrive
ggplot(data = Rides) +
  geom_bar(aes(x = Space, fill = PerformedDrive), position="fill") +
  ggtitle("Relation Between Space and PerformedDrive") +
  labs(x = "Space",
       y = "Proportional Amount") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 2: Charge vs PerformedDrive
ggplot(data = Rides) +
  geom_point(aes(x = Charge, y = PerformedDrive)) +
  ggtitle("Relation Between Charge and PerformedDrive") +
  labs(x = "Space",
       y = "PerformedDrive") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 3: Type vs PerformedDrive
ggplot(data = Rides) +
  geom_bar(aes(x = Type, fill = PerformedDrive), position="fill") +
  ggtitle("Relation Between Type and PerformedDrive") +
  labs(x = "Type",
       y = "Proportional Amount") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 4: Distance vs PerformedDrive
ggplot(data = Rides) +
  geom_point(aes(x = Distance, y = PerformedDrive)) +
  ggtitle("Relation Between Distance and PerformedDrive") +
  labs(x = "Distance",
       y = "PerformedDrive") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()
#Graph 4 very similar to Graph 2 in terms of data

#Graph 5: Run vs PerformedDrive
ggplot(data = Rides) +
  geom_bar(aes(x = Run, fill = PerformedDrive), position="fill") +
  ggtitle("Relation Between Run and PerformedDrive") +
  labs(x = "Run",
       y = "Proportional Amount") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw() +
  coord_flip()
#Graph 5 Last two bars????

#Graph 6: PickupState vs PerformedDrive
ggplot(data = Rides) +
  geom_bar(aes(x = PickupState, fill = PerformedDrive), position="fill") +
  ggtitle("Relation Between Pickup State and PerformedDrive") +
  labs(x = "Pickup State",
       y = "Proportional Amount") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 7: DropoffState vs PerformedDrive
ggplot(data = Rides) +
  geom_bar(aes(x = DropoffState, fill = PerformedDrive), position="fill") +
  ggtitle("Relation Between Dropoff State and PerformedDrive") +
  labs(x = "Dropoff State",
       y = "Proportional Amount") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 8: RoutedDistance vs PerformedDrive
ggplot(data = Rides) +
  geom_point(aes(x = RoutedDistance, y = PerformedDrive)) +
  ggtitle("Relation Between Routed Distance and PerformedDrive") +
  labs(x = "Routed Distance",
       y = "PerformedDrive") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()

#Graph 9: ImportDistance vs PerformedDrive
ggplot(data = Rides) +
  geom_point(aes(x = ImportDistance, y = PerformedDrive)) +
  ggtitle("Relation Between Import Distance and PerformedDrive") +
  labs(x = "Import Distance",
       y = "PerformedDrive") + 
  scale_fill_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  scale_colour_grey("PerformedDrive") + # use only greyscale (always colorblind friendly)
  theme_bw()
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

