# Libraries ---------------------------------------------------------------
library(dplyr)#for aggregating, data manipulation, etc.
library(ggplot2)#professional graphics
library(lubridate)#for working with dates
library(readxl) #we were given a master excel sheet
library(randomForest)
library(pROC) #for ROC curves

# Run a chunk on Mac: cmd + option + t
# Run a chunk on Windows: ctrl + alt + t
# This way you don't have to keep highlighting and running code when you open
# R studio each time

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
colnames(Area) <- gsub("[\r\n -]","",colnames(Area))
colnames(Drivers) <- gsub("[\r\n -]","",colnames(Drivers))
colnames(Rides) <- gsub("[\r\n -]","",colnames(Rides))

Rides = Rides[-c(2548,2549),] #remove the improperly formatted rows

#make the unperformed ride column

Rides$PerformedDrive <- ifelse(Rides$Dropoff.Perform == is.null((Rides$Dropoff.Perform)),0,1)

unique(Rides$PerformedDrive)

