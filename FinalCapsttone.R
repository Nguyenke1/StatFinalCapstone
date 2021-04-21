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
Area <- read_excel("By Date - Dec 2019, Jan 2020, Feb 2020.xlsx")
Drivers <- read_excel("By Driver - Dec 2019, Jan 2020, Feb 2020.xlsx")
Rides <- read_excel("By Individual Ride - Dec 2019, Jan 2020, Feb 2020.xlsx")
