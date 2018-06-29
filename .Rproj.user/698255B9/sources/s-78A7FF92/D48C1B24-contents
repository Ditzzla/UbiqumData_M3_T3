setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")

wifiTrain <- as.data.frame(read.csv("UJIndoorLoc/trainingData.csv", sep = ",", stringsAsFactors = FALSE))

###################################################
#### initial data investigation ##################
###################################################
nrow(wifiTrain)
ncol(wifiTrain)

#### plot Long Lat Floor to investigate Location ####
ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$FLOOR)) + geom_point()
ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$BUILDINGID)) + geom_point()
ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$RELATIVEPOSITION)) + geom_point()




#### change data type ####
str(wifiTrain[,(ncol(wifiTrain)-8):ncol(wifiTrain)])


