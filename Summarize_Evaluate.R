setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")
library("ggthemes")


####_============================================
#### Create Verification Dataset with Original values and Prediction ####
####_============================================

# Building prediction
dfEvaluation <- wifiVeri[,(ncol(wifiVeri)-8):ncol(wifiVeri)]

dfEvaluation$USERID <- NULL
dfEvaluation$SPACEID <- NULL
dfEvaluation$PHONEID <- NULL
dfEvaluation$TIMESTAMP <- NULL
dfEvaluation$RELATIVEPOSITION <- NULL


buildingPrediction <- as.factor(read.csv("UJIndoorLoc/BuildingPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
floorPrediction_B0 <- as.factor(read.csv("UJIndoorLoc/B0_FloorPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
floorPrediction_B1 <- as.factor(read.csv("UJIndoorLoc/B1_FloorPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
floorPrediction_B2 <- as.factor(read.csv("UJIndoorLoc/B2_FloorPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
latPrediction_B0 <- as.integer(read.csv("UJIndoorLoc/B0_LatitudePrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
latPrediction_B1 <- as.integer(read.csv("UJIndoorLoc/B1_LatitudePrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
latPrediction_B2 <- as.integer(read.csv("UJIndoorLoc/B2_LatitudePrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
longPrediction_B0 <- as.integer(read.csv("UJIndoorLoc/B0_LongitudePrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
longPrediction_B1 <- as.integer(read.csv("UJIndoorLoc/B1_LongitudePrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])
longPrediction_B2 <- as.integer(read.csv("UJIndoorLoc/B2_LongitudePrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])


dfEvaluation$buildingPred <- buildingPrediction

# split for Buildings and add Floor, Lat and Long predictions
# B0
B0_dfEvaluation <- dfEvaluation[which(dfEvaluation$BUILDINGID == 0),]
B0_dfEvaluation$floorPred <- floorPrediction_B0
B0_dfEvaluation$latPred <- latPrediction_B0
B0_dfEvaluation$longPred <- longPrediction_B0
B0_dfEvaluation <- mutate(B0_dfEvaluation, distance = sqrt((LONGITUDE - longPred)^2 + ((LATITUDE - latPred)^2)))
ggplot(data = B0_dfEvaluation) + geom_histogram(aes(x = distance), bins = 50) + ggtitle("B0 histogram of distance: prediction to location") #+ xlim(0, 500)
summary(B0_dfEvaluation$distance)

ggplot(data = B0_dfEvaluation) + geom_point(aes(x = B0_dfEvaluation$LONGITUDE, y = B0_dfEvaluation$longPred))
ggplot(data = B0_dfEvaluation) + geom_point(aes(x = B0_dfEvaluation$LONGITUDE, y = B0_dfEvaluation$distance))
ggplot(data = B0_dfEvaluation) + geom_point(aes(x = B0_dfEvaluation$LATITUDE, y = B0_dfEvaluation$distance))

# B1
B1_dfEvaluation <- dfEvaluation[which(dfEvaluation$BUILDINGID == 1),]
B1_dfEvaluation$floorPred <- floorPrediction_B1
B1_dfEvaluation$latPred <- latPrediction_B1
B1_dfEvaluation$longPred <- longPrediction_B1
B1_dfEvaluation <- mutate(B1_dfEvaluation, distance = sqrt((LONGITUDE - longPred)^2 + ((LATITUDE - latPred)^2)))
ggplot(data = B1_dfEvaluation) + geom_histogram(aes(x = distance), bins = 50) + ggtitle("B1 histogram of distance: prediction to location") #+ xlim(0, 500)
summary(B1_dfEvaluation$distance)

# B2
B2_dfEvaluation <- dfEvaluation[which(dfEvaluation$BUILDINGID == 2),]
B2_dfEvaluation$floorPred <- floorPrediction_B2
B2_dfEvaluation$latPred <- latPrediction_B2
B2_dfEvaluation$longPred <- longPrediction_B2
B2_dfEvaluation <- mutate(B2_dfEvaluation, distance = sqrt((LONGITUDE - longPred)^2 + ((LATITUDE - latPred)^2)))
ggplot(data = B2_dfEvaluation) + geom_histogram(aes(x = distance), bins = 50) + ggtitle("B2 histogram of distance: prediction to location") #+ xlim(0, 500)
summary(B2_dfEvaluation$distance)

# remerge to 1 dataframe
evaluation <- rbind(B0_dfEvaluation, B1_dfEvaluation, B2_dfEvaluation)
nrow(evaluation)
summary(evaluation$distance)

# histogram of distance
ggplot(data = evaluation) + geom_histogram(aes(x = distance), bins = 50) + ggtitle("histogram of distance: prediction to location") #+ xlim(0, 500)
summary(evaluation$distance)

# plot prediction long lat
ggplot(data = evaluation, aes(x = evaluation$latPred, y = evaluation$longPred, colour = evaluation$BUILDINGID)) + geom_jitter()

ggplot(data = evaluation) + geom_point(aes(x = evaluation$LONGITUDE, y = evaluation$distance, colour = evaluation$BUILDINGID))
ggplot(data = evaluation) + geom_point(aes(x = evaluation$LATITUDE, y = evaluation$distance, colour = evaluation$BUILDINGID))


# facet grid:
ggplot(data = evaluation) + geom_point(aes(x = evaluation$LATITUDE, y = evaluation$LONGITUDE, colour = evaluation$distance)) + facet_grid(BUILDINGID ~ FLOOR)
# facet grid: top 50 points






