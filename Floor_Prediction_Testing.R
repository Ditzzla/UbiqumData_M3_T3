setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")
library("ggthemes")
library(e1071)
library(rpart)

set.seed(123)

####_============================================
#### Floor prediction ####
####_============================================


sample2_floor_prediction <- sample_n(wifiTrain, 18323)
sample2_floor_prediction$USERID <- NULL
sample2_floor_prediction$TIMESTAMP <- NULL
sample2_floor_prediction$PHONEID <- NULL
sample2_floor_prediction$SPACEID <- NULL
sample2_floor_prediction$RELATIVEPOSITION <- NULL


train_control_2 <- trainControl(method ="repeatedcv",
                                number = 10)

####_#################################
#### Building 0 Floor prediction ####
####_#################################

B0_floor_prediction <- sample2_floor_prediction[which(sample2_floor_prediction$BUILDINGID == 0),]
#### remove unnecessary columns
which(apply(B0_floor_prediction, 2, var) == 0)
if (length(which(apply(B0_floor_prediction, 2, var) == 0)) > 0){
  B0_floor_prediction <- B0_floor_prediction[, -as.numeric(which(apply(B0_floor_prediction, 2, var) == 0))]
}

# refactor as building 0 has no floor 4
B0_floor_prediction$FLOOR <- factor(B0_floor_prediction$FLOOR)
ncol(wifiTrain)
ncol(B0_floor_prediction)

# Train B0 floor prediction:
svm_B0_Floor <- train(FLOOR ~. -LONGITUDE -LATITUDE,
                na.action = na.exclude,
                data = B0_floor_prediction,
                method = "svmLinear",
                trControl = train_control_2)

saveRDS(svm_B0_Floor, "svm_B0_Floor.RDS")

####_#################################
#### Building 1 Floor prediction ####
####_#################################

B1_floor_prediction <- sample2_floor_prediction[which(sample2_floor_prediction$BUILDINGID == 1),]
#### remove unnecessary columns
which(apply(B1_floor_prediction, 2, var) == 0)
if (length(which(apply(B1_floor_prediction, 2, var) == 0)) > 0){
  B1_floor_prediction <- B1_floor_prediction[, -as.numeric(which(apply(B1_floor_prediction, 2, var) == 0))]
}

# refactor floor levels
B1_floor_prediction$FLOOR <- factor(B1_floor_prediction$FLOOR)
ncol(wifiTrain)
ncol(B1_floor_prediction)

#svm Train B1 floor prediction:
svm_B1_Floor <- train(FLOOR ~. -LONGITUDE -LATITUDE,
                      na.action = na.exclude,
                      data = B1_floor_prediction,
                      method = "svmLinear",
                      trControl = train_control_2)


saveRDS(svm_B1_Floor, "svm_B1_Floor.RDS")

####_#################################
#### Building 2 Floor prediction ####
####_#################################

B2_floor_prediction <- sample2_floor_prediction[which(sample2_floor_prediction$BUILDINGID == 2),]
#### remove unnecessary columns
which(apply(B2_floor_prediction, 2, var) == 0)
if (length(which(apply(B2_floor_prediction, 2, var) == 0)) > 0){
  B2_floor_prediction <- B2_floor_prediction[, -as.numeric(which(apply(B2_floor_prediction, 2, var) == 0))]
}

# refactor floor levels
B2_floor_prediction$FLOOR <- factor(B2_floor_prediction$FLOOR)
ncol(wifiTrain)
ncol(B2_floor_prediction)

# Train B2 floor prediction:
svm_B2_Floor <- train(FLOOR ~. -LONGITUDE -LATITUDE,
                      na.action = na.exclude,
                      data = B2_floor_prediction,
                      method = "svmLinear",
                      trControl = train_control_2)

saveRDS(svm_B2_Floor, "svm_B2_Floor.RDS")

####_============================================
#### Floor Testing ####
####_============================================

#### add predicted Building to dataframe ####
buildingPrediction <- as.factor(read.csv("UJIndoorLoc/BuildingPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])

####_#################################
#### Building 0 Floor prediction ####
####_#################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B0_floor_verification <- wifiVeri
B0_floor_verification$BUILDINGID <- buildingPrediction

B0_floor_verification <- B0_floor_verification[which(B0_floor_verification$BUILDINGID == 0) ,]

B0_floor_verification <- B0_floor_verification[, which(names(B0_floor_verification) %in% names(B0_floor_prediction))]
ncol(B0_floor_prediction)
ncol(B0_floor_verification)

# highest Accuracy with SVMLinear 97%
test_svm_B0_Floor <- predict(svm_B0_Floor, newdata = B0_floor_verification)
postResample(test_svm_B0_Floor, B0_floor_verification$FLOOR)

# save prediction as csv
write.csv(test_svm_B0_Floor, file = "UJIndoorLoc/B0_FloorPrediction.csv", row.names = FALSE)

####_#################################
#### Building 1 Floor prediction ####
####_#################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B1_floor_verification <- wifiVeri
B1_floor_verification$BUILDINGID <- buildingPrediction

B1_floor_verification <- B1_floor_verification[which(B1_floor_verification$BUILDINGID == 1) ,]

B1_floor_verification <- B1_floor_verification[, which(names(B1_floor_verification) %in% names(B1_floor_prediction))]
ncol(B1_floor_prediction)
ncol(B1_floor_verification)

# highest Accuracy with SVMLinear 97%
test_svm_B1_Floor <- predict(svm_B1_Floor, newdata = B1_floor_verification)
postResample(test_svm_B1_Floor, B1_floor_verification$FLOOR)

# confusionMatrix(test_svm_B1_Floor, B1_floor_verification$FLOOR)

# save prediction as csv
write.csv(test_svm_B1_Floor, file = "UJIndoorLoc/B1_FloorPrediction.csv", row.names = FALSE)



####_#################################
#### Building 2 Floor prediction ####
####_#################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B2_floor_verification <- wifiVeri
B2_floor_verification$BUILDINGID <- buildingPrediction

B2_floor_verification <- B2_floor_verification[which(B2_floor_verification$BUILDINGID == 2) ,]

B2_floor_verification <- B2_floor_verification[, which(names(B2_floor_verification) %in% names(B2_floor_prediction))]
ncol(B2_floor_prediction)
ncol(B2_floor_verification)

# highest Accuracy with SVMLinear 97%
test_svm_B2_Floor <- predict(svm_B2_Floor, newdata = B2_floor_verification)
postResample(test_svm_B2_Floor, B2_floor_verification$FLOOR)


# save prediction as csv
write.csv(test_svm_B2_Floor, file = "UJIndoorLoc/B2_FloorPrediction.csv", row.names = FALSE)



####_#####################################
#### End of Floor Testing script####
####_#####################################


