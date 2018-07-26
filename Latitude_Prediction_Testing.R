setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")
library("ggthemes")

####_============================================
#### Latitude prediction ####
####_============================================


sample3_Latitude_prediction <- sample_n(wifiTrain, 18323)
sample3_Latitude_prediction$USERID <- NULL
sample3_Latitude_prediction$TIMESTAMP <- NULL
sample3_Latitude_prediction$PHONEID <- NULL
sample3_Latitude_prediction$SPACEID <- NULL
sample3_Latitude_prediction$RELATIVEPOSITION <- NULL


train_control_3 <- trainControl(method ="repeatedcv",
                                number = 10)

####_###################################
#### Building 0 Latitude prediction ####
####_###################################

# select relevant Building
B0_latitude_prediction <- sample3_Latitude_prediction[which(sample3_Latitude_prediction$BUILDINGID == 0),]

# remove $BuildingID
B0_latitude_prediction$BUILDINGID <- NULL

# refactor floor prediction
B0_latitude_prediction$FLOOR <- NULL

# Train B0 Latitude prediction:
svm_B0_Latitude <- train(LATITUDE ~. -LONGITUDE,
                      na.action = na.exclude,
                      data = B0_latitude_prediction,
                      method = "svmLinear",
                      trControl = train_control_3)

knn_B0_Latitude <- train(LATITUDE ~. -LONGITUDE,
                         na.action = na.exclude,
                         data = B0_latitude_prediction,
                         method = "knn",
                         trControl = train_control_3)

# saveRDS
saveRDS(svm_B0_Latitude, "svm_B0_Latitude.RDS")
saveRDS(knn_B0_Latitude, "knn_B0_Latitude.RDS")


####_###################################
#### Building 1 Latitude prediction ####
####_###################################

# select relevant Building
B1_latitude_prediction <- sample3_Latitude_prediction[which(sample3_Latitude_prediction$BUILDINGID == 1),]

# remove $BuildingID
B1_latitude_prediction$BUILDINGID <- NULL

# refactor floor prediction
B1_latitude_prediction$FLOOR <- NULL

# Train B1 Latitude prediction:
svm_B1_Latitude <- train(LATITUDE ~. -LONGITUDE,
                         na.action = na.exclude,
                         data = B1_latitude_prediction,
                         method = "svmLinear",
                         trControl = train_control_3)

# Train B1 Latitude prediction:
knn_B1_Latitude <- train(LATITUDE ~. -LONGITUDE,
                         na.action = na.exclude,
                         data = B1_latitude_prediction,
                         method = "knn",
                         trControl = train_control_3)

# saveRDS
saveRDS(svm_B1_Latitude, "svm_B1_Latitude.RDS")
saveRDS(knn_B1_Latitude, "knn_B1_Latitude.RDS")


####_###################################
#### Building 2 Latitude prediction ####
####_###################################

# select relevant Building
B2_latitude_prediction <- sample3_Latitude_prediction[which(sample3_Latitude_prediction$BUILDINGID == 2),]

# remove $BuildingID
B2_latitude_prediction$BUILDINGID <- NULL

# refactor floor prediction
B2_latitude_prediction$FLOOR <- NULL

# Train B2 Latitude prediction:
svm_B2_Latitude <- train(LATITUDE ~. -LONGITUDE,
                         na.action = na.exclude,
                         data = B2_latitude_prediction,
                         method = "svmLinear",
                         trControl = train_control_3)

# Train B2 Latitude prediction:
knn_B2_Latitude <- train(LATITUDE ~. -LONGITUDE,
                         na.action = na.exclude,
                         data = B2_latitude_prediction,
                         method = "knn",
                         trControl = train_control_3)

# saveRDS
saveRDS(svm_B2_Latitude, "svm_B2_Latitude.RDS")
saveRDS(knn_B2_Latitude, "knn_B2_Latitude.RDS")











####_============================================
#### Latitude Testing ####
####_============================================
buildingPrediction <- as.factor(read.csv("UJIndoorLoc/BuildingPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])


####_###################################
#### Building 0 Latitude prediction ####
####_###################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B0_latitude_verification <- wifiVeri
B0_latitude_verification$BUILDINGID <- buildingPrediction

B0_latitude_verification <- B0_latitude_verification[which(B0_latitude_verification$BUILDINGID == 0) ,]

B0_latitude_verification <- B0_latitude_verification[, which(names(B0_latitude_verification) %in% names(B0_latitude_prediction))]
ncol(B0_floor_prediction)
ncol(B0_floor_verification)

# test prediction
test_svm_B0_Latitude <- predict(svm_B0_Latitude, newdata = B0_latitude_verification)
postResample(test_svm_B0_Latitude, B0_latitude_verification$LATITUDE)

# test prediction
test_knn_B0_Latitude <- predict(knn_B0_Latitude, newdata = B0_latitude_verification)
postResample(test_knn_B0_Latitude, B0_latitude_verification$LATITUDE)


# save prediction as csv
write.csv(test_knn_B0_Latitude, file = "UJIndoorLoc/B0_LatitudePrediction.csv", row.names = FALSE)


####_###################################
#### Building 1 Latitude prediction ####
####_###################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B1_latitude_verification <- wifiVeri
B1_latitude_verification$BUILDINGID <- buildingPrediction

B1_latitude_verification <- B1_latitude_verification[which(B1_latitude_verification$BUILDINGID == 1) ,]

B1_latitude_verification <- B1_latitude_verification[, which(names(B1_latitude_verification) %in% names(B1_latitude_prediction))]
ncol(B1_floor_prediction)
ncol(B1_floor_verification)

# test prediction
test_svm_B1_Latitude <- predict(svm_B1_Latitude, newdata = B1_latitude_verification)
postResample(test_svm_B1_Latitude, B1_latitude_verification$LATITUDE)

# test prediction
test_knn_B1_Latitude <- predict(knn_B1_Latitude, newdata = B1_latitude_verification)
postResample(test_knn_B1_Latitude, B1_latitude_verification$LATITUDE)


# save prediction as csv
write.csv(test_knn_B1_Latitude, file = "UJIndoorLoc/B1_LatitudePrediction.csv", row.names = FALSE)


####_###################################
#### Building 1 Latitude prediction ####
####_###################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B2_latitude_verification <- wifiVeri
B2_latitude_verification$BUILDINGID <- buildingPrediction

B2_latitude_verification <- B2_latitude_verification[which(B2_latitude_verification$BUILDINGID == 2) ,]

B2_latitude_verification <- B2_latitude_verification[, which(names(B2_latitude_verification) %in% names(B2_latitude_prediction))]
ncol(B2_floor_prediction)
ncol(B2_floor_verification)

# test prediction
test_svm_B2_Latitude <- predict(svm_B2_Latitude, newdata = B2_latitude_verification)
postResample(test_svm_B2_Latitude, B2_latitude_verification$LATITUDE)

# test prediction
test_knn_B2_Latitude <- predict(knn_B2_Latitude, newdata = B2_latitude_verification)
postResample(test_knn_B2_Latitude, B2_latitude_verification$LATITUDE)

# save prediction as csv
write.csv(test_knn_B2_Latitude, file = "UJIndoorLoc/B2_LatitudePrediction.csv", row.names = FALSE)


####_#####################################
#### End of Latitude Testing script####
####_#####################################
