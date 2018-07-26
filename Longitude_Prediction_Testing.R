setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")
library("ggthemes")

####_============================================
#### Longitude prediction ####
####_============================================

train_control_4 <- trainControl(method ="repeatedcv",
                                number = 10)

####_###################################
#### Building 0 Latitude prediction ####
####_###################################

# select relevant Building
B0_longitude_prediction <- sample3_Latitude_prediction[which(sample3_Latitude_prediction$BUILDINGID == 0),]

# remove $BuildingID
B0_longitude_prediction$BUILDINGID <- NULL

# refactor floor prediction
B0_longitude_prediction$FLOOR <- NULL

# Train B0 Latitude prediction:
svm_B0_Longitude <- train(LONGITUDE ~. -LATITUDE,
                      na.action = na.exclude,
                      data = B0_longitude_prediction,
                      method = "svmLinear",
                      trControl = train_control_4)

# Train B0 Latitude prediction:
knn_B0_Longitude <- train(LONGITUDE ~. -LATITUDE,
                          na.action = na.exclude,
                          data = B0_longitude_prediction,
                          method = "knn",
                          trControl = train_control_4)

# saveRDS
saveRDS(svm_B0_Longitude, "svm_B0_Longitude.RDS")
saveRDS(knn_B0_Longitude, "knn_B0_Longitude.RDS")



####_###################################
#### Building 1 Latitude prediction ####
####_###################################

# select relevant Building
B1_longitude_prediction <- sample3_Latitude_prediction[which(sample3_Latitude_prediction$BUILDINGID == 1),]

# remove $BuildingID
B1_longitude_prediction$BUILDINGID <- NULL

# refactor floor prediction
B1_longitude_prediction$FLOOR <- NULL

# Train B1 Longitude prediction:
svm_B1_Longitude <- train(LONGITUDE ~. -LATITUDE,
                          na.action = na.exclude,
                          data = B1_longitude_prediction,
                          method = "svmLinear",
                          trControl = train_control_4)

# Train B1 Longitude prediction:
knn_B1_Longitude <- train(LONGITUDE ~. -LATITUDE,
                          na.action = na.exclude,
                          data = B1_longitude_prediction,
                          method = "knn",
                          trControl = train_control_4)

# saveRDS
saveRDS(svm_B1_Longitude, "svm_B1_Longitude.RDS")
saveRDS(knn_B1_Longitude, "knn_B1_Longitude.RDS")



####_###################################
#### Building 2 Latitude prediction ####
####_###################################

# select relevant Building
B2_longitude_prediction <- sample3_Latitude_prediction[which(sample3_Latitude_prediction$BUILDINGID == 2),]

# remove $BuildingID
B2_longitude_prediction$BUILDINGID <- NULL

# refactor floor prediction
B2_longitude_prediction$FLOOR <- NULL

# Train B2 Longitude prediction:
svm_B2_Longitude <- train(LONGITUDE ~. -LATITUDE,
                          na.action = na.exclude,
                          data = B2_longitude_prediction,
                          method = "svmLinear",
                          trControl = train_control_4)

# Train B2 Longitude prediction:
knn_B2_Longitude <- train(LONGITUDE ~. -LATITUDE,
                          na.action = na.exclude,
                          data = B2_longitude_prediction,
                          method = "knn",
                          trControl = train_control_4)

# saveRDS
saveRDS(svm_B2_Longitude, "svm_B2_Longitude.RDS")
saveRDS(knn_B2_Longitude, "knn_B2_Longitude.RDS")








####_============================================
#### Longitude Testing ####
####_============================================
buildingPrediction <- as.factor(read.csv("UJIndoorLoc/BuildingPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])


####_###################################
#### Building 0 Longitude prediction ####
####_###################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B0_longitude_verification <- wifiVeri
B0_longitude_verification$BUILDINGID <- buildingPrediction

B0_longitude_verification <- B0_longitude_verification[which(B0_longitude_verification$BUILDINGID == 0) ,]

B0_longitude_verification <- B0_longitude_verification[, which(names(B0_longitude_verification) %in% names(B0_longitude_prediction))]
ncol(B0_longitude_prediction)
ncol(B0_longitude_verification)

# test prediction
test_svm_B0_Longitude <- predict(svm_B0_Longitude, newdata = B0_longitude_verification)
postResample(test_svm_B0_Longitude, B0_longitude_verification$LONGITUDE)

# test prediction
test_knn_B0_Longitude <- predict(knn_B0_Longitude, newdata = B0_longitude_verification)
postResample(test_knn_B0_Longitude, B0_longitude_verification$LONGITUDE)

# save prediction as csv
write.csv(test_knn_B0_Longitude, file = "UJIndoorLoc/B0_LongitudePrediction.csv", row.names = FALSE)


####_###################################
#### Building 1 Longitude prediction ####
####_###################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B1_longitude_verification <- wifiVeri
B1_longitude_verification$BUILDINGID <- buildingPrediction

B1_longitude_verification <- B1_longitude_verification[which(B1_longitude_verification$BUILDINGID == 1) ,]

B1_longitude_verification <- B1_longitude_verification[, which(names(B1_longitude_verification) %in% names(B1_longitude_prediction))]
ncol(B1_longitude_prediction)
ncol(B1_longitude_verification)

# test prediction
test_svm_B1_Longitude <- predict(svm_B1_Longitude, newdata = B1_longitude_verification)
postResample(test_svm_B1_Longitude, B1_longitude_verification$LONGITUDE)

# test prediction
test_svm_B1_Longitude <- predict(svm_B1_Longitude, newdata = B1_longitude_verification)
postResample(test_svm_B1_Longitude, B1_longitude_verification$LONGITUDE)

# test prediction
test_knn_B1_Longitude <- predict(knn_B1_Longitude, newdata = B1_longitude_verification)
postResample(test_knn_B1_Longitude, B1_longitude_verification$LONGITUDE)

# save prediction as csv
write.csv(test_knn_B1_Longitude, file = "UJIndoorLoc/B1_LongitudePrediction.csv", row.names = FALSE)


####_###################################
#### Building 2 Longitude prediction ####
####_###################################
# replace BuildingID with predicted values, remove all buildings except the one for the model, remove columns
B2_longitude_verification <- wifiVeri
B2_longitude_verification$BUILDINGID <- buildingPrediction

B2_longitude_verification <- B2_longitude_verification[which(B2_longitude_verification$BUILDINGID == 2) ,]

B2_longitude_verification <- B2_longitude_verification[, which(names(B2_longitude_verification) %in% names(B2_longitude_prediction))]
ncol(B2_longitude_prediction)
ncol(B2_longitude_verification)

# test prediction
test_svm_B2_Longitude <- predict(svm_B2_Longitude, newdata = B2_longitude_verification)
postResample(test_svm_B2_Longitude, B2_longitude_verification$LONGITUDE)

# test prediction
test_knn_B2_Longitude <- predict(knn_B2_Longitude, newdata = B2_longitude_verification)
postResample(test_knn_B2_Longitude, B2_longitude_verification$LONGITUDE)

# save prediction as csv
write.csv(test_knn_B2_Longitude, file = "UJIndoorLoc/B2_LongitudePrediction.csv", row.names = FALSE)

####_#####################################
#### End of Longitude Testing script####
####_#####################################
