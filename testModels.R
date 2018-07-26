
############################
# Test Building prediction##
############################
# 
# verification_Building <- wifiVeri
# verification_Building$USERID <- NULL
# verification_Building$TIMESTAMP <- NULL
# verification_Building$PHONEID <- NULL
# verification_Building$SPACEID <- NULL
# verification_Building$RELATIVEPOSITION <- NULL
# 
# 
# testPrediction_kNN_s1 <- predict(kNN_s1, newdata = verification_Building)
# 
# postResample(testPrediction_kNN_s1, verification_Building$BUILDINGID)
# 
# # kNN with full sample cv = 10 repeated Accuracy 100%
# 
# # confusion Matrix
# confusionMatrix(testPrediction_kNN_s1, verification_Building$BUILDINGID)
# # 
# # # which(verification_Building$BUILDINGID != testPrediction_kNN_s1)
# # # verification_Building[which(verification_Building$BUILDINGID != testPrediction_kNN_s1),]
# # 
# write.csv(testPrediction_kNN_s1, file = "UJIndoorLoc/BuildingPrediction.csv", row.names = FALSE)


############################
#### Test Floor prediction ####
############################

#### add predicted Building to dataframe ####
buildingPrediction <- as.factor(read.csv("UJIndoorLoc/BuildingPrediction.csv", sep = ",", stringsAsFactors = FALSE)[,1])

#####################################
#### Building 0 Floor prediction ####
#####################################
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

# insert correlation matrix

# save prediction as csv
write.csv(test_svm_B0_Floor, file = "UJIndoorLoc/B0_FloorPrediction.csv", row.names = FALSE)


#####################################
#### Building 1 Floor prediction ####
#####################################
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

# confusion Matrix
# confusionMatrix(test_svm_B1_Floor, B1_floor_verification$FLOOR)

# save prediction as csv
write.csv(test_svm_B0_Floor, file = "UJIndoorLoc/B0_FloorPrediction.csv", row.names = FALSE)


##################################
#### Test ******** prediction ####
##################################




