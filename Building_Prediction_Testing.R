library(e1071)
library(rpart)

set.seed(123)

####_#########################
#### Building prediction #####
####_#########################
sample1_building_prediction <- sample_n(wifiTrain, 18323)
sample1_building_prediction$USERID <- NULL
sample1_building_prediction$TIMESTAMP <- NULL
sample1_building_prediction$PHONEID <- NULL
sample1_building_prediction$SPACEID <- NULL
sample1_building_prediction$RELATIVEPOSITION <- NULL


train_control_1 <- trainControl(method ="repeatedcv",
                              number = 10
                              )


system.time(SVM_Building_prediction <- train(BUILDINGID ~. -LONGITUDE -LATITUDE -FLOOR,
                na.action = na.exclude,
                data = sample1_building_prediction,
                method = "svmLinear3",
                trControl = train_control_1))


saveRDS(SVM_Building_prediction, "SVM_Building_prediction.RDS")

####_########################
#### Test Building prediction####
####_########################

verification_Building <- wifiVeri
verification_Building$USERID <- NULL
verification_Building$TIMESTAMP <- NULL
verification_Building$PHONEID <- NULL
verification_Building$SPACEID <- NULL
verification_Building$RELATIVEPOSITION <- NULL


test_Building_Prediction <- predict(SVM_Building_prediction, newdata = verification_Building)

postResample(test_Building_Prediction, verification_Building$BUILDINGID)

# kNN with full sample cv = 10 repeated Accuracy 100%

# confusion Matrix
confusionMatrix(test_Building_Prediction, verification_Building$BUILDINGID)
#
# # which(verification_Building$BUILDINGID != testPrediction_kNN_s1)
# # verification_Building[which(verification_Building$BUILDINGID != testPrediction_kNN_s1),]
#
write.csv(testPrediction_kNN_s1, file = "UJIndoorLoc/BuildingPrediction.csv", row.names = FALSE)


####_#####################################
#### End of Building prediction script####
####_#####################################

