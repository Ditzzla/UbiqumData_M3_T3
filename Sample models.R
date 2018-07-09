library(e1071)
library(rpart)

#####################
#### sample data ####
#####################

# build2 <- wifiTrain[which(wifiTrain$BUILDINGID == "2"),]
# build1 <- wifiTrain[which(wifiTrain$BUILDINGID == "1"),]
# build0 <- wifiTrain[which(wifiTrain$BUILDINGID == "0"),]
set.seed(123)
#############################
#### Building prediction ####
#############################
sample1_building_prediction <- sample_n(wifiTrain, 19227)
sample1_building_prediction$USERID <- NULL
sample1_building_prediction$TIMESTAMP <- NULL
sample1_building_prediction$PHONEID <- NULL
#sample1_building_prediction <- sample1_building_prediction[,-c((ncol(sample1_building_prediction)-8):(ncol(sample1_building_prediction)-6),(ncol(sample1_building_prediction)-4):ncol(sample1_building_prediction))]
smp1_rows <- sample.int(n = nrow(sample1_building_prediction), size = floor(.75 * nrow(sample1_building_prediction)), replace = F)

s1_train <- sample1_building_prediction[smp1_rows,]
s1_test <- sample1_building_prediction[-smp1_rows,]

train_control_1 <- trainControl(method ="repeatedcv",
                              number = 10
                              )


kNN_s1 <- train(BUILDINGID ~. -LONGITUDE -LATITUDE -FLOOR -SPACEID -RELATIVEPOSITION,
                data = s1_train,
                method = "knn",
                trControl = train_control_1)

testPrediction_kNN_s1 <- predict(kNN_s1, newdata = s1_test)

postResample(testPrediction_kNN_s1, s1_test$BUILDINGID)
# kNN with full sample cv = 10 repeated Accuracy 100%

# confusion Matrix
confusionMatrix(testPrediction_kNN_s1, s1_test$BUILDINGID)

which(s1_test$BUILDINGID != testPrediction_kNN_s1)
s1_test[which(s1_test$BUILDINGID != testPrediction_kNN_s1),]

write.csv(testPrediction_kNN_s1, file = "UJIndoorLoc/BuildingPrediction.csv")

##########################
#### Floor prediction ####
##########################

#### add predicted Building to dataframe ####
buildingPrediction <- read.csv("UJIndoorLoc/BuildingPrediction.csv", sep = ",", stringsAsFactors = FALSE)
sample2_floor_prediction <- sample1_building_prediction
sample2_floor_prediction$PBuildID <- testPrediction_kNN_s1

set.seed(123)
smp2_rows <- sample.int(n = nrow(sample2_floor_prediction), size = floor(.75 * nrow(sample2_floor_prediction)), replace = F)

s2_train <- sample2_floor_prediction[smp2_rows,]
s2_test <- sample2_floor_prediction[-smp2_rows,]

train_control_2 <- trainControl(method ="repeatedcv",
                                number = 10
)

#=============================================
#### FLOOR prediction kNN ####
#=============================================

kNN_s2 <- train(FLOOR ~. -LONGITUDE -LATITUDE -BUILDINGID -SPACEID -RELATIVEPOSITION,
                data = s2_train,
                method = "knn",
                trControl = train_control_2)
###########
testPrediction_kNN_s2 <- predict(kNN_s2, newdata = s2_test)

postResample(testPrediction_kNN_s2, s2_test$FLOOR)

#=============================================
#### FLOOR prediction SVM ####
#=============================================

SVM_s2 <- train(FLOOR ~. -LONGITUDE -LATITUDE -BUILDINGID -SPACEID -RELATIVEPOSITION,
                data = s2_train,
                method = "svmLinear",
                trControl = train_control_2)

testPrediction_SVM_s2 <- predict(SVM_s2, newdata = s2_test)

postResample(testPrediction_SVM_s2, s2_test$FLOOR)

#=============================================
#### FLOOR prediction C5.0 ####
#=============================================


system.time(
  C50_s2 <- train(FLOOR ~. -LONGITUDE -LATITUDE -BUILDINGID -SPACEID -RELATIVEPOSITION,
                            data = s2_train,
                            method = "C5.0",
                            trControl = train_control_2)
  )

testPrediction_C50_s2 <- predict(C50_s2, newdata = s2_test)

postResample(testPrediction_C50_s2, s2_test$FLOOR)

# highest Accuracy with SVM 97,8%

##############################
#### Longitude prediction ####
##############################

#### add predicted Floor to dataframe ####
sample3_floor_prediction <- sample2_floor_prediction
sample3_floor_prediction$PFloor <- testPrediction_SVM_s2

#### predict long/lat ####