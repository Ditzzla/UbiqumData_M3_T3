library(e1071)
library(rpart)

set.seed(123)

#############################
#### Building prediction ####
#############################
# sample1_building_prediction <- sample_n(wifiTrain, 18323)
# sample1_building_prediction$USERID <- NULL
# sample1_building_prediction$TIMESTAMP <- NULL
# sample1_building_prediction$PHONEID <- NULL
# sample1_building_prediction$SPACEID <- NULL
# sample1_building_prediction$RELATIVEPOSITION <- NULL
# 
# 
# train_control_1 <- trainControl(method ="repeatedcv",
#                               number = 10
#                               )
# 
# 
# system.time(kNN_s1 <- train(BUILDINGID ~. -LONGITUDE -LATITUDE -FLOOR,
#                 na.action = na.exclude,
#                 data = sample1_building_prediction,
#                 method = "svmLinear3",
#                 trControl = train_control_1))


####============================================
#### Floor prediction ####
####============================================
set.seed(123)

sample2_floor_prediction <- sample_n(wifiTrain, 18323)
sample2_floor_prediction$USERID <- NULL
sample2_floor_prediction$TIMESTAMP <- NULL
sample2_floor_prediction$PHONEID <- NULL
sample2_floor_prediction$SPACEID <- NULL
sample2_floor_prediction$RELATIVEPOSITION <- NULL


train_control_2 <- trainControl(method ="repeatedcv",
                                number = 10)

#####################################
#### Building 0 Floor prediction ####
#####################################

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


#####################################
#### Building 1 Floor prediction ####
#####################################

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

# Train B0 floor prediction:
svm_B1_Floor <- train(FLOOR ~. -LONGITUDE -LATITUDE,
                      na.action = na.exclude,
                      data = B1_floor_prediction,
                      method = "svmLinear",
                      trControl = train_control_2)

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

# Train B0 floor prediction:
svm_B2_Floor <- train(FLOOR ~. -LONGITUDE -LATITUDE,
                      na.action = na.exclude,
                      data = B2_floor_prediction,
                      method = "svmLinear",
                      trControl = train_control_2)






##############################
#### Longitude prediction ####
##############################

#### add predicted Floor to dataframe ####
sample3_floor_prediction <- sample2_floor_prediction
sample3_floor_prediction$PFloor <- testPrediction_SVM_s2

#### predict long/lat ####