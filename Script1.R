setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")
library("ggthemes")

wifiTrain <- as.data.frame(read.csv("UJIndoorLoc/trainingData.csv", sep = ",", stringsAsFactors = FALSE))
wifiVerification <- as.data.frame(read.csv("UJIndoorLoc/validationData.csv", sep = ",", stringsAsFactors = FALSE))

###################################################
#### initial data investigation ##################
###################################################
# nrow(wifiTrain)
# ncol(wifiTrain)


#### change data type ####
str(wifiTrain[,(ncol(wifiTrain)-8):ncol(wifiTrain)])
wifiTrain$LONGITUDE <- as.integer(wifiTrain$LONGITUDE)
wifiTrain$LATITUDE <- as.integer(wifiTrain$LATITUDE)
wifiTrain$FLOOR <- as.factor(wifiTrain$FLOOR)
wifiTrain$BUILDINGID <- as.factor(wifiTrain$BUILDINGID)
wifiTrain$SPACEID <- as.factor(wifiTrain$SPACEID)
wifiTrain$RELATIVEPOSITION <- as.factor(wifiTrain$RELATIVEPOSITION)
wifiTrain$USERID <- as.factor(wifiTrain$USERID)
wifiTrain$PHONEID <- as.factor(wifiTrain$PHONEID)


#### plot Long Lat Floor to investigate Location ####
# positionPlot <- ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$FLOOR)) + geom_point()
# positionPlot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$BUILDINGID)) + geom_point()
# ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$RELATIVEPOSITION)) + geom_point()
# ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$SPACEID)) + geom_point()
# ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$FLOOR)) + geom_jitter()
# ggplot(data = wifiTrain, aes(x = wifiTrain$FLOOR, y = wifiTrain$RELATIVEPOSITION, colour = wifiTrain$BUILDINGID)) + geom_jitter()
# ggplot(data = wifiTrain, aes(x = wifiTrain$SPACEID, y = wifiTrain$FLOOR, colour = wifiTrain$BUILDINGID)) + geom_jitter()
# ggplot(data = wifiTrain, aes(x = wifiTrain$SPACEID, y = wifiTrain$RELATIVEPOSITION, colour = wifiTrain$BUILDINGID)) + geom_jitter()
# ggplot(data = wifiTrain, aes(x = wifiTrain$FLOOR, y = wifiTrain$BUILDINGID, colour = wifiTrain$RELATIVEPOSITION)) + geom_jitter()
# ggplot(data = wifiTrain, aes(x = wifiTrain$LATITUDE, y = wifiTrain$LONGITUDE, colour = wifiTrain$USERID)) + geom_jitter()
# plot(wifiTrain$WAP070)

###################################################
#### preprocess ##################
###################################################

#### replace 100s with smallest value: -105 ####
minAbsoluteSignal <- min(wifiTrain[,1:520] - 1)
maxAbsoluteSignal <- max(wifiTrain[,1:520])

for (i in 1:520) {
  wifiTrain[which(wifiTrain[,i] == 100) ,i] <- minAbsoluteSignal
}

#### identify outliers ?####
#### replace all singals with above -30 with minAbsoluteSignal, wrong signal ####
for (i in 1:520) {
  wifiTrain[which(wifiTrain[,i] > -30) ,i] <- minAbsoluteSignal
}

#### remove columns with zero variance | nor necessary as columns with values below 90 removed####
which(apply(wifiTrain, 2, var) == 0)
wifiTrain <- wifiTrain[ - as.numeric(which(apply(wifiTrain, 2, var) == 0))]
ncol(wifiTrain)

#### find max value of each row, lowest value is threshold for next step ####
wap_1 <- wifiTrain[,1:(ncol(wifiTrain)-9)]

rowMax <- function(vector){
  apply(vector,1 , max, na.rm = T)
}

rowMax(wap_1[1:nrow(wap_1),])
min(rowMax(wap_1[1:nrow(wap_1),]))
which(rowMax(wap_1[1:nrow(wap_1),]) < -96)
# -96 new threshold: only rows with only zeros get deleted

#### find columns with only very week signals (<-100) ####
colMax <- function(vector){
  sapply(vector, max, na.rm = T)
}

colMax(wifiTrain[,1:(ncol(wifiTrain)-9)])
wifiTrain <- wifiTrain[,- which(colMax(wifiTrain[,1:(ncol(wifiTrain)-9)]) < -96)]

#### normalize dataframe from 1:520 ####
# normalize function
normalize <- function(x, xmax = max(wifiTrain[,1:(ncol(wifiTrain)-9)]), xmin = minAbsoluteSignal){
  xnew = ((x - xmin)/(xmax - xmin))
}
# apply function to all WAP and replace
wifiTrain[,1:(ncol(wifiTrain)-9)] <- lapply(wifiTrain[,1:(ncol(wifiTrain)-9)], FUN = normalize)


#### remoce duplicate rows ####
# wifiTrain[!duplicated(wifiTrain),]
# no duplicates!

#### remove rows with only zeros ####
wap_df <- wifiTrain[,1:(ncol(wifiTrain)-9)]
wap_df[which(rowSums (wap_df, na.rm = FALSE, dims = 1) == 0),]
wifiTrain <- wifiTrain[-which(rowSums (wap_df, na.rm = FALSE, dims = 1) == 0),]

#### distribution of signal strenght ####
distribution_vector <- c(as.matrix(wifiTrain[,1:(ncol(wifiTrain) - 9)]))
ggplot() + geom_histogram(aes(x = distribution_vector), bins = 40) + ggtitle("distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 40000)



#####################
#### sample data ####
#####################
set.seed(123)
# build2 <- wifiTrain[which(wifiTrain$BUILDINGID == "2"),]
# build1 <- wifiTrain[which(wifiTrain$BUILDINGID == "1"),]
# build0 <- wifiTrain[which(wifiTrain$BUILDINGID == "0"),]

sample1_building_prediction <- sample_n(wifiTrain, 3000)
sample1_building_prediction <- sample1_building_prediction[,-c((ncol(sample1_building_prediction)-8):(ncol(sample1_building_prediction)-6),(ncol(sample1_building_prediction)-4):ncol(sample1_building_prediction))]
smp1_rows <- sample.int(n = nrow(sample1_building_prediction), size = floor(.75 * nrow(sample1_building_prediction)), replace = F)

s1_train <- sample1_building_prediction[smp1_rows,]
s1_test <- sample1_building_prediction[-smp1_rows,]

train_control <- trainControl(method ="cv",
                              number = 10)


kNN_s1 <- train(BUILDINGID ~.,
                data = s1_train,
                method = "knn",
                trControl = train_control)

testPrediction_kNN_s1 <- predict(kNN_s1, newdata = s1_test)

postResample(testPrediction_kNN_s1, s1_test$BUILDINGID)

# confusion Matrix
confusionMatrix(testPrediction_kNN_s1, s1_test$BUILDINGID)

which(s1_test$BUILDINGID != testPrediction_kNN_s1)
s1_test[which(s1_test$BUILDINGID != testPrediction_kNN_s1),]
#s1_test[57,]

#### add predicted Building to dataframe ####

#### predict floor ####

#### add floor ####

#### predict long/lat ####
