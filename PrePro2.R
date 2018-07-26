setwd("/Users/Andi/Desktop/DataCourse/3 - Deep Analystics and Data visualization/3_ Evaluate Techniques for Wifi Locating/M3T3")

library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("forecast")
library("caret")
library("ggthemes")

wifiTrain <- as.data.frame(read.csv("UJIndoorLoc/trainingData.csv", sep = ",", stringsAsFactors = FALSE))
wifiVeri <- as.data.frame(read.csv("UJIndoorLoc/validationData.csv", sep = ",", stringsAsFactors = FALSE))

###################################################
#### initial data investigation ##################
###################################################
# nrow(wifiTrain)
# ncol(wifiTrain)

###################################################
#### change data type ####
###################################################
str(wifiTrain[,(ncol(wifiTrain)-8):ncol(wifiTrain)])
wifiTrain$LONGITUDE <- as.integer(wifiTrain$LONGITUDE)
wifiTrain$LATITUDE <- as.integer(wifiTrain$LATITUDE)
wifiTrain$FLOOR <- as.factor(wifiTrain$FLOOR)
wifiTrain$BUILDINGID <- as.factor(wifiTrain$BUILDINGID)
wifiTrain$SPACEID <- as.factor(wifiTrain$SPACEID)
wifiTrain$RELATIVEPOSITION <- as.factor(wifiTrain$RELATIVEPOSITION)
wifiTrain$USERID <- as.factor(wifiTrain$USERID)
wifiTrain$PHONEID <- as.factor(wifiTrain$PHONEID)
str(wifiTrain[,(ncol(wifiTrain)-8):ncol(wifiTrain)])

str(wifiVeri[,(ncol(wifiVeri)-8):ncol(wifiVeri)])
wifiVeri$LONGITUDE <- as.integer(wifiVeri$LONGITUDE)
wifiVeri$LATITUDE <- as.integer(wifiVeri$LATITUDE)
wifiVeri$FLOOR <- as.factor(wifiVeri$FLOOR)
wifiVeri$BUILDINGID <- as.factor(wifiVeri$BUILDINGID)
wifiVeri$SPACEID <- as.factor(wifiVeri$SPACEID)
wifiVeri$RELATIVEPOSITION <- as.factor(wifiVeri$RELATIVEPOSITION)
wifiVeri$USERID <- as.factor(wifiVeri$USERID)
wifiVeri$PHONEID <- as.factor(wifiVeri$PHONEID)
str(wifiVeri[,(ncol(wifiVeri)-8):ncol(wifiVeri)])


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
#### replace 100s with smallest value: -105 ####
minAbsoluteSignal <- min(wifiTrain[,1:520] - 1)
maxAbsoluteSignal <- max(wifiTrain[,1:520])


for (i in 1:520) {
  wifiTrain[which(wifiTrain[,i] == 100) ,i] <- minAbsoluteSignal
}

for (i in 1:520) {
  wifiVeri[which(wifiVeri[,i] == 100) ,i] <- minAbsoluteSignal
}

###################################################
#### replace all singals with above -30 with minAbsoluteSignal, wrong signal ####
###################################################
for (i in 1:520) {
  wifiTrain[which(wifiTrain[,i] > -30) ,i] <- minAbsoluteSignal
}

for (i in 1:520) {
  wifiVeri[which(wifiVeri[,i] > -30) ,i] <- minAbsoluteSignal
}

###################################################
#### remoce duplicate rows ####
###################################################
nrow(wifiTrain)
wifiTrain <- unique(wifiTrain)
nrow(wifiTrain)
# 637 duplicates

wifiVeri <- unique(wifiVeri)
# no duplicates

###################################################
#### remove user 6 observations ####
###################################################
nrow(wifiTrain)
if(length(which(wifiTrain$USERID == 6)) > 0){
  wifiTrain <- wifiTrain[-which(wifiTrain$USERID == 6),]
}
nrow(wifiTrain)

# only user 0 in verification

###################################################
#### find max value of each row, lowest value is threshold for next step ####
###################################################
wap_1 <- wifiTrain[,1:(ncol(wifiTrain)-9)]

rowMax <- function(vector){
  apply(vector,1 , max, na.rm = T)
}

rowMax(wap_1[1:nrow(wap_1),])
min(rowMax(wap_1[1:nrow(wap_1),]))
length(which(rowMax(wap_1[1:nrow(wap_1),]) < -96))
# -96 new threshold: only rows with only zeros get deleted


###################################################
#### find columns with only very week signals (<-100) ####
###################################################
colMax <- function(vector){
  sapply(vector, max, na.rm = T)
}

colMax(wifiTrain[,1:(ncol(wifiTrain)-9)])
wifiTrain <- wifiTrain[,- which(colMax(wifiTrain[,1:(ncol(wifiTrain)-9)]) < -96)]

###################################################
#### remove columns with zero variance | nor necessary as columns with values below 90 removed####
###################################################
which(apply(wifiTrain, 2, var) == 0)
if (length(which(apply(wifiTrain, 2, var) == 0)) > 0){
wifiTrain <- wifiTrain[, -as.numeric(which(apply(wifiTrain, 2, var) == 0))]
}
ncol(wifiTrain)

################################################
#### remove columns of verification dataset ####
################################################
wifiVeri <- wifiVeri[, which(names(wifiVeri) %in% names(wifiTrain))]


###################################################
#### distribution of signal strenght wifiTrain ####
###################################################
distribution_vector <- c(as.matrix(wifiTrain[,1:(ncol(wifiTrain) - 9)]))
ggplot() + geom_histogram(aes(x = distribution_vector), bins = 40) + ggtitle("distribution") + 
theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 40000)

###################################################
#### distribution of signal strenght wifiVeri ####
###################################################
distribution_vector2 <- c(as.matrix(wifiVeri[,1:(ncol(wifiVeri) - 9)]))
ggplot() + geom_histogram(aes(x = distribution_vector2), bins = 40) + ggtitle("distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 1500)

###################################################
#### normalize dataframe by row ####
###################################################
wifiTrain[,1:(ncol(wifiTrain)-9)] <- as.data.frame(t(apply(wifiTrain[1:(ncol(wifiTrain)-9)], 1, function(x) (x - min(x))/(max(x)-min(x)))))

wifiVeri[,1:(ncol(wifiVeri)-9)] <- as.data.frame(t(apply(wifiVeri[1:(ncol(wifiVeri)-9)], 1, function(x) (x - min(x))/(max(x)-min(x)))))

###################################################
#### distribution of signal strenght ####
###################################################
distribution_vector <- c(as.matrix(wifiTrain[,1:(ncol(wifiTrain) - 9)]))
ggplot() + geom_histogram(aes(x = distribution_vector), bins = 40) + ggtitle("distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 40000)

###################################################
#### distribution of signal strenght wifiVeri ####
###################################################
distribution_vector2 <- c(as.matrix(wifiVeri[,1:(ncol(wifiVeri) - 9)]))
ggplot() + geom_histogram(aes(x = distribution_vector2), bins = 40) + ggtitle("distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 2000)


