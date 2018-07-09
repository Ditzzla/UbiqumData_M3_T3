
wifidata <- as.data.frame(read.csv("UJIndoorLoc/trainingData.csv", sep = ",", stringsAsFactors = FALSE))

wifidata$BUILDINGFLOOR <- paste(
  wifidata$BUILDINGID, wifidata$FLOOR, sep = "-")

wifidata$BUILDINGFLOOR <- as.factor(wifidata$BUILDINGFLOOR)

set.seed(28)
sample1_wifipred <- sample_n(wifidata, 5000)
sample1_rows <- sample.int(n = nrow(sample1_wifipred), 
                           size = floor(.75 * nrow(sample1_wifipred)), 
                           replace = F)

s1_train <- sample1_wifipred[sample1_rows,]
s1_test <- sample1_wifipred[-sample1_rows,]

ctrl1<- trainControl( 
  method = "repeatedcv", 
  number = 10,
  repeats = 3
)

KNNFit1 <- train(
  
  BUILDINGFLOOR ~ ., # Predictor data object
  data = s1_train, # Outcome data object
  method = "knn", # Type of model 
  tuneLength = 3,
  trControl = ctrl1 # applies our training controls
  
)

KNNProd1 <- predict(KNNFit1, newdata = s1_test)

postResample(KNNProd1, s1_test$BUILDINGFLOOR)

