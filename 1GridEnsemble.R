rm(list=ls())
library(needs,h2o,h2oEnsemble)
needs(data.table,RANN,caret,scales,h2o)
load("/media/3TB/kaggle/fb/data/train.rdata")
load("/media/3TB/kaggle/fb/data/test.rdata")
grid1<- train[train$Grid==train[3,]$Grid,]
trainPartition <- createDataPartition(grid1$place_id,p=0.75)
trainData <- grid1[unlist(trainPartition),]
validData <- grid1[-unlist(trainPartition),]
#preprocess trainData
accuracyThreshold<-as.integer(quantile(trainData$accuracy,probs=c(0.95)))
#trainData$ThresholdedAccuracy <- trainData$accuracy
trainData[trainData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
validData[validData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
cts <- trainData[,list(N=.N),by=place_id]
cutoff <- quantile(cts$N,0.9)
pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
gridDataTrain <- trainData[place_id %in% pidsAfterCutOff$place_id]
gridDataTrain <- gridDataTrain[,c(featureNames,"place_id"),with=F]
gridDataTrain$place_id <- as.factor(gridDataTrain$place_id)
gridDataValidation <- validData[,c(featureNames,"place_id"),with=F]
local <- h2o.init(nthreads=-1,max_mem_size = "50G")
trainingH2O <- as.h2o(gridDataTrain)
validationH2O <- as.h2o(gridDataValidation)
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper","h2o.naivebayes.wrapper")
metalearner <- "h2o.deeplearning.wrapper"


# Train the ensemble using 5-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
fit <- h2o.ensemble(x = featureNames, y = "place_id", 
                    training_frame = trainingH2O, 
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 2, shuffle = TRUE))
perf <- h2o.ensemble_performance(fit, newdata = validationH2O)
