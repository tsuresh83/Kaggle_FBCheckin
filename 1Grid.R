
rm(list=ls())
library(needs)
needs(data.table,RANN,caret,scales,h2o)
load("/media/3TB/kaggle/fb/data/train.rdata")
load("/media/3TB/kaggle/fb/data/test.rdata")
method <- "nearestneighbor"
if(method=="nearestneighbor"){ #~70 % accuracy
  grid1<- train[train$Grid==train[3,]$Grid,]
  trainPartition <- createDataPartition(grid1$place_id,p=0.75)
  trainData <- grid1[unlist(trainPartition),]
  validData <- grid1[-unlist(trainPartition),]
  #preprocess trainData
  accuracyThreshold<-as.integer(quantile(trainData$accuracy,probs=c(0.95)))
  #trainData$ThresholdedAccuracy <- trainData$accuracy
  trainData[trainData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
  cts <- trainData[,list(N=.N),by=place_id]
  top5 <- cts[cts[,with(cts,order(-N))],][1:5,]
  #cutoff <- quantile(cts$N,0.5)
#   pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
#   gridData <- trainData[place_id %in% pidsAfterCutOff$place_id]
  gridData <- trainData[,featureNames,with=F]
  for(j in featureNames) set(gridData,j=j,value=rescale(gridData[[j]],to=c(0,10)))
  gridData$x <- 2*gridData$x
  gridData$y <- 3*gridData$y
  #preprocess validData
  #validData$ThresholdedAccuracy <- validData$accuracy
  validData[validData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  gridDataValidation <- validData[,featureNames,with=F]
  for(j in featureNames) set(gridDataValidation,j=j,value=rescale(gridDataValidation[[j]],to=c(0,10)))
  gridDataValidation$x <- 2*gridDataValidation$x
  gridDataValidation$y <- 3*gridDataValidation$y
  nearestNeighbor <- nn2(gridData,gridDataValidation,k=50)
  nearestNeighborIds <-nearestNeighbor$nn.idx
  #closest3NeighborsIds <- nearestNeighborIds[,1:3]
  getPlaceId <- function(index){
    return(trainData[index,]$place_id)
  }
  #cl <- makeCluster(12)
  #closest3Neighbors <- apply(closest3NeighborsIds[1:6,],c(1,2),getPlaceId)
  closestNeighbors <- as.data.table(apply(nearestNeighborIds,2,getPlaceId))
  getFrequent3 <- function(x){
    #print(x)
    return(names(as.list(rev(sort(table(x)))))[1:3])
  }
  getTop3 <- function(x){
    #print(x)
    frequent3 <- getFrequent3(x)
    return(union(intersect(frequent3,unique(x)[1:3]),frequent3)[1:3])
  }
  getNearest3 <- function(x){
    return(unique(x)[1:3])
  }
  closest3Neighbors <- as.data.table(t(apply(closestNeighbors,1,getNearest3)))
  results <- vector()
  for(i in 1:nrow(closest3Neighbors)){
    results[i] <- validData[i,]$place_id %in% closest3Neighbors[i,]
  }
  print(length(results[results==T])/length(results))
}
if(method=="bayesian"){
  grid1<- train[train$Grid==train[3,]$Grid,]
  trainPartition <- createDataPartition(grid1$place_id,p=0.75)
  trainData <- grid1[unlist(trainPartition),]
  validData <- grid1[-unlist(trainPartition),]
  #preprocess trainData
  accuracyThreshold<-as.integer(quantile(trainData$accuracy,probs=c(0.95)))
  #trainData$ThresholdedAccuracy <- trainData$accuracy
  trainData[trainData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
  gridData <- trainData[,c(featureNames,"place_id"),with=F]
  cts <- gridData[,list(N=.N),by=place_id]
  cutoff <- quantile(cts$N,0.85)
  pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
  gridData <- gridData[place_id %in% pidsAfterCutOff$place_id]
  gridData$place_id <- as.factor(gridData$place_id)
  
  #for(j in featureNames) set(gridData,j=j,value=rescale(gridData[[j]],to=c(0,10)))
  local <- h2o.init()
  gridDataH2o<- as.h2o(gridData)
  #bayesModel <- naiveBayes(place_id~.,data=gridData)
  #preprocess validData
  #validData$ThresholdedAccuracy <- validData$accuracy
  validData[validData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  gridDataValidation <- validData[,c(featureNames,"place_id"),with=F]
  #for(j in featureNames) set(gridDataValidation,j=j,value=rescale(gridDataValidation[[j]],to=c(0,10)))
  gridDataValidationH2o<-as.h2o(gridDataValidation)
  model <- h2o.naiveBayes(x=featureNames,y="place_id",training_frame=gridDataH2o,compute_metrics = F)
  prediction <- h2o.predict(model,newdata=gridDataValidationH2o)
  predictionDF <- as.data.frame(prediction)
  getTop3Predictions <- function(row,cnames){
    x <- as.numeric(row)
    x<- x[2:length(row)]
    pids <- cnames[(which(x %in% sort(x,decreasing=T)[1:3])+1)]
    pids <- (gsub("X|p","",pids))
    return(pids)
  }
  predictionPlaceIds <- apply(predictionDF,1,getTop3Predictions,colnames(predictionDF))
  predictionPlaceIds <- t(predictionPlaceIds)
  #p <- predict(bayesModel,gridDataValidation,type="raw")
  resultsBayes <- vector()
  for(i in 1:nrow(predictionPlaceIds)){
    resultsBayes[i] <- gridDataValidation[i,]$place_id %in% predictionPlaceIds[i,]
  }
  print(length(resultsBayes[resultsBayes==T])/length(resultsBayes))
}
if(method=="gbm"){
  grid1<- train[train$Grid%in%train[3,]$Grid,]
  trainPartition <- createDataPartition(grid1$place_id,p=0.75)
  trainData <- grid1[unlist(trainPartition),]
  validData <- grid1[-unlist(trainPartition),]
  #preprocess trainData
  accuracyThreshold<-as.integer(quantile(trainData$accuracy,probs=c(0.95)))
  #trainData$ThresholdedAccuracy <- trainData$accuracy
  trainData[trainData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
  gridData <- trainData[,c(featureNames,"place_id"),with=F]
  cts <- gridData[,list(N=.N),by=place_id]
  cutoff <- quantile(cts$N,0.9)
  pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
  gridData <- gridData[place_id %in% pidsAfterCutOff$place_id]
  gridData$place_id <- as.factor(gridData$place_id)
  
  #for(j in featureNames) set(gridData,j=j,value=rescale(gridData[[j]],to=c(0,10)))
  local <- h2o.init()
  gridDataH2o<- as.h2o(gridData)
  #bayesModel <- naiveBayes(place_id~.,data=gridData)
  #preprocess validData
  #validData$ThresholdedAccuracy <- validData$accuracy
  validData[validData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  gridDataValidation <- validData[,c(featureNames,"place_id"),with=F]
  #for(j in featureNames) set(gridDataValidation,j=j,value=rescale(gridDataValidation[[j]],to=c(0,10)))
  gridDataValidationH2o<-as.h2o(gridDataValidation)
  model <- h2o.gbm(x=featureNames,y="place_id",training_frame=gridDataH2o)
  predictionGBM <- h2o.predict(model,newdata=gridDataValidationH2o)
  predictionDFGBM <- as.data.frame(predictionGBM)
  getTop3Predictions <- function(row,cnames){
    x <- as.numeric(row)
    x<- x[2:length(row)]
    pids <- cnames[(which(x %in% sort(x,decreasing=T)[1:3])+1)]
    pids <- ((gsub("X|p","",(pids))))[1:3]
    return(pids)
  }
  gbmpredictionPlaceIds <- apply(predictionDFGBM,1,getTop3Predictions,colnames(predictionDFGBM))
  gbmpredictionPlaceIds <- as.matrix(t(gbmpredictionPlaceIds))
  #p <- predict(bayesModel,gridDataValidation,type="raw")
  resultsGBM <- vector()
  for(i in 1:nrow(gbmpredictionPlaceIds)){
    resultsGBM[i] <- gridDataValidation[i,]$place_id %in% gbmpredictionPlaceIds[i,]
  }
  print(length(resultsGBM[resultsGBM==T])/length(resultsGBM))
}
if(method=="rf"){
  grid1<- train[train$Grid==train[3,]$Grid,]
  trainPartition <- createDataPartition(grid1$place_id,p=0.75)
  trainData <- grid1[unlist(trainPartition),]
  validData <- grid1[-unlist(trainPartition),]
  #preprocess trainData
  accuracyThreshold<-as.integer(quantile(trainData$accuracy,probs=c(0.95)))
  #trainData$ThresholdedAccuracy <- trainData$accuracy
  trainData[trainData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
  gridData <- trainData[,c(featureNames,"place_id"),with=F]
  cts <- gridData[,list(N=.N),by=place_id]
  cutoff <- quantile(cts$N,0.9)
  pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
  gridData <- gridData[place_id %in% pidsAfterCutOff$place_id]
  gridData$place_id <- as.factor(gridData$place_id)
  
  #for(j in featureNames) set(gridData,j=j,value=rescale(gridData[[j]],to=c(0,10)))
  local <- h2o.init()
  gridDataH2o<- as.h2o(gridData)
  #bayesModel <- naiveBayes(place_id~.,data=gridData)
  #preprocess validData
  #validData$ThresholdedAccuracy <- validData$accuracy
  validData[validData$accuracy>=accuracyThreshold,]$accuracy <- accuracyThreshold
  gridDataValidation <- validData[,c(featureNames,"place_id"),with=F]
  #for(j in featureNames) set(gridDataValidation,j=j,value=rescale(gridDataValidation[[j]],to=c(0,10)))
  gridDataValidationH2o<-as.h2o(gridDataValidation)
  model <- h2o.randomForest(x=featureNames,y="place_id",training_frame=gridDataH2o)
  predictionRF <- h2o.predict(model,newdata=gridDataValidationH2o)
  predictionDFRF <- as.data.frame(predictionRF)
  getTop3Predictions <- function(row,cnames){
    x <- as.numeric(row)
    x<- x[2:length(row)]
    pids <- cnames[(which(x %in% sort(x,decreasing=T)[1:3])+1)]
    pids <- ((gsub("X|p","",(pids))))[1:3]
    return(pids)
  }
  rfpredictionPlaceIds <- apply(predictionDFRF,1,getTop3Predictions,colnames(predictionDFRF))
  rfpredictionPlaceIds <- as.matrix(t(rfpredictionPlaceIds))
  #p <- predict(bayesModel,gridDataValidation,type="raw")
  resultsRF <- vector()
  for(i in 1:nrow(rfpredictionPlaceIds)){
    resultsRF[i] <- gridDataValidation[i,]$place_id %in% rfpredictionPlaceIds[i,]
  }
  print(length(resultsRF[resultsRF==T])/length(resultsRF))
}
ensemble <- list()
for(i in 1:nrow(predictionPlaceIds)){
  #ensemble[[i]] <- union(intersect(predictionPlaceIds[i,],closest3Neighbors[i,]),na.exclude(union(as.character(closest3Neighbors[i,]),top5$place_id)))[1:3]
  #ensemble[[i]]<- data.frame(list(union(intersect(predictionPlaceIds[i,],closest3Neighbors[i,]),closest3Neighbors[i,])[1:3]))
  ensemble[[i]] <- union(intersect(predictionPlaceIds[i,],closest3Neighbors[i,]),predictionPlaceIds[i,])[1:3]
}
ensembleDF <- rbindlist(ensemble)
ensembleResults <- vector()
for(i in 1:nrow(predictionPlaceIds)){
  ensembleResults[i] <- validData[i,]$place_id %in% as.character(ensembleDF[i,])
}
print(length(ensembleResults[ensembleResults==T])/length(ensembleResults))