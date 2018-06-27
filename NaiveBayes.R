rm(list=ls())
library(needs)
scriptName <-"NaiveBayes"
params <- list("x"=23,"y"=69,"AccThreshold"=200)
needs(data.table,h2o,parallel)
train <- fread("/media/3TB/kaggle/fb/data/train.csv",integer64="character")
train <- train[,
               .(row_id,
                 x,
                 y,
                 accuracy=as.numeric(accuracy),
                 quarter_period_of_day = as.numeric(floor((time + 120) / (6*60)) %% 4),
                 hour = as.numeric(floor(time/60) %% 24),
                 dayOfWeek = as.numeric(floor(time/60/24) %% 7),
                 monthOfYear=as.numeric(floor(time/60/24/30) %% 12),
                 place_id),
               ]
#for ~500 shops per tile, we set a square of side 10/23
train[train$accuracy>=params[["AccThreshold"]],]$accuracy <-params[["AccThreshold"]]
train$XCut <- cut(train$x,seq(0,10.01,(10/params[["x"]])),include.lowest = T)
train$YCut <- cut(train$y,seq(0,10.01,(10/params[["y"]])),include.lowest = T)
train$Grid <- paste(train$XCut,train$YCut)
#train$place_id <- as.factor(train$place_id)
featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
setAccuracyThreshold <- function(x){
  accuracyThreshold<-as.integer(quantile(x$accuracy,probs=c(0.95)))
  print(length(unique(x$Grid)))
  t <- x$accuracy
  t[t>=accuracyThreshold] <- accuracyThreshold
  return(t)
}
test <- fread("/media/3TB/kaggle/fb/data/test.csv",integer64="character")
test <- test[,
               .(row_id,
                 x,
                 y,
                 accuracy=as.numeric(accuracy),
                 quarter_period_of_day = as.numeric(floor((time + 120) / (6*60)) %% 4),
                 hour = as.numeric(floor(time/60) %% 24),
                 dayOfWeek = as.numeric(floor(time/60/24) %% 7),
                 monthOfYear=as.numeric(floor(time/60/24/30) %% 12)
                 ),
               ]
test$XCut <- cut(test$x,seq(0,10.01,(10/params[["x"]])),include.lowest = T)
test$YCut <- cut(test$y,seq(0,10.01,(10/params[["y"]])),include.lowest = T)
test$Grid <- paste(test$XCut,test$YCut)
test[test$accuracy>=params[["AccThreshold"]],]$accuracy <-params[["AccThreshold"]] #~95th percentile. calling the function takes a long time..this shoudl speed things up
getTop3Predictions <- function(row,cnames){
  x <- as.numeric(row)
  x<- x[2:length(row)]
  pids <- cnames[(which(x %in% sort(x,decreasing=T)[1:3])+1)]
  pids <- (gsub("X|p","",pids))
  return(pids)
}
submissionList <- list()
getTop3Predictions <- function(row,cnames){
  x <- as.numeric(row)
  x<- x[2:length(row)]
  pids <- cnames[(which(x %in% sort(x,decreasing=T)[1:3])+1)]
  pids <- (gsub("X|p","",pids))
  return(pids)
}
local <- h2o.init(nthreads=-1,max_mem_size = "50G")
uniqueGridIds <- unique(train$Grid)
gridTrainAndTest <- function(indexOfUniqueGrid){
  print(paste(indexOfUniqueGrid,">>",uniqueGridIds[indexOfUniqueGrid]))
  teWithRowID <- test[test$Grid==uniqueGridIds[indexOfUniqueGrid],c("row_id",featureNames),with=F]
  te <- teWithRowID[,featureNames,with=F]
  tr <- train[train$Grid==uniqueGridIds[indexOfUniqueGrid],c(featureNames,"place_id"),with=F]
  cts <- tr[,list(N=.N),by=place_id]
  cutoff <- quantile(cts$N,0.85)
  pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
  tr <- tr[place_id %in% pidsAfterCutOff$place_id]
  tr$place_id <- as.factor(tr$place_id)
  trH2o <- as.h2o(tr)
  model <- h2o.naiveBayes(x=featureNames,y="place_id",training_frame=trH2o)
  teH2o <- as.h2o(te)
  prediction <- h2o.predict(model,newdata=teH2o)
  predictionDF <- as.data.frame(prediction)
  
  predictionPlaceIds <- apply(predictionDF,1,getTop3Predictions,colnames(predictionDF))
  predictionPlaceIds <- t(predictionPlaceIds)
  tmpSub <- data.table(row_id=teWithRowID$row_id,place_id="")
  tmpSub<- cbind(tmpSub,predictionPlaceIds)
  tmpSub$place_id <- paste(tmpSub$V1,tmpSub$V2,tmpSub$V3)
  return(tmpSub[,c("row_id","place_id"),with=F])
}
submissionList <- lapply(seq_along(uniqueGridIds),gridTrainAndTest)
# for(i in unique(test$Grid)){
#   te <- test[test$Grid==i,featureNames,with=F]
#   tr <- train[train$Grid==i,c(featureNames,"place_id"),with=F]
#   cts <- tr[,list(N=.N),by=place_id]
#   cutoff <- quantile(cts$N,0.85)
#   pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
#   tr <- tr[place_id %in% pidsAfterCutOff$place_id]
#   tr$place_id <- as.factor(tr$place_id)
#   trH2o <- as.h2o(tr)
#   model <- h2o.naiveBayes(x=featureNames,y="place_id",training_frame=trH2o)
#   teH2o <- as.h2o(te)
#   prediction <- h2o.predict(model,newdata=teH2o)
#   predictionDF <- as.data.frame(prediction)
# 
#   predictionPlaceIds <- apply(predictionDF,1,getTop3Predictions,colnames(predictionDF))
#   predictionPlaceIds <- t(predictionPlaceIds)
#   tmpSub <- data.table(row_id=test[test$Grid==i,]$row_id,place_id="")
#   tmpSub<- cbind(tmpSub,predictionPlaceIds)
#   tmpSub$place_id <- paste(tmpSub$V1,tmpSub$V2,tmpSub$V3)
#   submissionList[[i]] <- tmpSub[,c("row_id","place_id"),with=F]
# }
submission <-rbindlist(submissionList)
write.table(submission,file=paste("/media/3TB/kaggle/fb/results/",scriptName,paste0(names(params),params,collapse = "_"),".csv",sep=""),row.names=F,sep=",",quote=F)