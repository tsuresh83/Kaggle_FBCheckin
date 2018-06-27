rm(list=ls())
library(needs)
scriptName <-"NearestNeighbor_V2"
params <- list("x"=23,"y"=69,"AccThreshold"=200)
needs(RANN,data.table,Hmisc,scales,parallel)
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
train$XCut <- cut(train$x,seq(0,10.01,(10/params[[x]])),include.lowest = T)
train$YCut <- cut(train$y,seq(0,10.01,(10/params[[y]])),include.lowest = T)
train$Grid <- paste(train$XCut,train$YCut)
featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
setAccuracyThreshold <- function(x){
  accuracyThreshold<-as.integer(quantile(x$accuracy,probs=c(0.95)))
  print(length(unique(x$Grid)))
  t <- x$accuracy
  t[t>=accuracyThreshold] <- accuracyThreshold
  return(t)
}
#train[, ThresholdAccuracy:=setAccuracyThreshold, by=Grid]
#train[, (accuracy):=lapply(.SD, function(x) setAccuracyThreshold(x)), by=Grid, .SDcols=c("accuracy")]
#train$ThresholdAccuracy <- train$accuracy
# rescaledDT <- data.table()
for(i in unique(train$Grid)){
  train[train$Grid==i & train$accuracy>=params[["AccThreshold"]],]$accuracy <-params[["AccThreshold"]] #~95th percentile. calling the function takes a long time..this shoudl speed things up
  #for(j in featureNames) set(train[train$Grid==i,],j=j,value=rescale(train[train$Grid==i,][[j]],to=c(0,10)))
  train[train$Grid==i, (featureNames):=lapply(.SD, function(x) rescale(x,to=c(0,10))), by=Grid, .SDcols=featureNames]
}
#consider replacing the above with something like....
# nm1 <- grep("^value", colnames(data), value=TRUE)
# nm2 <- paste("lag", nm1, sep=".")
# data[, (nm2):=lapply(.SD, function(x) c(NA, x[-.N])), by=groups, .SDcols=nm1]
#nShopsPerGrid <- train[,list(uniquePlaceIds=length(unique(place_id))), by=list(Grid)]
#nRowsPerGrid <- train[,.N, by=list(Grid)]
#thresholdPerGrid <- train[,.list(AccuracyThreshold = quantile(accuracy,probs = c(0.95))),by="Grid"]
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
for(i in unique(test$Grid)){
  test[test$Grid==i & test$accuracy>=params[["AccThreshold"]],]$accuracy <-params[["AccThreshold"]] #~95th percentile. calling the function takes a long time..this shoudl speed things up
  #for(j in featureNames) set(train[train$Grid==i,],j=j,value=rescale(train[train$Grid==i,][[j]],to=c(0,10)))
  test[test$Grid==i, (featureNames):=lapply(.SD, function(x) rescale(x,to=c(0,10))), by=Grid, .SDcols=featureNames]
}
getPlaceId <- function(index,data){
  return(data[index,]$place_id)
}
getTop3 <- function(x){
  #print(x)
  return(names(as.list(rev(sort(table(x)))))[1:3])
}
submissionList <- list()
for(i in unique(test$Grid)){
  te <- test[test$Grid==i,featureNames,with=F]
  tr <- train[train$Grid==i,featureNames,with=F]
  nearestNeighbor <- nn2(tr,te,k=50)
  nearestNeighborIds <-nearestNeighbor$nn.idx
  closestNeighbors <- as.data.table(apply(nearestNeighborIds,2,getPlaceId,train[train$Grid==i,]))
  closest3Neighbors <- as.data.table(t(apply(closestNeighbors,1,getTop3)))
  tmpSub <- data.table(row_id=test[test$Grid==i,]$row_id,place_id="")
  tmpSub<- cbind(tmpSub,closest3Neighbors)
  tmpSub$place_id <- paste(tmpSub$V1,tmpSub$V2,tmpSub$V3)
  submissionList[[i]] <- tmpSub[,c("row_id","place_id"),with=F]
}
submission <-rbindlist(submissionList)
write.table(submission,file=paste("/media/3TB/kaggle/fb/results/",scriptName,paste0(names(params),params,collapse = "_"),".csv",sep=""),row.names=F,sep=",",quote=F)