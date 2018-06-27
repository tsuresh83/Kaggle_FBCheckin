rm(list=ls())
library(needs)
scriptName <-"NearestNeighbor_V4"
params <- list("x"=0.85,"y"=0.02,"AccThreshold"=200)
needs(RANN,data.table,Hmisc,scales,parallel,caret)
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
train[train$accuracy>params[["AccThreshold"]],]$accuracy <- params[["AccThreshold"]]
train$accuracy <- params[["AccThreshold"]]/train$accuracy
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
test[test$accuracy>params[["AccThreshold"]],]$accuracy <- params[["AccThreshold"]]
test$accuracy <- params[["AccThreshold"]]/test$accuracy
getPlaceId <- function(index,data){
  return(data[index,]$place_id)
}
getTop3 <- function(x){
  #print(x)
  return(names(as.list(rev(sort(table(x)))))[1:3])
}
train[, (featureNames):=lapply(.SD, function(x) rescale(x,to=c(0,10))),  .SDcols=featureNames]
test[, (featureNames):=lapply(.SD, function(x) rescale(x,to=c(0,10))),  .SDcols=featureNames]
# samples <- createDataPartition(train$y,p=0.0001)
# validation <- train[unlist(samples),]
xBuffer <- params[["x"]] #~75% sd
yBuffer <- params[["y"]] #~75% sd
submissionList <- list()
getNeighbors <- function(i){
  xRangeMin <- test[i,]$x-xBuffer
  xRangeMin <- ifelse(xRangeMin>0,xRangeMin,0)
  xRangeMax <- test[i,]$x+xBuffer
  xRangeMax <- ifelse(xRangeMax>0,xRangeMax,0)
  yRangeMin <- test[i,]$y-yBuffer
  yRangeMin <- ifelse(yRangeMin>0,yRangeMin,0)
  yRangeMax <- test[i,]$y+yBuffer
  yRangeMax <- ifelse(yRangeMax>0,yRangeMax,0)
  neighbors <- train[train$x>=xRangeMin&train$x<=xRangeMax&
                       train$y>=yRangeMin&train$y<=yRangeMax,]
  nearestNeighbor <- nn2(neighbors[,featureNames,with=F],test[i,featureNames,with=F],k=min(50,nrow(neighbors)))
  nearestNeighborIds <-nearestNeighbor$nn.idx
  closestNeighbors <- as.data.table(apply(nearestNeighborIds,2,getPlaceId,neighbors))
  closest3Neighbors <- as.data.table(t(apply(closestNeighbors,2,getTop3)))
  tmpSub <- data.table(row_id=test[i,]$row_id,place_id="")
  tmpSub<- cbind(tmpSub,closest3Neighbors)
  tmpSub$place_id <- paste(tmpSub$V1,tmpSub$V2,tmpSub$V3)
  return(tmpSub[,c("row_id","place_id"),with=F])
}
results <- mclapply(1:nrow(test),getNeighbors,mc.cores = 4)
submission <-rbindlist(submissionList)
write.table(submission,file=paste("/media/3TB/kaggle/fb/results/",scriptName,paste0(names(params),params,collapse = "_"),".csv",sep=""),row.names=F,sep=",",quote=F)
# resultsBoolean <- vector()
# for(i in 1:nrow(validation)){
#   resultsBoolean[i] <- validation[i,]$place_id %in% unlist(strsplit(results[[i]]$place_id," "))
# }
# print(length(resultsBoolean[resultsBoolean==T])/length(results))
# for(i in 1: nrow(validation)){
#   print(i)
#   xRangeMin <- validation[i,]$x-0.85
#   xRangeMin <- ifelse(xRangeMin>0,xRangeMin,0)
#   xRangeMax <- validation[i,]$x+0.85
#   xRangeMax <- ifelse(xRangeMax>0,xRangeMax,0)
#   yRangeMin <- validation[i,]$y-0.02
#   yRangeMin <- ifelse(yRangeMin>0,yRangeMin,0)
#   yRangeMax <- validation[i,]$y+0.02
#   yRangeMax <- ifelse(yRangeMax>0,yRangeMax,0)
#   neighbors <- train[train$x>=xRangeMin&train$x<=xRangeMax&
#                        train$y>=yRangeMin&train$y<=yRangeMax,]
#   nearestNeighbor <- nn2(neighbors[,featureNames,with=F],validation[i,featureNames,with=F],k=min(50,nrow(neighbors)))
#   nearestNeighborIds <-nearestNeighbor$nn.idx
#   closestNeighbors <- as.data.table(apply(nearestNeighborIds,2,getPlaceId,neighbors))
#   closest3Neighbors <- as.data.table(t(apply(closestNeighbors,2,getTop3)))
#   tmpSub <- data.table(row_id=validation[i,]$row_id,place_id="")
#   tmpSub<- cbind(tmpSub,closest3Neighbors)
#   tmpSub$place_id <- paste(tmpSub$V1,tmpSub$V2,tmpSub$V3)
#   return(tmpSub[,c("row_id","place_id"),with=F])
#   
# }
# rm(list=ls())
# library(needs)
# scriptName <-"NearestNeighbor"
# needs(RANN,data.table,Hmisc,scales,parallel)
# train <- fread("/media/3TB/kaggle/fb/data/train.csv",integer64="character")
# accuracyThreshold<-200 #95th quantile
# train$ThresholdedAccuracy <- train$accuracy
# train[train$ThresholdedAccuracy>=accuracyThreshold,]$ThresholdedAccuracy <- accuracyThreshold
# train$NormAccuracy <- accuracyThreshold/train$ThresholdedAccuracy
# setkey(train,"place_id")
# WtdLocationByPlaceId <- train[, list(UnWtdMeanX = mean(x), UnWtdMeanY=mean(y),UnWtdSDX = sd(x),UnWtdSDY = sd(y),
#                                      MeanX=wtd.mean(x,weights=NormAccuracy), MeanY=wtd.mean(y,weights=NormAccuracy),
#                                      MedianX = median(x),MedianY=median(y),
#                                      VarX=wtd.var(x,weights=NormAccuracy),VarY=wtd.var(y,weights=NormAccuracy),
#                                      MeanAccuracy=mean(NormAccuracy),MedianAccuracy=median(NormAccuracy)),by=key(train)]
