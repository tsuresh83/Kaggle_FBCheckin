rm(list=ls())
library(needs)
scriptName <-"NearestNeighbor"
needs(RANN,data.table,Hmisc,scales,parallel)
train <- fread("/media/3TB/kaggle/fb/data/train.csv",integer64="character")
accuracyThreshold<-200 #95th quantile
train$ThresholdedAccuracy <- train$accuracy
train[train$ThresholdedAccuracy>=accuracyThreshold,]$ThresholdedAccuracy <- accuracyThreshold
train$NormAccuracy <- train$ThresholdedAccuracy/accuracyThreshold
setkey(train,"place_id")
WtdLocationByPlaceId <- train[, list(MeanX=wtd.mean(x,weights=NormAccuracy), MeanY=wtd.mean(y,weights=NormAccuracy),
                                     MedianX = median(x),MedianY=median(y),
                                     VarX=wtd.var(x,weights=NormAccuracy),VarY=wtd.var(y,weights=NormAccuracy),
                                     MeanAccuracy=mean(NormAccuracy),MedianAccuracy=median(NormAccuracy)),by=key(train)]
WtdLocationByPlaceId$ScaledMeanAccuracy <-rescale(WtdLocationByPlaceId$MeanAccuracy,to=c(0,10))
WtdLocationByPlaceId$ScaledMedianAccuracy <-rescale(WtdLocationByPlaceId$MedianAccuracy,to=c(0,10))
test <- fread("/media/3TB/kaggle/fb/data/test.csv",integer64="character")
test$ThresholdedAccuracy <- test$accuracy
test[test$ThresholdedAccuracy>=accuracyThreshold,]$ThresholdedAccuracy <- accuracyThreshold
test$NormAccuracy <- test$ThresholdedAccuracy/accuracyThreshold
test$ScaledAccuracy <-rescale(test$NormAccuracy,to=c(0,10))
data = test[,c("x","y","ScaledAccuracy"),with=F]
nearestNeighbor <- nn2(WtdLocationByPlaceId[,c("MeanX","MeanY","ScaledMeanAccuracy"),with=F],test[,c("x","y","ScaledAccuracy"),with=F])
nearestNeighborIds <-nearestNeighbor$nn.idx
closest3NeighborsIds <- nearestNeighborIds[,1:3]
getPlaceId <- function(index){
  return(WtdLocationByPlaceId[index,]$place_id)
}
#cl <- makeCluster(12)
#closest3Neighbors <- apply(closest3NeighborsIds[1:6,],c(1,2),getPlaceId)
closest3Neighbors <- (apply(closest3NeighborsIds,c(2),getPlaceId))
#stopCluster(cl)
submission <- data.table(row_id=test$row_id,place_id="")
submission<- cbind(submission,closest3Neighbors)
submission$place_id <- paste(submission$V1,submission$V2,submission$V3)
write.table(submission[,c(1,2),with=F],file=paste("/media/3TB/kaggle/fb/results/",scriptName,".csv",sep=""),row.names=F,sep=",",quote=F)