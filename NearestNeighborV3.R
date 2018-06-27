rm(list=ls())
library(needs)
scriptName <-"NearestNeighbor_V3"
params <- list("x"=23,"y"=69,"AccThreshold"=200)
needs(RANN,data.table,Hmisc,scales,parallel)
load("/media/3TB/kaggle/fb/data/NNV2.rdata")
#train$x <- 2*train$x
#train$y <- 3*train$y
#test$x <- 2*test$x
#test$y <- 3*test$y
setkey(train,Grid)
setkey(test,Grid)
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
  tr$x <- 2*tr$x
  tr$y <- 3*tr$y
  te$x <- 2*te$x
  te$y <- 3*te$y
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