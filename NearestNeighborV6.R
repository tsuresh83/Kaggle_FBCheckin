rm(list=ls())
library(needs)
scriptName <-"NearestNeighbor_V5_WithPadding"
params <- list("x"=0.25,"y"=0.25,"AccThreshold"=200,"xpad"=0.027,"ypad"=0.015,"checkinThreshold"=0.85,"xweight"=2,"yweight"=3)
needs(RANN,data.table,Hmisc,scales,parallel,caret,doParallel,foreach)
registerDoParallel(cores=4)
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
featureNames <- c("x","y","accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
featureNamesSansLocation <- c("accuracy","quarter_period_of_day","hour","dayOfWeek","monthOfYear")
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
xCuts <- seq(0,10.5,by=params$x)
yCuts <- seq(0,10.5,by=params$y)
test$XCuts <- cut(test$x,xCuts,include.lowest = T)
test$YCuts <- cut(test$y,yCuts,include.lowest = T)
test$Group <- paste(test$XCuts,test$YCuts)
getPlaceId <- function(index,data){
  return(data[index,]$place_id)
}
getTop3 <- function(x){
  #print(x)
  return(names(as.list(rev(sort(table(x)))))[1:3])
}
getNearestNeighbors <- function(gid,testData,testXRange,testYRange){
  trainMinX <- max(0,testXRange[1]-params$xpad)
  trainMaxX <- min(10,testXRange[2]+params$xpad)
  trainMinY <- max(0,testYRange[1]-params$ypad)
  trainMaxY <- min(10,testYRange[2]+params$ypad)
  nearestNeighborData <- train[x>=trainMinX & x <=trainMaxX & y>=trainMinY & y<=trainMaxY]
  #cut off places with very few checkins
  cts <- nearestNeighborData[,list(N=.N),by=place_id]
  cutoff <- quantile(cts$N,params$checkinThreshold)
  pidsAfterCutOff <- cts[N>cutoff,"place_id",with=F]
  nearestNeighborData <- nearestNeighborData[place_id %in% pidsAfterCutOff$place_id]
  #checkin cut off ends here
  tmpList <- list()
  tmpList[["Train"]]<- nearestNeighborData[,-"place_id",with=F]
  tmpList[["Test"]]<- testData
  all <- rbindlist(tmpList,idcol = "ID")
  all[, (featureNames):=lapply(.SD, function(x) rescale(x,to=c(0,10))),  .SDcols=featureNames]
  all$x <- params$xweight*all$x
  all$y <- params$yweight*all$y
  nearestNeighbors <- nn2(all[all$ID=="Train",featureNames,with=F],query = all[all$ID=="Test",featureNames,with=F],k=50)$nn.idx
  closestNeighbors <- as.data.table(apply(nearestNeighbors,2,getPlaceId,nearestNeighborData))
  closest3Neighbors <- as.data.table(t(apply(closestNeighbors,1,getTop3)))
  tmpSub <- data.table(row_id=testData$row_id,place_id="")
  tmpSub<- cbind(tmpSub,closest3Neighbors)
  tmpSub$place_id <- paste(tmpSub$V1,tmpSub$V2,tmpSub$V3)
  return(tmpSub[,c("row_id","place_id"),with=F])
}
submissionList <- list()
totalGids <- length(unique(test$Group))
ctr<-1
for(gid in unique(test$Group)){
  print(paste(ctr,"of",totalGids))
  ctr<- ctr+1
  ss <- test[test$Group==gid,c("row_id",featureNames),with=F]
  submissionList[[gid]]<-getNearestNeighbors(gid,ss,range(ss$x),range(ss$y))
}
submission <-rbindlist(submissionList)
write.table(submission,file=paste("/media/3TB/kaggle/fb/results/",scriptName,paste0(names(params),params,collapse = "_"),"Frequent3.csv",sep=""),row.names=F,sep=",",quote=F)