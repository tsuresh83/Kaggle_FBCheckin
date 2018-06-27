rm(list=ls())
library(SDMTools)
library(tiff)
library(raster)
library(ggplot2)
library(RImageBook)
library(EBImage)
library(plyr)
library(ggplot2)
#############
#variables and colunmn names that contain 'window' correspond to the term 'tile' in the manuscript
#window / tile sizes (square side) of interest in pixels.  Ratios will be calculated at each of the specified window sizes.
#wsize<-c(50,100,200,300,400,500,1000)
wsize<-c(200)
#switch to calculate ratios by area or count of domains
ratioByArea <-T
#denotes the ratio of signal detection output resolution to tissue classification output resolution.
#2 means signal detection output has 2 times the resolution of the tissue classification output.
#Change this appropriately.  If both tissue classification and signal detection outputs are at the
#same resolution set this to 1
signalDetectionScalingFactor = 1 
#the source stain whose segmentation led to the signal images in the analysis
signalStain <-'CASP3'
noOfTilesPerDraw <- c(100)
#noOfRepeats <- c(1,3,10,20,50,100,200,400,1000)
noOfRepeats <-c(10)
#a parameter that sets the number of repeats of a sample proportional to the percentage area coverage of the grid area.
jumps <- 10
# directory to save output to
outputDir <- '~/'
outputDir <- paste(outputDir,signalStain,sep="/")
#create outputDir recursively
dir.create(outputDir,recursive = T)
ratiosByAreaDF <- data.frame(Window=numeric(),Ratio=character(),Value=numeric(),ClassLabel=character(),SignalLabel=character(),ClassArea=numeric(),TreatmentArm=character(),ClassFile=character(),SignalFile=character())
ratiosByCountDF <- data.frame(Window=numeric(),Ratio=character(),Value=numeric(),ClassLabel=character(),SignalLabel=character(),ClassArea=numeric(),TreatmentArm=character(),ClassFile=character(),SignalFile=character())
wilcoxTestResults <- data.frame()
##################################################################
# Calculate ratios and summarize in a data frame
##################################################################
#function definitions begin
calculateRatiosByArea <- function(classImage, signalImage, classLabel,treatmentArm, classFile,signalFile){
  classImageName <- classImage
  signalImageName <- signalImage
  classImage <- readImage(classImage)
  classImage <-(imageData(classImage))
  print("class image loaded")
  signalImage <- readImage(signalImage)
  print("signal file loaded")
  signalImage <- (imageData(signalImage))
  print(paste(classImageName,signalImageName,"loaded"))
  for (ws in wsize) {
    sr<-as.matrix(aggregate(raster(classImage),fact=round(ws/signalDetectionScalingFactor),fun=sum))
    ss<-as.matrix(aggregate(raster(signalImage),fact=ws,fun=sum))
    ratios <- (ss/(((signalDetectionScalingFactor)^2)*sr))
    ratios[ratios=='Inf']<-(-1)
    ratios[ratios=='NaN']<-(-2)
    ratios <- as.vector(ratios)
    #rbind to the global variable ratiosByAreaDF
    ratiosByAreaDF <<- rbind(ratiosByAreaDF,data.frame(Window=ws,Ratio=paste(signalStain,classLabel,":",classLabel),Value=ratios,ClassArea = (signalDetectionScalingFactor)^2,ClassLabel=classLabel,SignalLabel=signalStain,ClassArea=as.vector(sr),TreatmentArm=treatmentArm,ClassFile=classFile,SignalFile=signalFile))
  }
}
calculateRatiosByCount <- function(classImage, signalImage, classLabel,treatmentArm, classFile,signalFile){
  classImage <- readImage(classImage)
  classImage <- imageData(classImage)
  signalImage <- readImage(signalImage)
  cellNo <- imageData(bwlabel(signalImage))
  for (ws in wsize) {
    sr<-as.matrix(aggregate(raster(classImage),fact=round(ws/signalDetectionScalingFactor),fun=sum))
    ss<-as.matrix(aggregate(raster(cellNo),fact=ws,fun=countCells))
    ratios<-(ss/(((signalDetectionScalingFactor)^2)*sr))
    ratios[ratios=='Inf']<-(-1)
    ratios[ratios=='NaN']<-(-2)
    ratios <- as.vector(ratios)
    #rbind to the global variable ratiosByCountDF
    ratiosByCountDF <<- rbind(ratiosByCountDF,data.frame(Window=ws,Ratio=paste(signalStain,classLabel,":",classLabel),Value=ratios,ClassLabel=classLabel,SignalLabel=signalStain,ClassArea=as.vector(sr),TreatmentArm=treatmentArm,ClassFile=classFile,SignalFile=signalFile))
  }
}
calculateRatiosByCountByCenterOfMass <- function(classImage, signalImage, classLabel,treatmentArm, classFile,signalFile){
  classImage <- readImage(classImage)
  classImage <- imageData(classImage)
  signalImage <- readImage(signalImage)
  cellNo <- (imageData(bwlabel(signalImage)))
  moments <- as.data.frame(computeFeatures.moment(cellNo,cellNo))
  cx <- as.integer(round(moments$m.cx))
  cy <- as.integer(round(moments$m.cy))
  for(i in 1:length(cx)){
    cellNo[cx[i],cy[i]] <- (-1)
  }
  cellNo[cellNo!=-1] <-0
  for (ws in wsize) {
    sr<-as.matrix(aggregate(raster(classImage),fact=round(ws/signalDetectionScalingFactor),fun=sum))
    ss<-as.matrix(aggregate(raster(cellNo),fact=ws,fun=sum))
    ss<-abs(ss)
    ratios<-(ss/(((signalDetectionScalingFactor)^2)*sr))
    ratios[ratios=='Inf']<-(-1)
    ratios[ratios=='NaN']<-(-2)
    ratios <- as.vector(ratios)
    #rbind to the global variable ratiosByCountDF
    ratiosByCountDF <<- rbind(ratiosByCountDF,data.frame(Window=ws,Ratio=paste(signalStain,classLabel,":",classLabel),Value=ratios,ClassLabel=classLabel,SignalLabel=signalStain,ClassArea=as.vector(sr),TreatmentArm=treatmentArm,ClassFile=classFile,SignalFile=signalFile))
  }
}
countCells <- function(...,na.rm=F){
  dots <- list(...)
  rasterImage <- as.matrix(dots[[1]]) 
  return(length(unique(as.vector(rasterImage[rasterImage!=0]))))
}
doStats <- function(forWilcox, windowSize, windowsPerDraw){
  LT <- data.frame()
  GT <- data.frame()
  #forWilcox <- subset(ihcrAll, ihcrAll$Survival <= ii)
  forWilcox <- forWilcox[!is.nan(forWilcox$Value),]
  forWilcox <- forWilcox[!is.na(forWilcox$Value),]
  forWilcox <- forWilcox[forWilcox$Value != Inf,]
  if(length(unique(forWilcox$TreatmentArm))<2){
    return(data.frame())
  }
  tempLT <- ddply(forWilcox,.(Ratio,NoOfRepeats,Repeat),function(x){doWilcox(x,"less")})
  tempLT$Window <- windowSize
  tempLT$WindowsPerDraw <- windowsPerDraw
  tempLT$Test <-"less"
  LT<-rbind(LT,tempLT)
  tempGT <- ddply(forWilcox,.(Ratio,NoOfRepeats,Repeat),function(x){doWilcox(x,"greater")})
  tempGT$Window <- windowSize
  tempGT$WindowsPerDraw <- windowsPerDraw
  tempGT$Test <- "greater"
  GT<-rbind(GT,tempGT)
  ltColNames <- colnames(LT)
  if(ltColNames[1]=='X'){
    LT <- LT[,2:ncol(LT)]
  }
  gtColNames <- colnames(GT)
  if(gtColNames[1]=='X'){
    GT <- GT[,2:ncol(GT)]
  }
  wilcoxResults <- rbind(LT,GT)
  return(wilcoxResults)
}
doWilcox<- function(IHCR,test){
  tarm <- sort(unique(IHCR$TreatmentArm))
  n<-1
  colNames<-c("Ratio")
  pairwiseWilcoxTest<-data.frame()
  colNames<-c("Ratio")
  for (i in 1:length(tarm)){
    for (j in 1:length(tarm)){
      if (i<j){
        n<-n+1
        IHCR_pairs<-subset(IHCR,IHCR$TreatmentArm==tarm[i] | IHCR$TreatmentArm==tarm[j])
        tmp <-ddply(IHCR_pairs,.(Ratio),function(x){data.frame(p.value=wilcox.test(subset(x,x$TreatmentArm==tarm[i])$Value,subset(x,x$TreatmentArm==tarm[j])$Value,exact = TRUE,alternative=test)$p.value)})
        COLNAME<-paste(tarm[i],tarm[j],sep=".")
        colNames<-c(colNames,COLNAME)
        if (n==2){
          pairwiseWilcoxTest<-tmp
          names(pairwiseWilcoxTest) <-c("Ratio",COLNAME)
        } else {
          pairwiseWilcoxTest<-cbind(pairwiseWilcoxTest,data.frame(COLNAME=tmp$p.value))
        }
      }
    }
  }
  colnames(pairwiseWilcoxTest)<-gsub("\\+",".",colNames)
  return(pairwiseWilcoxTest)
}
callWilcox <- function(samples){
  repeatedSamples <- samples[rep(1:nrow(samples),times=samples$Duplicates),]
  wilcoxResults <- doStats(repeatedSamples,unique(samples$Window),unique(samples$WindowsPerDraw))          
  wilcoxTestResults <<- rbind(wilcoxTestResults,wilcoxResults) 
}
#function definitions end
# list of classes and paths to the class segmented output - here 'live' and 'necr are classes and the following vectors
# are paths to the segmented outputs for a given treatment arm (arm1 here)
#named list - names correspond to region 
arm1Classes <- list()

# list of classes and paths to the signal detection output over the specified class - here 'liveSignal' and 'necrSignal'
# are signals and the following vectors are paths to the segmented outputs of signals coming from 'live' and 'necr' classes
# respectively.  There is a 1:1 correspondance between the ordering of class segmented output and signal detection output for
# a given treatment arm (arm1 here)
arm1Signals <- list()
arm2Classes <- list()
arm2Signals <- list()

#combine all lists to a single list of classes and signals with all treatment arms
armClasses <- list(arm1=arm1Classes,arm2=arm2Classes)
armSignals <- list(arm1=arm1Signals,arm2=arm2Signals)

#iterate through treatment arms
#iterate through classes
#iterate through images
#get corresponding signal image and call function to calculate ratios
for(arm in names(armClasses)){
  armxClassesList <- armClasses[[arm]]
  for(Class in names(armxClassesList)){
    print(paste("class",Class))
    classFileList <- armClasses[[arm]][[Class]]
    print(classFileList)
    #signal file lists are named '<className>Signal' - explains the paste(Class,'Signal',sep="") indexing scheme
    signalFileList <- armSignals[[arm]][[paste(Class,"Signal",sep="")]]
    print(signalFileList)
    for(i in 1 : length(classFileList)){
      # here '/' is the file path separator for unix based systems.
      # if using windows change is to '\'
      classFileName <- unlist(strsplit(classFileList[i],'/'))
      classFileName <- classFileName[length(classFileName)]
      signalFileName <- unlist(strsplit(signalFileList[i],'/'))
      signalFileName <- signalFileName[length(signalFileName)]
      if(ratioByArea){  
        calculateRatiosByArea (classFileList[i], signalFileList[i], Class,arm, classFileName,signalFileName)
      }else{
        #calculateRatiosByCount(classFileList[i], signalFileList[i], Class,arm, classFileName,signalFileName)
        calculateRatiosByCountByCenterOfMass(classFileList[i], signalFileList[i], Class,arm, classFileName,signalFileName)
      }
    }
    
  }
}
fileName <- paste(outputDir,'/Ratios',ifelse(ratioByArea,'ByArea.rdata','ByCount.rdata'),sep="")
ratios<-if(ratioByArea)ratiosByAreaDF else ratiosByCountDF
save(ratios,file=fileName)
#######################################################################
# Creates data frame by repeated sampling of the above data frame
# Performs statistical comparison using wilcox test 
# Creates confidence intervals from weighted means of samples and
# uses the non-overlapping confidence interval method to determine
# if the means are statistically significant at a confidence level of 95%
#######################################################################
#ratiosDF <- if(ratioByArea) ratiosByAreaDF else ratiosByCountDF
tmpFN <- ifelse(ratioByArea,paste(outputDir,'/RatiosByArea.rdata',sep=""),paste(outputDir,'/RatiosByCount.rdata',sep=""))
ratiosDF = get(load(tmpFN))
ratiosDF$UID <- paste(ratiosDF$Window,ratiosDF$TreatmentArm,ratiosDF$ClassLabel,ratiosDF$SignalLabel,ratiosDF$ClassFile,ratiosDF$SignalFile)
means <- data.frame()
meansPerUID <- data.frame()
for(nor in noOfRepeats){
  for(win in wsize){
    print(paste("Win",win,"NOR",nor))
    for(wpd in noOfTilesPerDraw){
      for(rat in unique(ratiosDF$Ratio)){
        for(i in 1 : nor){
          samples <- data.frame()
          for(ta in unique(ratiosDF$TreatmentArm)){
            for(uid in unique(ratiosDF$UID)){
              subsetOfInterest <- ratiosDF[ratiosDF$UID==uid & ratiosDF$TreatmentArm == ta & ratiosDF$Value>=0 & ratiosDF$Value <=1& ratiosDF$Window==win &
                                             ratiosDF$Ratio==rat,]
              if(nrow(subsetOfInterest)==0){
                next # if the uid does not belong to the treatement arm in the current loop
              }
              totalSamples <- wpd
              ratSample <- subsetOfInterest[sample(nrow(subsetOfInterest),totalSamples,replace=T),]
              ratSample$WindowsPerDraw <- wpd
              ratSample$Repeat <- i
              ratSample$NoOfRepeats <- nor
              ratSample$PercentageArea <- ratSample$ClassArea / ((ratSample$Window/signalDetectionScalingFactor)^2) #accounts for tissue classification resolution difference
              ratSample$Duplicates <- round((ratSample$PercentageArea)*100/jumps)
              ratSample$Duplicates[ratSample$Duplicates==0] <- 1
              samples <- rbind(samples,ratSample)
            }
          }
          means <- rbind(means,ddply(samples,.(Ratio,Window,TreatmentArm,WindowsPerDraw,NoOfRepeats),function(x){data.frame(Mean=wt.mean(x$Value,x$ClassArea))}))
          meansPerUID <- rbind(meansPerUID,ddply(samples,.(Ratio,Window,TreatmentArm,UID,WindowsPerDraw,NoOfRepeats),function(x){data.frame(Mean=wt.mean(x$Value,x$ClassArea))}))
          callWilcox(samples)
        }
      }
    }
  }
}
wilcoxResultsFileName <- paste(outputDir,'/WilcoxResults',ifelse(ratioByArea,'ByArea.rdata','ByCount.rdata'),sep="")
cis <- ddply(means,.(Ratio,Window,TreatmentArm,WindowsPerDraw,NoOfRepeats),function(x){data.frame(Mean=mean(x$Mean),I2p5=quantile(x$Mean,probs=c(0.025)),I97p5=quantile(x$Mean,probs=c(0.975)))})
#change arm1 and arm2 x$arm1.arm2 in the following lines to match the name of the treatment arms you specified in armClasses list
arm1CIs <- cis[cis$TreatmentArm=='arm1',]
arm2CIs <- cis[cis$TreatmentArm=='arm2',]
cisMerged <- merge(arm1CIs,arm2CIs,by=c("Ratio","Window","WindowsPerDraw","NoOfRepeats"))
cisMerged$NotOverlap <- cisMerged$I97p5.x <=cisMerged$I2p5.y |
  cisMerged$I97p5.y <= cisMerged$I2p5.x
cisMerged$Arm1LTArm2 <- cisMerged$I97p5.x <=cisMerged$I2p5.y
cisMerged$Arm2LTArm1 <- cisMerged$I97p5.y <= cisMerged$I2p5.x
cisFileName <- paste(outputDir,'/ConfidenceIntervalsResults',ifelse(ratioByArea,'ByArea.rdata','ByCount.rdata'),sep="")
save(cisMerged,file=cisFileName)
save(wilcoxTestResults,file=wilcoxResultsFileName)
############################################################
#calculate probability of significance
############################################################
tempFN <- ifelse(ratioByArea,paste(outputDir,'/WilcoxResultsByArea.rdata',sep=""),paste(outputDir,'/WilcoxResultsByCount.rdata',sep=""))
wilcoxTestResults = get(load(tempFN))
tempFN <- ifelse(ratioByArea,paste(outputDir,'/ConfidenceIntervalsResultsByArea.rdata',sep=""),paste(outputDir,'/ConfidenceIntervalsResultsByCount.rdata',sep=""))
cisMerged = get(load(tempFN))
#calculate probability of significance per alternative hypothesis of the wilcox test
# as the fraction of tests with p value < 0.05 
#change arm1 and arm2 x$arm1.arm2 in the following line to match the name of the treatment arms you specified in armClasses list
probabilityOfSignificance <- ddply(wilcoxTestResults,.(Window,WindowsPerDraw,NoOfRepeats,Ratio,Test),
                                   function(x){ProbOfSignif={data.frame(ProbOfSignif=nrow(x[x$arm1.arm2 <0.05,])/nrow(x))}})
#Threshold probabilityOfSignificance at 95% for plotting 
probabilityOfSignificance$ProbOfSignif[probabilityOfSignificance$ProbOfSignif<=0.95] <-0
probabilityOfSignificance$ProbOfSignif[probabilityOfSignificance$ProbOfSignif>0.95 & probabilityOfSignificance$Test=="less"] <-1
probabilityOfSignificance$ProbOfSignif[probabilityOfSignificance$ProbOfSignif>0.95 & probabilityOfSignificance$Test=="greater"] <- (-1)
# restructure the CI results to match the probabilityOfSignificance data frame to enable merging
ciResultsLT <- cisMerged[c(1,2,3,4,6,7,8,14)]
ciResultsGT <- cisMerged[c("Ratio","Window","WindowsPerDraw","NoOfRepeats","Mean.y","I2p5.y","I97p5.y","Arm2LTArm1")]
colnames(ciResultsLT)<- gsub("\\.x","",colnames(ciResultsLT))
colnames(ciResultsLT)<-gsub("Arm1LTArm2","CIsOverlap",colnames(ciResultsLT))
ciResultsLT$Test <-"less"
colnames(ciResultsGT)<- gsub("\\.y","",colnames(ciResultsGT))
colnames(ciResultsGT)<-gsub("Arm2LTArm1","CIsOverlap",colnames(ciResultsGT))
ciResultsGT$Test <- "greater"
cisResults <- rbind(ciResultsLT,ciResultsGT)
cisResults$ProbOfSignifCI <- ''
cisResults$ProbOfSignifCI[cisResults$CIsOverlap==T & cisResults$Test=="less"] <-'.'
cisResults$ProbOfSignifCI[cisResults$CIsOverlap==T & cisResults$Test=="greater"] <-'-'
#merge wilcox and CI results
wilcoxAndCIResultsMerged <- merge(probabilityOfSignificance,cisResults,by=c("Ratio","Window","WindowsPerDraw","NoOfRepeats","Test"))
#plot and save
wilcoxAndCIResultsMerged$WindowFactor <- as.factor(wilcoxAndCIResultsMerged$Window)
wilcoxAndCIResultsMerged$WindowsPerDrawFactor <- as.factor(wilcoxAndCIResultsMerged$WindowsPerDraw)
wilcoxAndCIResultsMerged$ProbOfSignif <- as.factor(wilcoxAndCIResultsMerged$ProbOfSignif)
plot <- ggplot(wilcoxAndCIResultsMerged,aes(x=WindowFactor,y=WindowsPerDrawFactor,fill=ProbOfSignif))+geom_tile()+
  scale_fill_manual(values=c("-1"="lightblue","0"="white","1"="red"))+
  xlab(expression(paste("Window Size (",mu,"m)",sep="")))+ylab("No of Sample Windows Per WSI")+facet_grid(.~Ratio)
plotFileName <- paste(outputDir,'/Significance',ifelse(ratioByArea,'ByArea.png','ByCount.png'),sep="")
ggsave(file=plotFileName,plot = plot, units="in",dpi=300,width=7,height=3,scale = 1)