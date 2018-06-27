library(needs)
needs(data.table,readr)
train <- fread("/media/3TB/kaggle/fb/data/train.csv",integer64 = "character")
trainsub <- subset(train,train$x>=5& train$x<=5.5 & train$y>=5 & train$y<=5.5)

setkey(trainsub,"place_id")
perIdSum <- trainsub[,.N,by=key(trainsub)]
# df <- data.frame(Product=gl(3,10,labels=c("A","B", "C")), 
#                  Year=factor(rep(2002:2011,3)), 
#                  Sales=1:30)
# dt <- data.table(df) 
# setkey(dt, "Year")
# X <- dt[, list(SUM=sum(Sales)), by=key(dt)] 
# R3 <- dt[X, list(Sales, Product, Share=Sales/SUM)]