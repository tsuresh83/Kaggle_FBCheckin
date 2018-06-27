# The idea of data transformation and raiting calculations is from Python script 
# by 'ZFTurbo: https://kaggle.com/zfturbo'
rm(list=ls())
#########################################################################
new_dim_x1  <- 290     # new dimensions for x 
new_dim_y1  <- 725     # new dimensions for y
new_dim_x2  <- 145     # new dimensions for x 
new_dim_y2  <- 362     # new dimensions for y
#########################################################################
library(needs)
needs(dplyr,readr,foreach,data.table,tidyr,xgboost) 
train <- fread("/media/3TB/kaggle/fb/data/train.csv", integer64 = "character")
test <- fread("/media/3TB/kaggle/fb/data/test.csv", integer64 = "character")

train <- train[,
               .(row_id,
                 x1 = as.integer(floor(x/10 * new_dim_x1)),
                 y1 = as.integer(floor(y/10 * new_dim_y1)),
                 x2 = as.integer(floor(x/10 * new_dim_x2)),
                 y2 = as.integer(floor(y/10 * new_dim_y2)),               
                 quarter_period_of_day = as.integer(floor((time + 120) / (6*60)) %% 4),
                 hour = as.integer(floor(time/60) %% 24),
                 dayOfWeek = as.integer(floor(time/60/24) %% 7),
                 monthOfYear=as.integer(floor(time/60/24/30) %% 12),
                 time,
                 place_id),
               ]


test <- test[,
             .(row_id,
               x1 = as.integer(floor(x/10 * new_dim_x1)),
               y1 = as.integer(floor(y/10 * new_dim_y1)),
               x2 = as.integer(floor(x/10 * new_dim_x2)),
               y2 = as.integer(floor(y/10 * new_dim_y2)),               
               quarter_period_of_day = as.integer(floor((time + 120) / (6*60)) %% 4),
               hour = as.integer(floor(time/60) %% 24),
               dayOfWeek = as.integer(floor(time/60/24) %% 7),
               monthOfYear=as.integer(floor(time/60/24/30) %% 12)),
             ]

xGridSize <- 4 #~side 2
yGridSize <- 4 #~side 1
train$PositionGrp <-"-"
test$PositionGrp <-"-"
xCut <- c(-1,seq(xGridSize,(new_dim_x1),xGridSize))
yCut <- c(-1,seq(yGridSize,(new_dim_y1),yGridSize))
train$XPosBin <- cut(train$x1,breaks = xCut,right = T)
train$YPosBin <- cut(train$y1,breaks = yCut,right=T)
train$PositionGrp <- paste(train$XPosBin,train$YPosBin)
test$XPosBin <- cut(test$x1,breaks = xCut,right = T)
test$YPosBin <- cut(test$y1,breaks = yCut,right=T)
test$PositionGrp <- paste(test$XPosBin,test$YPosBin)
sampleGrid <- train[train$PositionGrp=="(168,172] (344,348]",]

#setkey(train,"PositionGrp")
#pidsPerGrid <- train[,list(UniquePlaceIds=.N),by=key(train)]
# test_chunks <- split(test, test$hour)
# result <- foreach(chunk=1:length(test_chunks), .combine="rbind", .packages = "dplyr") %do% 
# {
#   print(sprintf("task %d/%d", chunk, length(test_chunks)))
#   test_chunk <- test_chunks[[chunk]]
#   hour_test <- test_chunk$hour[1] 
#   
#   #####################################################
#   train[,rating:= dnorm(hour_test, mean=hour_mean, sd=hour_sd) * rating_history,]
#   #####################################################
# 
#   print("Train group 1")
#   train_group1  <- train[,.(rating=sum(rating)), by=.(x1, y1, quarter_period_of_day, place_id)][order(-rating)]
#   train_group1 <- train_group1[,.(place_id, pos=seq_len(.N)), by=.(x1, y1, quarter_period_of_day)][pos<=3][,pos:=NULL]
#   
#   print("Join1")
#   setkey(test_chunk,x1, y1, quarter_period_of_day)
#   setkey(train_group1,x1, y1, quarter_period_of_day)
#   
#   test_train_join1 <-
#     test_chunk[,.(row_id, x1, y1, quarter_period_of_day)][train_group1, nomatch=0, allow.cartesian=TRUE][,.(row_id, place_id)]
# 
#   validate_test_train_join <- test_train_join1[,.(count=.N),by=(row_id)][count==3]
#   test_chunk <- test_chunk[!validate_test_train_join, on = "row_id"]
#   
#   print("Join2")
#   setkey(test_chunk,x1, y1)
#   setkey(train_group1,x1, y1)
#   
#   test_train_join2 <-
#     test_chunk[,.(row_id, x1, y1)][train_group2, nomatch=0, allow.cartesian=TRUE][,.(row_id, place_id)]
#   
#   validate_test_train_join <- test_train_join2[,.(count=.N),by=(row_id)][count==3]
#   test_chunk <- test_chunk[!validate_test_train_join, on = "row_id"]
#   
#   print("Join3")
#   setkey(test_chunk,x2, y2)
#   setkey(train_group3,x2, y2)
#   
#   test_train_join3 <-
#     train_group3[test_chunk[,.(row_id, x2, y2)], nomatch=NA, allow.cartesian=TRUE][,.(row_id, place_id)]
#   
#   #validate_test_train_join <- test_train_join3[,.(count=.N),by=(row_id)][count==3]
#   #test_chunk <- test_chunk[!validate_test_train_join, on = "row_id"]
#   #validate_test_train_join <- NULL
#  
#   print("Group all")
#   test_train_join_all <- rbindlist(list(test_train_join1,test_train_join2,test_train_join3), use.names=TRUE) %>% unique()
#   
#   # result_new <- test_train_join_all[, .(place_id = paste(head(place_id, 3),collapse=" ")), by = row_id]
#   test_train_join_all <- test_train_join_all[, .(place_id, pos=seq_len(.N)), by = .(row_id)][pos<=3]
#   test_train_join_all <- dcast.data.table(test_train_join_all, row_id~pos, value.var="place_id")
# 
#   print("Done")
#   
#   return(test_train_join_all)
# }
# 
# print("Converting to final format")
# setorder(result, row_id)
# #library(tidyr)
# result <- unite_(result, "place_id", c("1", "2", "3"), sep = " ")
# 
# print("Saving result")
# # result$place_id[result$place_id=="NA"] <- ""
# write_csv(result, "result.csv")