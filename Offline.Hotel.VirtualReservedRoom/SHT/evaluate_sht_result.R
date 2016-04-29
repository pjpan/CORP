# package申明；
if(!require(gbm)) install.packages("gbm")
if(!require(rJava)) install.packages("rJava")
if(!require(RJDBC)) install.packages("RJDBC")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(randomForest)) install.packages("randomForest")

#package的引用
library(gbm)
library(ggplot2)
library(dplyr)

options(stringsAsFactors = FALSE) #  char类型不要自动转化成 factor

SHT_raw <- read.table(file = "D:\\project\\【HTL】Offline虚拟保留房模型\\供应商模型\\sht03orders.txt" ,sep ='\t', na.strings= 'NULL', header= TRUE) # 2.1~3.23 新数据结果

# vdate <- unique(SHT_raw$d)
vdate<-seq(as.Date("2015-02-18"), length=32, by="day")
roc <- NULL
recall <- NULL
accuracy <- NULL
random <- NULL
reorders <- NULL
misorders <- NULL

# 循环统计出最后的汇总结果；
for(i in 1:length(vdate))
{
  test <- filter(SHT_raw, d == vdate[i]) # 只针对过去3天没有出现过满房（同入住天）的订单进行判断；如果同子酒店；并且同子酒店过去7天的满房单少于5；
# last7dnoroom == 0, htlnoroomorders_7d <= 5
# 如果最近3天供应商出现过满房，则不能立即确认； # 如果最近7天同子酒店出现过满房次数超过5次，则不能立即确认；
  # pre_process ，数据预处理测试集合的数据结果；
  test[is.na(test)] <- 0
  # transform metrics
  test$ordroomnum <- ifelse(test$ordroomnum >= 6, -1,test$ordroomnum)  # 超过6的归结为一类，与其他的category来进行平衡；
  test$orddays <- ifelse(test$orddays >= 3, -1, test$orddays)  # 超过3的归结为一类，与其他的category来进行平衡；
  test$cprflag <- ifelse(test$cprflag >=3 | is.null(test$cprflag), -1 , test$cprflag) # 平衡数据量值
  test$recommendlevel <- ifelse(test$recommendlevel>= 10 | test$recommendlevel <= 0 | is.na(test$recommendlevel), -1 
                                , test$recommendlevel)  #  超过10 和小于0的看做一类别，跟其他类别的量级持平；
  test$level <- ifelse(test$level>= 3 & test$level <=4, -2, test$level)  # 中间档的酒店订单量太小，合并
  
  test$star <- ifelse(test$star<=3, 3, test$star)  # 供应商高星级的酒店居多
  
  test$ordprior <- ifelse(test$ordprior>=1 &　test$ordprior<=7,7
                          ,ifelse(test$ordprior>7 & test$ordprior <=14,14
                                  ,ifelse(test$ordprior>14 & test$ordprior <=21,21
                                          ,ifelse(test$ordprior>21 & test$ordprior <=30,30,
                                                  ifelse(test$ordprior >30, 35, test$ordprior)
                                          ))))

  test$ordhour <- as.integer(test$ordhour)
  test$roomtypestd <- ifelse(as.integer(test$roomtypestd)>1,0,1)
  test$last3droompct <- ifelse(test$roomquantity > 0, test$last3droom/test$roomquantity, -1)
  test$last3droompct <- ifelse(test$last3droompct > 1, 1, test$last3droompct)
  test$last3droom_sht <- ifelse(test$last3droom_sht >= 7 , 7, test$last3droom_sht)
  test$ordhour <- ifelse(test$ordhour <= 7 , 7, test$ordhour)
  test$last30droom <- ifelse(test$last30droom >= 11 , 11, test$last30droom)
  
  test$htltotalorders_7d <- ifelse(test$htltotalorders_7d >= 70 , 70, test$htltotalorders_7d)  #  同子酒店的订单量
  # test$htlnoroomorders_7d <- ifelse(test$htlnoroomorders_7d >= 9 , 9, test$htlnoroomorders_7d)  #  同子酒店的无房订单量
  
  test[is.na(test)] <- -1         # # 把一些因子的NA变成 -1
  
  test$pred <- predict(gbm.sht, newdata= test, n.trees = best.raw, type="response")
  test$pred <- ifelse(test$htlnoroomorders_7d>=5, 1, test$pred)
  test$pred <- ifelse(test$last7dnoroom>=1, 1, test$pred)  # last7d无房的房量高于0

  table(test$htlnoroomorders_7d, test$isfullroom)

  # table(test$htlnoroomorders_7d, test$isfullroom)
  
  # roc[i] <- gbm.roc.area(test$isfullroom, test$pred)  # 0.6834796
  # }

  roc[i] <- gbm.roc.area(test$isfullroom, test$pred)  
  range <- 0.04
  recall[i] <- sum((test$pred <= range) & (test$isfullroom==0))/sum(test$isfullroom==0)
  accuracy[i] <- sum((test$pred <= range) & (test$isfullroom==0))/sum(test$pred <= range)
  random[i] <- sum(test$isfullroom==0)/nrow(test)
  totalcnt[i] <- nrow(test)
  reorders[i] <- sum((test$pred <= range) & (test$isfullroom==0))
  misorders[i] <- sum(test$pred <= range)-sum((test$pred <= range) & (test$isfullroom==0))    
}

finnal.evaluate <- data.frame(vdate, roc, recall, accuracy, random, totalcnt,reorders,misorders)
View(finnal.evaluate)

# ggplot() + geom_line(data = finnal.evaluate, aes(x= vdate, y =accuracy)) + geom_line(data = finnal.evaluate, aes(x= vdate, y = recall))
# 
# # 指标评估  ,precision & recall的评估结果；
# pred.gbm <- test$pred
# accuracy = recall = specificity = sensitivity = NULL
# cutoffPoint=seq(0.01,1,0.001)
# for (i in 1:length(cutoffPoint))
# {
#   c1 <- cutoffPoint[i]
#   pred <- 1*(pred.gbm >=c1)
#   specificity[i] <- sum((pred==1) & (test$isfullroom == 1))/sum(pred == 1)
#   sensitivity[i] <- sum((pred==1) & (test$isfullroom==1))/sum(test$isfullroom==1)
#   recall[i] <- sum((pred==0) & (test$isfullroom==0))/sum(test$isfullroom==0)
#   accuracy[i] <- sum((pred==0) & (test$isfullroom==0))/sum(pred==0)
# }
# sht.finnal.result <- data.frame(cutoffPoint, accuracy, recall, specificity, sensitivity)
# ggplot(data = sht.finnal.result)+geom_line(aes(x= recall , y=  accuracy))

# View(sht.finnal.result)
# tail(sht.finnal.result)

