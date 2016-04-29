# package申明；
if(!require(gbm)) install.packages("gbm")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

#package的引用
library(gbm)
library(ggplot2)
library(dplyr)
options(stringsAsFactors = FALSE) #  char类型不要自动转化成 factor

#  用上线的数据来进行重新训练；
setwd("/home/pjpan/virtualroom")
HTL_detail <- read.table(file="./ppj_reservedroomlog_online.txt", sep=',', na.strings = 'null', stringsAsFactors = FALSE, header =TRUE)
HTL_score <- read.table(file="./ppj_confirm_noroom.txt", sep='\t', na.strings = 'null', stringsAsFactors = FALSE, header =TRUE)
HTL_score$isfullroom <- ifelse(HTL_score$cancelreason=='NOROOM',1,0)

HTL <- left_join(HTL_score, HTL_detail, by = 'orderid')

# rm(list= ls()) # 清除所有的变量；
# pre_process ，数据预处理
HTL[is.na(HTL)] <- 0
HTL$hotelroomquantity <- as.integer(HTL$hotelroomquantity)
HTL$roomquantity <- as.integer(HTL$roomquantity)

# 预测房间缺失值数值大小；
HTL$pred.roomqty <- ifelse(HTL$roomquantity <=0 ,ifelse(HTL$maxroomorder_r30d>0, HTL$maxroomorder_r30d, HTL$maxhtlorder_r30d) , HTL$roomquantity)  # 如果没有房型的房间数，用历史出现的最大房量来替代替代；

HTL$last1ynoroomrate <- ifelse(as.numeric(HTL$last1ynoroomrate)>=1, 1, as.numeric(HTL$last1ynoroomrate))
HTL$masterhtlnoroomrate <- ifelse(as.numeric(HTL$masterhtlnoroomrate)>=1, 1, as.numeric(HTL$masterhtlnoroomrate))

# 字符串变成数值型的结果,如果百分比超过1，看做是1;
HTL$roompct <- ifelse(as.numeric(HTL$room_sumorder)/as.numeric(HTL$pred.roomqty)>=1, 1
                      ,as.numeric(HTL$room_sumorder)/as.numeric(HTL$pred.roomqty))

HTL$roomnoroompct <- ifelse(as.numeric(HTL$room_noroomsumorder)/as.numeric(HTL$pred.roomqty)>=1, 1
                            ,as.numeric(HTL$room_noroomsumorder)/as.numeric(HTL$pred.roomqty))

HTL$htlnoroompct <- ifelse(as.numeric(HTL$htlnoroompct) >= 1, 1, as.numeric(HTL$htlnoroompct))
HTL$htlroompct <- ifelse(as.numeric(HTL$htlroompct) >= 1,1 ,as.numeric(HTL$htlroompct))

# 历史满房临界点进行预处理；
HTL$maxroomorder <- as.integer(HTL$maxroomorder)
HTL$maxroompct <- ifelse(as.numeric(HTL$maxroomorder)/as.numeric(HTL$pred.roomqty) >=1, 1, as.numeric(HTL$maxroomorder)/as.numeric(HTL$pred.roomqty))

HTL$maxhtlorder <- as.integer(HTL$maxhtlorder)
HTL$maxhtlpct <- ifelse(as.integer(HTL$maxhtlorder)/HTL$hotelroomquantity>=1, 1, as.integer(HTL$maxhtlorder)/HTL$hotelroomquantity)
HTL$maxhtlroompct <- ifelse(as.numeric(HTL$maxhtlroompct)>=1, 1, as.numeric(HTL$maxhtlroompct))

HTL$maxroompct_r30d <- ifelse(as.numeric(HTL$maxroomorder_r30d)/as.numeric(HTL$pred.roomqty) >= 1, 1, 
                              as.numeric(HTL$maxroomorder_r30d)/as.numeric(HTL$pred.roomqty))
HTL$maxroomorder_r30d <- as.integer(HTL$maxroomorder_r30d)

HTL$maxhtlroompct_r30d <- ifelse(as.numeric(HTL$maxhtlroompct_r30d)>=1, 1, as.numeric(HTL$maxhtlroompct_r30d))
HTL$maxhtlorder_r30d <- as.integer(HTL$maxhtlorder_r30d)

HTL$isoverroommax <- ifelse((HTL$room_sumorder+as.integer(HTL$ordroomnum)) > HTL$maxroomorder, 1, 0)
HTL$overroommaxpct <- (HTL$room_sumorder+as.integer(HTL$ordroomnum))/HTL$maxroomorder-1

HTL$isoverhtlmax <- ifelse(HTL$htl_sumorder > HTL$maxhtlorder, 1, 0)
HTL$overhtlmaxpct <- HTL$htl_sumorder/HTL$maxhtlorder-1

HTL$room_last7doccupyrate <- ifelse(as.numeric(HTL$room_last7dsumorder)/as.numeric(HTL$pred.roomqty)>=1, 1
                                    ,as.numeric(HTL$room_last7dsumorder)/as.numeric(HTL$pred.roomqty))
HTL$room_last3doccupyrate <- ifelse(as.numeric(HTL$room_last3dsumorder)/as.numeric(HTL$pred.roomqty)>=1, 1
                                    ,as.numeric(HTL$room_last3dsumorder)/as.numeric(HTL$pred.roomqty))

# transform metrics
HTL$ordroomnum <- ifelse(HTL$ordroomnum >= 6, -1,HTL$ordroomnum)  # 超过6的归结为一类，与其他的category来进行平衡；
HTL$orddays <- ifelse(HTL$orddays >= 3, -1, HTL$orddays)  # 超过3的归结为一类，与其他的category来进行平衡；

HTL$cprflag <- ifelse(HTL$cprflag >=3 | is.null(HTL$cprflag), -1 , HTL$cprflag) # 平衡数据量值

HTL$arrivalhour <- ifelse(HTL$arrivalhour <= 17, 1 , HTL$arrivalhour) # 最晚的到点时间一般性是从18:00开始的；

HTL$recommendlevel <- ifelse(HTL$recommendlevel>= 10 | HTL$recommendlevel <= 0 | is.na(HTL$recommendlevel), -1 
                             , HTL$recommendlevel)  #  超过10 和小于0的看做一类别，跟其他类别的量级持平；

HTL$level <- ifelse(HTL$level>= 3 & HTL$level <=5, -2, HTL$level)  # 中间档的酒店订单量太小，合并

HTL$ordprior <- ifelse(HTL$ordprior>=1 &　HTL$ordprior<=7,7
                       ,ifelse(HTL$ordprior>7 & HTL$ordprior <=14,14
                               ,ifelse(HTL$ordprior>14 & HTL$ordprior <=21,21
                                       ,ifelse(HTL$ordprior>21 & HTL$ordprior <=30,30,
                                               ifelse(HTL$ordprior >30, 35, ifelse(HTL$ordprior<0,-1,HTL$ordprior))
                                       ))))

HTL$ordhour <- as.integer(HTL$ordhour)
HTL$htlsmtpct <- as.numeric(HTL$htlsmtpct)
HTL$roomhtlsmtpct <- as.numeric(HTL$roomhtlsmtpct)

HTL$room_noroomrate <- as.numeric(HTL$room_noroomrate)
HTL$room_sumorder <- as.numeric(HTL$room_sumorder)
HTL$isfullroom <- ifelse(HTL$cancelreason == 'NOROOM',1,0)
HTL[is.na(HTL)] <- -1         # # 把一些因子的NA变成 -1

#  ## 提炼出测试集和训练集 ##
set.seed(121)
trainsample <- rbinom(dim(HTL)[[1]], 1, 0.5)==1
# split train & test result
HTLtrain <- HTL[trainsample,]
HTLtest <- HTL[!trainsample,]
responsetrain <- filter(HTLtrain, isfullroom==1)
responsetest <- filter(HTLtest, isfullroom==1)

# # GBM模型 
raw.gbm <- gbm(isfullroom ~
                 cityi # cityi 与 zonei之间有强的相关性；
               +gcity   
               +zonei
               +gzone
               +zonestari       
               +intense
               +last1ynoroomrate      # 最近一年同子酒店的订单满房率  level 4: 11.8461426
               +masterhtlnoroomrate   # 最近7天同母酒店的订单满房率   level 3: 15.3741806
               +htlroompct             # 同酒店同入住天基础房型的占比  # level 6: 8.50037860
               +htlnoroompct         # level 2: 37.24       
               +roompct              # level 8: 13.5719766
               +roomnoroompct        # level 2: 14.51
               +htlsmtpct             # level 5: 9.2622999
               +roomhtlsmtpct           # level 8: 7.9173105
               +room_sumorder         # 最近一个月的同房型同入住天的订单数
               +room_noroomrate       # level 3: 31.6725872
               +htl_noroomrate     # 1 重要变量 leve l: 17.91828668
               +room_last7doccupyrate       # level 7: 7.0071140               
               +room_last3doccupyrate    # level 7: 6.1129707
               +factor(ordhour)  # 2 重要变量   leve l: 12.17132199
               +factor(level)                      # level 8: 8.2122337
               +factor(recommendlevel)        # level 5: 7.2352479
               +factor(ordroomnum)       # level 6：5.96696433 
               +maxroomorder                 # level 3 : maxroomorder
               +maxhtlorder                     # level 6: 6.77403782
               +maxhtlroompct                   # level 4: 10.2556069                 
               +maxroompct                       # level 5： 13.4864222
               #                +maxroompct_r30d                 # level 6: 5.71153070
               #                +maxroomorder_r30d                 # level 6 : 8.02089829
               #                +maxhtlorder_r30d                  # level 7: 14.2531900
               #                +factor(isoverroommax)
               #                +factor(isoverhtlmax)
               #                +factor(orddays)
               #                # +factor(arrivalhour)
               #                # +factor(ispromotion)           # 是否促销房型   
               #                # +factor(cprflag)
               #                +factor(goldstar)
               #                #+factor(isweekday)
               +factor(ordprior)
               #                +htl_last3dsumorder
               #                +room_last3dsumorder
               #                +htl_last7dsumorder
               #                +htl_sumorder
               #                +room_last7dsumorder               
               ,data = HTLtrain
               #,data = HTLtest
               ,distribution = "adaboost"               
               ,n.trees = 1500               
               ,shrinkage = 0.02               
               ,interaction.depth = 3               
               ,bag.fraction = 0.6               
               ,train.fraction=0.5              
               ,cv.folds=3
               ,keep.data = FALSE
               #               ,n.minobsinnode = 10
)

# summary(raw.gbm)
best.raw <- gbm.perf(raw.gbm, method='cv')
# Roc curve
HTLtest$pred <- predict(raw.gbm, newdata= HTLtest, n.trees = best.raw, type="response")
HTLtrain$pred <- predict(raw.gbm, newdata= HTLtrain, n.trees = best.raw, type="response")
testerror <- gbm.roc.area(HTLtest$isfullroom, HTLtest$pred)  # 0.8009718 (1.13/1.12日数据)   # step3: 0.7319154   # step4: 0.71   # step5: 0.69  # step 6 ： 0.6838834
trainerror <- gbm.roc.area(HTLtrain$isfullroom, HTLtrain$pred)  # 0.7921033

# 指标评估
pred.gbm <- HTLtest$pred
summary(pred.gbm)
# write.csv(data.frame(HTLtest,pred.gbm),file = "D:/project/【HTL】Offline虚拟保留房模型/模型/test/model.0317.csv")
accuracy = recall = specificity = sensitivity = NULL
cutoffPoint=seq(0.001,1,0.001)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred.gbm >=c1)
  pred[is.na(pred)] <- 0
  specificity[i] <- sum((pred==1) & (HTLtest$isfullroom == 1))/sum(pred == 1)
  sensitivity[i] <- sum((pred==1) & (HTLtest$isfullroom==1))/sum(HTLtest$isfullroom==1)
  recall[i] <- sum((pred==0) & (HTLtest$isfullroom==0))/sum(HTLtest$isfullroom==0)
  accuracy[i] <- sum((pred==0) & (HTLtest$isfullroom==0))/sum(pred==0)
}
resultPred.test2 <- data.frame(cutoffPoint, accuracy, recall, specificity, sensitivity)
View(resultPred.test2)

