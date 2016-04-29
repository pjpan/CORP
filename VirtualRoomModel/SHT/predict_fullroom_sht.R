# package申明；
if(!require(gbm)) install.packages("gbm")
if(!require(rJava)) install.packages("rJava")
if(!require(RJDBC)) install.packages("RJDBC")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(randomForest)) install.packages("randomForest")
if(!require(broom)) install.packages("broom")
if(!require(glmnet)) install.packages("glmnet")

#package的引用
library(gbm)
library(ggplot2) 
library(dplyr)
library(broom)
library(sqldf)
library(glmnet)
library(ISLR)
library(car)
options(stringsAsFactors = FALSE) #  char类型不要自动转化成 factor

# options(error=stop)

# 数据集来进行训练；
SHT_raw <- read.table(file = "D:\\project\\【HTL】Offline虚拟保留房模型\\供应商模型\\sht03orders0506.txt" ,sep ='\t', na.strings= 'NULL', header= TRUE) # 2.1~3.23 新数据结果
SHT <- filter(SHT_raw, d>='2015-03-01'
               , last7dnoroom == 0
               , htlnoroomorders_7d <= 5
              )  # 只针对过去3天没有出现过满房的订单进行判断（同入住天）, 6%的总订单贡献了46%的满房订单；并且同子酒店过去7天的满房单少于5；

SHT_fullroom <- filter(SHT_raw, isfullroom==1)
####################save metrics distribution  as a PNG file########################### #
# metricname2 <- names(SHT_fullroom)
# for(i in 11:length(metricname2))
# {  
#     file<-paste("D:\\project\\【HTL】Offline虚拟保留房模型\\供应商模型\\变量分布\\fullroom_",metricname2[i],".png",sep='')
#     png(file)
#     eval(parse(text=paste0("p <- ggplot()+geom_histogram(data = SHT,aes(x=",metricname2[i],"))")))
#     print(p)
#     dev.off()
# }

names(SHT_fullroom)
SHT_fullroom_obj <- filter(SHT_fullroom, last7droom_sht>0)
train <- 1:(dim(SHT_fullroom_obj)[[1]]/2)
# split train $ test set
SHT_fullroom_train <- SHT_fullroom_obj[train,]
SHT_fullroom_test <- SHT_fullroom_obj[-train,]

# ############# predict roomquantity begin################ #

# split train & test set
train <- 1:(dim(shtrmqty)[[1]]/2)
shtrmqty_train <- shtrmqty[train,]
shtrmqty_test <- shtrmqty[-train,]

# 线性模型 pass，R^2 = 0.57
# model.lm <- lm(roomquantity ~ hotelroomquantity+factor(basicroomnum)*factor(roomtypestd)*factor(level)
#                , data=shtrmqty_train)
# tidy(model.lm)
# augment(model.lm)
# glance(model.lm)
# vif(model.lm) # 检验是否多重共线性
# shtrmqty_test$pred.rmqty <- predict(model.lm, newdata=shtrmqty_test)
# View(select(shtrmqty_test,hotel,room,pred.rmqty,roomquantity))
# ############# predict roomquantity end################ #


# ####################save metrics distribution  as a PNG file########################### #
# metricname <- names(SHT)
# for(i in 11:length(metricname))
# {  
#     file<-paste("D:\\project\\【HTL】Offline虚拟保留房模型\\供应商模型\\变量分布\\",metricname[i],".png",sep='')
#     png(file)
#     eval(parse(text=paste0("p <- ggplot()+geom_bar(data = SHT,aes(x=",metricname[i],", y= isfullroom),stat=\"identity\")")))
#     print(p)
#     dev.off()
# }

# #####################################################################################  #

# SHT %>% group_by(d,isfllroom) %>% summarise(count=n())

# rm(list= ls()) # 清除所有的变量；

# pre_process ，数据预处理
SHT[is.na(SHT)] <- 0
# transform metrics
SHT$ordroomnum <- ifelse(SHT$ordroomnum >= 3, -1,SHT$ordroomnum)  # 超过6的归结为一类，与其他的category来进行平衡；
SHT$orddays <- ifelse(SHT$orddays >= 3, -1, SHT$orddays)  # 超过3的归结为一类，与其他的category来进行平衡；
SHT$cprflag <- ifelse(SHT$cprflag >=3 | is.null(SHT$cprflag), -1 , SHT$cprflag) # 平衡数据量值
SHT$recommendlevel <- ifelse(SHT$recommendlevel>= 10 | SHT$recommendlevel <= 0 | is.na(SHT$recommendlevel), -1, SHT$recommendlevel)  #  超过10 和小于0的看做一类别，跟其他类别的量级持平；
SHT$level <- ifelse(SHT$level>= 3, -2, SHT$level)  # 中间档的酒店订单量太小，合并

SHT$star <- ifelse(SHT$star<=3, 3, SHT$star)  # 供应商高星级的酒店居多

SHT$ordprior <- ifelse(SHT$ordprior>=1 &　SHT$ordprior<=7,7
                       ,ifelse(SHT$ordprior>7 & SHT$ordprior <=14,14
                               ,ifelse(SHT$ordprior>14 & SHT$ordprior <=21,21
                                       ,ifelse(SHT$ordprior>21 & SHT$ordprior <=30,30,
                                               ifelse(SHT$ordprior >30, 35, SHT$ordprior)
                                       ))))

SHT$ordhour <- as.integer(SHT$ordhour)
SHT$roomtypestd <- ifelse(as.integer(SHT$roomtypestd)>1,0,1)
SHT$last3droompct <- ifelse(SHT$roomquantity > 0, SHT$last3droom/SHT$roomquantity, -1)
SHT$last3droompct <- ifelse(SHT$last3droompct > 1, 1, SHT$last3droompct)
SHT$last3droom_sht <- ifelse(SHT$last3droom_sht >= 7 , 7, SHT$last3droom_sht)
SHT$ordhour <- ifelse(SHT$ordhour <= 7 , 7, SHT$ordhour)
SHT$last30droom <- ifelse(SHT$last30droom >= 24 , 24, SHT$last30droom)
SHT$htltotalorders_7d <- ifelse(SHT$htltotalorders_7d >= 70 , 70, SHT$htltotalorders_7d)  #  同子酒店的订单量
# SHT$htlnoroomorders_7d <- ifelse(SHT$htlnoroomorders_7d >= 9 , 9, SHT$htlnoroomorders_7d)  #  同子酒店的无房订单量
SHT$pricerange <- ifelse(SHT$last30dmedianprice <=300, 1, 
                         ifelse(SHT$last30dmedianprice <=450,2,
                                ifelse(SHT$last30dmedianprice <=600,3
                                       ,ifelse(SHT$last30dmedianprice <=750,4,5))))
SHT$masterroompct <- SHT$mst_last30droom/SHT$hotelroomquantity

SHT$mst_last30dnoroom <- ifelse(SHT$mst_last30dnoroom>=8, 8, SHT$mst_last30dnoroom)

table(SHT$last30droom, SHT$isfullroom)

# # 查看每个变量的分布情况；
# summ <- cbind(total=apply(table(SHT$mst_last30dnoroom, SHT$isfullroom),FUN=sum, MARGIN = 1), table(SHT$mst_last30dnoroom, SHT$isfullroom))
# View(cbind(summ, summ[,3]/summ[,1]))

SHT[is.na(SHT)] <- -1         # # 把一些因子的NA变成 -1

# table(SHT$last3dnoroom_sht ,SHT$isfullroom)
View(tail(filter(SHT, isfullroom==1)))

#  提炼出测试集和训练集
set.seed(121)
roc = NULL
threaf <- seq(0.1:1,by =0.05)
k <- 10

# for(k in 1:length(threaf))
# {
trainsample <- rbinom(dim(SHT)[[1]], 1, threaf[k])==1
# split train & test result
SHTtrain <- SHT[trainsample,]
SHTtest <- SHT[!trainsample,]
# responsetrain <- filter(SHTtrain, isfullroom==1)
# responsetest <- filter(SHTtest, isfullroom==1)

# # GBM模型 
gbm.sht <- gbm(isfullroom ~
                  factor(ordprior)
                 +htlorders_60d
                 +shtnoroomrate_60d
                 +zonenoroomrate_3d
                 +factor(goldstar)
                 +factor(recommendlevel)         
                 +ordhour
                 +gintense
                 +star
                 +gcity
                 +gzone
                 +last3droom_sht
                 +cityi
                 +htlnoroomorders_7d   # 37.0360131
                 +zonestari
                 +zonei
                 +last7droom_sht
                 +intense
                 +last3droompct    # 同房型的入住率
                 +htlnoroomrate_7d
                 +htltotalorders_7d
                 +htlnoroomrate_60d  # 11.7423540
                 +noroomrate_60d 
                 +shtorders_60d      # 报错            
#                  +mst_last30dnoroom
#                  +mst_last30dnoroom_sht
#                  +mst_last30dnoroom_htl
#                  +mst_last30droom   #  同母酒店同入住天过去30天的房量；
#                 +pricerange     #  价格段；
#                 +masterroompct    #  过去30天同入住天同母酒店的房量占比
#                +level  # 这个数值有问题，不能factor               
#                +defaultrecommendlevel            
#                +last3droom
#                +last7droom
#                +factor(cprflag)
#                +factor(submitfrom)
#                +last30droom
#                +last30droom_sht
#                +orddays
#                +ordroomnum
#                +last3dmedianprice
#                +last3davgprice_sht
#                +last7dmedianprice
#                +last30dmedianprice
#                +last7davgprice_sht
#                +last30avgprice_sht
#                +roomquantity
#                +zonenoroomorders_3d
#                +hotelroomquantity
#                +zonetotalorders_3d
#                +totalorders_60d
#                +last3dnoroom_sht  # 剔除此变量，作为先验结果直接变成不能立即确认，同母酒店同母房型同入住天的结果；
              ,data = SHTtrain
              ,distribution = "adaboost"               
              ,n.trees = 1000               
              ,shrinkage = 0.02               
              ,interaction.depth = 3               
              ,bag.fraction = 0.5             
              ,train.fraction = 0.5              
              ,cv.folds = 3
              ,keep.data = FALSE
             # ,n.minobsinnode = 5
)

# summary(gbm.sht)
best.raw <- gbm.perf(gbm.sht, method='cv')
# Roc curve
SHTtest$pred <- predict(gbm.sht, newdata= SHTtest, n.trees = best.raw, type="response")
SHTtest$pred <- ifelse(SHTtest$htlnoroomorders_7d > 5, 1, SHTtest$pred)
SHTtest$pred <- ifelse(SHTtest$last7dnoroom >= 1, 1, SHTtest$pred)  # last7d无房的房量高于0
gbm.roc.area(SHTtest$isfullroom, SHTtest$pred)  # 0.7166724

# write.csv(SHTtest, file = "D:/project/【HTL】Offline虚拟保留房模型/供应商模型/sht.version.csv")
# misclass <- filter(SHTtest, isfullroom ==1, pred<=0.03)

# 指标评估  ,precision & recall的评估结果；
pred.gbm <- SHTtest$pred
accuracy = recall = specificity = sensitivity = NULL
cutoffPoint=seq(0.01,1,0.001)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred.gbm >=c1)
  specificity[i] <- sum((pred==1) & (SHTtest$isfullroom == 1))/sum(pred == 1)
  sensitivity[i] <- sum((pred==1) & (SHTtest$isfullroom==1))/sum(SHTtest$isfullroom==1)
  recall[i] <- sum((pred==0) & (SHTtest$isfullroom==0))/sum(SHTtest$isfullroom==0)
  accuracy[i] <- sum((pred==0) & (SHTtest$isfullroom==0))/sum(pred==0)
}
roc.result <- data.frame(cutoffPoint, accuracy, recall, specificity, sensitivity)
ggplot(data = roc.result)+geom_line(aes(x= recall , y=  accuracy))
View(roc.result)

