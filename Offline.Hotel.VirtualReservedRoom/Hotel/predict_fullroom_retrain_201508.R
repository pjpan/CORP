library(dplyr)
library(gbm)
library(glmnet)
library(ggplot2)
library(randomForest)

# setwd("/Users/ppj/Desktop/R/virtualroom")
setwd("/home/pjpan/virtualroom/DirectModelRetrain")
# source("./direct_function.R")

# 设置字符串不自动变成factor，导致出错
options(stringsAsFactors = F)
# rawdata <- read.table(file ="./virtualroomlog0803.txt", header = T, na.strings = "NULL", sep="\t", stringsAsFactors = F)
rawdata <- read.table(file ="./directmodel20150901.txt", header = T, na.strings = "NULL", sep="\t", stringsAsFactors = F)

# rm(list=ls())


# 对输入变量进行拆分；
parameter_HTL <- strsplit(filter(rawdata,hotelbelongto!='SHT')$modelinputparameter, ',')
rawbetalog_HTL <- as.data.frame(do.call(rbind,parameter_HTL),stringsAsFactors = F) #  设置字符串不为factor

# names(rawdata)
# View(head(rawbetalog_HTL))
names(rawbetalog_HTL) <- c('orderid',
                           'room_sumorder', 
                           'room_noroomsumorder',
                           'htl_noroomsumorder',
                           'htl_sumorder',
                           'roompct',
                           'room_last7dnoroom',
                           'room_last7dsumorder',
                           'room_last3dnoroom',
                           'room_last3dsumorder',
                           'htl_last7dnoroom',
                           'htl_last7dsumorder',
                           'htl_last3dnoroom',
                           'htl_last3dsumorder',
                           'hotelroomquantity',
                           'roomquantity',
                           'maxroomorder',
                           'maxhtlorder',
                           'maxroom_noroomsumorder',
                           'maxhtl_noroomsumorder',
                           'maxroomorder_r30d',
                           'maxhtlorder_r30d',
                           'htlroompct',
                           'roomnoroompct',
                           'htlnoroompct',
                           'htlsmtpct',
                           'roomhtlsmtpct',
                           'room_noroomrate',
                           'htl_noroomrate',
                           'room_last7doccupyrate',
                           'room_last7dnoroomrate',
                           'room_last3dnoroomrate',
                           'room_last3doccupyrate',
                           'htl_last7dnoroomrate',
                           'htl_last3dnoroomrate',
                           'last1ynoroomrate',
                           'masterhtlnoroomrate',
                           'maxroompct',
                           'maxroomnoroompct',
                           'maxhtlnoroompct',
                           'maxhtlroompct',
                           'maxroomhtlsmtpct',
                           'maxroompct_r30d',
                           'maxhtlroompct_r30d',
                           'maxroomhtlsmtpct_r30d',
                           'cityi',
                           'gcity',
                           'zonei',
                           'gzone',
                           'zonestari',
                           'intense',
                           'gintense',
                           'intenseprec',
                           'elongroomstatus',
                           'province',
                           'level',
                           'ordprior',
                           'ordroomnum',
                           'orddays',
                           'guarantee',
                           'recommendlevel',
                           'defaultrecommendlevel',
                           'balancetype',
                           'ordroomstatus',
                           'ordhour',
                           'isweekday',
                           'goldstar',
                           'cprflag',
                           'hotelbelongto',
                           'star',
                           'orddaysbefhol',
                           'orddaysafthol',
                           'avldaysbefhol',
                           'avldaysafthol',
                           'ispromotion',
                           'arrivalhour'  )


HTL_ALL <- cbind(rawbetalog_HTL, cancelreason = rawdata$cancelreason, vdate = rawdata$d.1)
# 因为bug，所以先用旧的数据替代
# HTL_ALL$ordprior<-rawdata$ordadvanceday  相差不大了；

# 新增目标变量；
HTL_ALL$isfullroom <- ifelse(HTL_ALL$cancelreason=='NOROOM',1,0)
rm(rawbetalog_HTL)
rm(rawdata)
rm(parameter_HTL)
gc()

# View(select(head(filter(HTL_ALL,ordprior2==ordprior),10),ordprior,ordprior2,orderid))

# 目前为止，没有出现pct>1的情况
# HTL_ALL$maxroompct_r30d <- ifelse(as.numeric(HTL_ALL$maxroomorder_r30d)/as.numeric(HTL_ALL$pred.roomqty) >= 1, 1, 
#                               as.numeric(HTL_ALL$maxroomorder_r30d)/as.numeric(HTL_ALL$pred.roomqty))
# HTL_ALL$maxhtlroompct_r30d <- ifelse(as.numeric(HTL_ALL$maxhtlroompct_r30d)>=1, 1, as.numeric(HTL_ALL$maxhtlroompct_r30d)) # 与maxhtlroompct相似度0.96
# HTL_ALL$maxhtlorder_r30d <- as.integer(HTL_ALL$maxhtlorder_r30d) # 与maxhtlorder相似度0.96
# cor(HTL_ALL$maxhtlroompct_r30d,HTL_ALL$maxhtlroompct) 0.8561396  相似性太高
# cor(HTL_ALL$maxhtlorder_r30d,HTL_ALL$maxhtlorder)  0.9615752  相似性太高
# cor(HTL_ALL$maxroomorder_r30d,HTL_ALL$maxroomorder) 0.9167416  相似性太高
# cor(HTL_ALL$maxroompct_r30d,HTL_ALL$maxroompct) 0.9167416 相似性太高
# cor(as.numeric(HTL_ALL$room_sumorder), as.numeric(HTL_ALL$room_last7dsumorder))  # 0.9650751 
# cor(as.numeric(HTL_ALL$room_last3dsumorder), as.numeric(HTL_ALL$room_last7dsumorder))  # 0.9678887 

# feature transform
HTL_ALL <- mutate(HTL_ALL
                  , cityi = as.numeric(cityi)
                  , gcity = as.numeric(gcity)
                  , zonei = as.numeric(zonei)
                  , gzone = as.numeric(gzone)
                  , zonestari = as.numeric(zonestari)
                  , intense = as.numeric(intense)
                  , elongroomstatus = as.integer(elongroomstatus)
                  , elongroomstatus = ifelse(elongroomstatus>0 , 1, ifelse(elongroomstatus<0, -1, elongroomstatus))
                  , int_orddays = as.integer(orddays)
                  , int_orddays = ifelse(int_orddays >= 3, 3, int_orddays)
                  , hotelroomquantity = ifelse(as.integer(hotelroomquantity)<0,0,as.integer(hotelroomquantity))   # 酒店的房间数
                #  , hotelroomquantity = log(as.integer(hotelroomquantity)+1)   # 酒店的房间数
                  , roomquantity = as.integer(roomquantity)  # 此基础房型的房间数
                  , room_sumorder = as.integer(room_sumorder)
                  , room_noroomsumorder = as.integer(room_noroomsumorder)
                  , htl_sumorder = as.integer(htl_sumorder)
                  , htl_noroomsumorder = as.integer(htl_noroomsumorder)                  
                  , pred.roomqty = ifelse(roomquantity <=0 
                                          ,ifelse(as.integer(maxroomorder_r30d)>0, as.integer(maxroomorder_r30d)
                                                  , as.integer(maxhtlorder_r30d)) 
                                          , roomquantity)  # 如果没有房型的房间数，用历史出现的最大房量来替代；
                  , int_recommendlevel= as.integer(recommendlevel)  # 推荐级别，与满房率负相关
                  , int_recommendlevel=ifelse(int_recommendlevel>=10,10,int_recommendlevel)
                  , int_ordroomnum = ifelse(as.integer(ordroomnum) >= 4, 4,as.integer(ordroomnum))                  
                  , ftr_cprflag = as.numeric(cprflag)
                  , pct_last1ynoroomrate = ifelse(as.numeric(last1ynoroomrate)>1, 1, as.numeric(last1ynoroomrate))
                  , pct_masterhtlnoroomrate = ifelse(as.numeric(masterhtlnoroomrate)>1, 1, as.numeric(masterhtlnoroomrate))
                  , pct_htlnoroompct = ifelse(as.numeric(htlnoroompct) >= 1, 1, as.numeric(htlnoroompct))
                  , pct_htlroompct = ifelse(as.numeric(htlroompct) >= 1,1 ,as.numeric(htlroompct))
                  , pct_htlsmtpct = as.integer(htl_sumorder)/(as.integer(hotelroomquantity)+0.001)
                  , pct_htlsmtpct = ifelse(pct_htlsmtpct>1, 1, pct_htlsmtpct)
                  , pct_htlsmtpct = ifelse(pct_htlsmtpct<0, -1, pct_htlsmtpct)
                  , pct_roompct = ifelse(as.numeric(room_sumorder)/(as.numeric(pred.roomqty)+0.001)>1, 1 ,as.numeric(room_sumorder)/(as.numeric(pred.roomqty)+0.001))
                  , pct_roompct = ifelse(pct_roompct>1, 1 , pct_roompct)
                  , pct_roompct = ifelse(pct_roompct<0, -1 , pct_roompct)
                  , pct_roomnoroomrate = as.numeric(room_noroomsumorder)/(as.numeric(room_sumorder)+0.001)  # 同基础房型满房在店房间数/总在店房间数
                  , pct_roomnoroomrate = ifelse(pct_roomnoroomrate>1,1,pct_roomnoroomrate)
                  , pct_htlnoroomrate = as.numeric(htl_noroomsumorder)/(as.numeric(htl_sumorder)+0.001) # 同子酒店在店房间数/总在店房间数
                  , pct_htlnoroomrate = ifelse(pct_htlnoroomrate>1,1,pct_htlnoroomrate)
                  , room_3_7 = (as.numeric(room_last3dsumorder) + as.numeric(room_last7dsumorder))/2        # 
                  , room_7_30 = (as.numeric(room_last7dsumorder) + as.numeric(room_sumorder))/2             # 
                  , hotel_7_30 = (as.numeric(htl_last7dsumorder) + as.numeric(htl_sumorder))/2            # 
                  , hotel_3_7 = (as.numeric(htl_last7dsumorder) + as.numeric(htl_last3dsumorder))/2       # 
                  , pct_roomfullrate = as.integer(room_sumorder)/(as.integer(maxroomorder)+0.001)
                  , pct_roomfullrate = ifelse(pct_roomfullrate>1,1,ifelse(pct_roomfullrate<0,-1,pct_roomfullrate))  # 超过1看做1，没有历史满房最大值
                  , pct_htlfullrate = as.integer(htl_sumorder)/(as.integer(maxhtlorder)+0.001)
                  , pct_htlfullrate = ifelse(pct_htlfullrate>1,-1,pct_htlfullrate)  # 如果没有酒店房间数，这会超过1,，看做1
                  , pct_roomhtlsmtpct = as.integer(room_sumorder)/(as.integer(htl_sumorder)+0.001)
                  , pct_roomhtlsmtpct = ifelse(pct_roomhtlsmtpct>1,1,pct_roomhtlsmtpct)
                  , pct_roomhtlsmtpct = ifelse(pct_roomhtlsmtpct<0,-1,pct_roomhtlsmtpct)
                  , ftr_arrivalhour = ifelse(as.integer(arrivalhour)<= 18, 18 , as.integer(arrivalhour))  #  factor
                  , ftr_level = as.integer(level)
                  , int_ordhour = as.integer(ordhour)
                  , int_ordhour = ifelse(int_ordhour<=6, 6, int_ordhour)
                  , ftr_isweekday = as.integer(isweekday)
                  , ftr_goldstar = as.integer(goldstar)
                  , ftr_star = as.integer(star)
                  , int_ordprior = ifelse(as.integer(ordprior)>=60, 61, as.integer(ordprior))
                  , pct_room_last3doccupyrate = ifelse(as.numeric(room_last3dsumorder)/(as.numeric(pred.roomqty)+0.001)>=1, 1 
                                                       ,as.numeric(room_last3dsumorder)/(as.numeric(pred.roomqty)+0.001))
                  , pct_room_last3doccupyrate = ifelse(pct_room_last3doccupyrate<0, -1, pct_room_last3doccupyrate)
                  , pct_room_last7doccupyrate = ifelse(as.numeric(room_last7dsumorder)/(as.numeric(pred.roomqty)+0.001)>=1, 1 
                                                       ,as.numeric(room_last7dsumorder)/(as.numeric(pred.roomqty)+0.001))
                  , pct_room_last7doccupyrate = ifelse(pct_room_last7doccupyrate<0, -1, pct_room_last7doccupyrate)
                  , bin_isoverroommax = ifelse((as.integer(room_sumorder)+as.integer(ordroomnum)) >= HTL_ALL$maxroomorder, 1, 0)
                  , bin_isoverhtlmax = ifelse(as.integer(htl_sumorder) > as.integer(maxhtlorder), 1, 0)
                  # 满房临界点进行预处理；
                  , maxroomorder = as.integer(maxroomorder)
                  , maxroom_noroomsumorder = as.integer(maxroom_noroomsumorder)
                  , maxroompct = ifelse(as.numeric(maxroomorder)/(as.numeric(pred.roomqty)+0.001) >=1, 1, as.numeric(maxroomorder)/(as.numeric(pred.roomqty)+0.001))
                  , maxhtlorder = as.integer(maxhtlorder)
                  , maxhtl_noroomsumorder = as.integer(maxhtl_noroomsumorder)
                  , maxhtlpct = ifelse(as.integer(maxhtlorder)/(hotelroomquantity+0.001)>=1, 1, as.integer(maxhtlorder)/(hotelroomquantity)+0.001)
                  , maxhtlpct = ifelse(maxhtlpct<0, -1, maxhtlpct)
                  , maxhtlroompct = ifelse(as.numeric(maxhtlroompct)>=1, 1, as.numeric(maxhtlroompct))
                  , maxroomnoroompct = as.numeric(maxroomnoroompct)
                  , maxroomnoroompct = ifelse(maxroomnoroompct>1, 1, maxroomnoroompct)
                  , maxhtlnoroompct = as.numeric(maxhtlnoroompct)
                  , maxhtlnoroompct = ifelse(maxhtlnoroompct>1, 1, maxhtlnoroompct)
                  , maxroomhtlsmtpct = as.numeric(maxroomhtlsmtpct)
                  , maxroomhtlsmtpct = ifelse(maxroomhtlsmtpct>1, 1, maxroomhtlsmtpct)
                  , maxroomorder_r30d =as.integer(maxroomorder_r30d)
                  , maxhtlorder_r30d = as.integer(maxhtlorder_r30d)
                  , ispromotion = as.integer(ispromotion)
                  , avldaysbefhol = as.integer(avldaysbefhol)
                  , avldaysafthol = as.integer(avldaysafthol)
)


# 查看所有变量的直方图
# d <- reshape2::melt(HTL_ALL[,-c(1)])
# ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
# rm(d)
# gc(d)


# 查看一些变量之间的分布结果；
# ggplot(data=HTL_ALL)+geom_point(aes(x=ordprior,y=cprflag,colour=factor(isfullroom)))
# ggplot(data=HTL_ALL)+geom_density(aes(x=masterhtlnoroomrate,color=factor(isfullroom)))
# ggplot(data=HTL_ALL)+geom_density(aes(x=last1ynoroomrate,color=factor(isfullroom)))
# ggplot(data=HTL_ALL)+geom_density(aes(x=roomnoroompct,color=factor(isfullroom)))
# ggplot(data=HTL_ALL)+geom_density(aes(x=room_noroomsumorder,color=factor(isfullroom)))
# ggplot(data=HTL_ALL)+geom_density(aes(x=htlroompct,color=factor(isfullroom)))
# ggplot(data=HTL_ALL)+geom_density(aes(x=roompct,color=factor(isfullroom)))


# variable importance  & selection
# formula <- paste0(names(HTL_ALL),collapse = "+")

# ranomforest的公式
# HTL_ALL$isfullroom <- as.factor(HTL_ALL$isfullroom) # 设置目标变量为factor
HTL_ALL[is.na(HTL_ALL)] <- -1

# 针对低星级的订单进行预测，star<=3,人工过滤了部分边栏，保留了56个变量进行预测；
# 如果同基础房型已经出现过了满房，则立即标记成不能立即确认；
HTL_lowstar <- select(filter(HTL_ALL ,star<=3 ,ordprior>=0)
                      ,room_sumorder
                      ,room_noroomsumorder
                      ,htl_noroomsumorder
                      ,htl_sumorder
                      ,hotelroomquantity
                      ,maxroomorder
                      ,maxhtlorder
                      ,maxroom_noroomsumorder
                      ,maxhtl_noroomsumorder
                      ,maxroompct
                      ,maxroomnoroompct
                      ,maxhtlnoroompct
                      ,maxhtlroompct
                      ,maxroomhtlsmtpct
                      ,cityi
                      ,gcity
                      ,zonei
                      ,gzone
                      ,zonestari
                      ,intense
                      ,elongroomstatus
                      ,pred.roomqty
                      ,maxhtlpct
                      ,int_orddays
                      ,int_recommendlevel
                      ,pct_last1ynoroomrate
                      ,pct_masterhtlnoroomrate
                      ,int_ordroomnum
                      ,pct_htlnoroompct
                      ,pct_htlroompct
                      ,pct_htlsmtpct
                      ,pct_roompct
                      ,pct_roomnoroomrate
                      ,pct_htlnoroomrate
                      ,room_3_7
                      ,room_7_30
                      ,hotel_7_30
                      ,hotel_3_7
                      ,pct_roomfullrate
                      ,pct_htlfullrate
                      ,pct_roomhtlsmtpct
                      # ,ftr_arrivalhour
                      ,ftr_level
                      ,int_ordhour
                      ,ftr_goldstar
                      ,ftr_star
                      ,ftr_cprflag
                      ,ftr_isweekday
                      ,int_ordprior
                      ,pct_room_last3doccupyrate
                      ,pct_room_last7doccupyrate
                      ,bin_isoverroommax
                      ,bin_isoverhtlmax
                      ,avldaysbefhol
                      ,avldaysafthol
                      ,isfullroom
                      )

#  校验每个变量的数值结果是否正确，是否出现了NA/NAN/INF数值；
vars <- names(HTL_lowstar)[c(1:52,54:66)]
yvars <- "isfullroom"

aa <- list()
for(i in 1:length(vars))
{
  aa[[i]] <- rbind(key=vars[i],value=eval(parse(text = paste0("summary(HTL_lowstar$",vars[i],")"))))
  print(i)
}
aa

# 箱形图，查看变量之间的分布关系；
# ggplot(data=HTL_lowstar,aes(x=isfullroom,y=int_ordprior))+geom_boxplot()
# ggplot(data=HTL_lowstar,aes(x=isfullroom,y=maxhtlpct))+geom_boxplot()
# ggplot(data=HTL_lowstar,aes(x=isfullroom,y=maxroompct))+geom_boxplot()
# ggplot(data=HTL_lowstar,aes(x=ftr_arrivalhour,y=isfullroom))+geom_boxplot()

# 提炼出测试集和训练集合；
train <- rbinom(dim(HTL_lowstar)[[1]],size=1 ,prob = 0.8)==1
HTL_lowstar_train <- HTL_lowstar[train,]
HTL_lowstar_test <- HTL_lowstar[!train,]

# 查看所有变量的直方图
# d <- reshape2::melt(HTL_lowstar[,-c(1,56)])
# ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
# rm(d)
# gc(d)

rf.formula <- paste0(yvars,"~", paste0(vars,collapse="+"))
# 用randomforest来选取变量；
rfmodel <- randomForest(as.formula(rf.formula)                   
                        ,data=HTL_lowstar_train
                        ,ntree=1000
                        ,keep.forest=F
                        ,proximity=F
                        ,importance=T
                        ,na.action= na.exclude
                        ,keep
)

# 查看每个变脸的重要性
varImpPlot(rfmodel,n.var = 29)
imp <- importance(rfmodel)
ordervar <- imp[order(imp[, 2], decreasing = TRUE),]
cbind(var=ordervar[,0],mean = cummean(ordervar[,2]))
ggplot()+geom_line(aes(x=1:length(ordervar[,1]), y=cummean(ordervar[,2]))) +ggtitle(label = "每个变量的重要性")

# 预测结果
HTL_lowstar_test$pred <- predict(rfmodel, HTL_lowstar_test)

# GBM 预测,利用randomforest 挑选优质的变量
direct.gbmmodel <- gbm(isfullroom ~ 
                 pct_masterhtlnoroomrate
                +pct_last1ynoroomrate
                +htl_noroomsumorder
                +pct_htlnoroomrate
                +pct_roomnoroomrate
                +pct_htlfullrate
                +pct_htlroompct
                +pct_roompct                
                +maxhtlpct
                +maxhtlorder                
                +maxroomorder                
                +maxhtl_noroomsumorder  # 后面新增的变量
                +maxhtlpct
                +pct_htlsmtpct
                +factor(int_ordhour)
                +int_ordprior
                +factor(int_recommendlevel)  # 不能变成factor，会导致稀疏                
                +cityi  # 权重最重要
                +gcity 
                +intense
                +zonei   # 紧张度重要性次之
                +gzone
                +zonestari
                +room_noroomsumorder
                +hotel_3_7               # 后来的新增变量
                +hotelroomquantity
                +pred.roomqty
                +factor(int_ordroomnum)
                +factor(int_orddays)
                +factor(ftr_goldstar)
                , data = HTL_lowstar_train 
                , n.trees = 3500
                , interaction.depth = 3
                , shrinkage = 0.01
                , distribution = 'adaboost'
                , cv.folds = 3
                , bag.fraction = 0.6       
                , train.fraction= 0.5
                , keep.data = F
)

# 查看变量的结果
summary(direct.gbmmodel)
direct.ntrees <- gbm.perf(direct.gbmmodel, plot.it = T,method = 'cv', oobag.curve=F)
plot.gbm(gbmmodel, i.var= 17, n.trees= direct.ntrees)

ggplot()+geom_density(data = filter(HTL_lowstar),aes(x= int_ordprior ))+facet_grid(.~ isfullroom)

# predict testdata
HTL_lowstar_test$pred <- predict(gbmmodel, newdata = HTL_lowstar_test, type="response",n.trees = direct.ntrees)
obj <- filter(HTL_lowstar_test, ftr_star==0)$isfullroom
pred.gbm <- filter(HTL_lowstar_test, ftr_star==0)$pred
summary(pred.gbm)
#查看分布图
ggplot(data = HTL_lowstar_test)+geom_density(aes(x= pred , color=factor(isfullroom)))
gbm.roc.area(obj,pred.gbm) # 0.7758259
accuracy = recall = specificity = sensitivity = NULL
cutoffPoint=seq(0.01,1,0.002)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred.gbm >=c1)
  specificity[i] <- sum((pred==1) & (obj == 1))/sum(pred == 1)
  sensitivity[i] <- sum((pred==1) & (obj==1))/sum(obj==1)
  recall[i] <- sum((pred==0) & (obj==0))/sum(obj==0)
  accuracy[i] <- sum((pred==0) & (obj==0))/sum(pred==0)
}
roc.result <- data.frame(cutoffPoint, accuracy, recall, specificity, sensitivity) # precision = 98.1%  recall = 29.8%
View(roc.result)
p <- ggplot(data = roc.result)+geom_line(aes(x= recall , y=  accuracy))
p <- p+theme(axis.title.x = element_text(face="bold", size=15))
p <- p+theme(axis.title.y = element_text(face="bold", size=15))
p <- p+ggtitle("准确率&召回率走势图")+theme(title = element_text(face="bold", size=15))+geom_abline(intercept=0.99 , slope=0 ,color='red')
p

# 查看误判的订单明细结果；
HTL_lowstar_test <- mutate(HTL_lowstar_test, pred.isfullroom = pred<=0.024)


# 把模型存储下来；
save(direct.gbmmodel, direct.ntrees, file = './ReservedRoomV20150908.RData')


# # logistic regression with L2
# x <- as.matrix(HTL_lowstar_train[,c(vars)])
# y <- HTL_lowstar_train$isfullroom
# lrmodel <- glmnet(x, y, alpha=0, lambda=0.001, family="binomial")
# 
# lr.model <- glmnet(as.matrix(HTL_lowstar_train[,c("pct_last1ynoroomrate","pct_masterhtlnoroomrate","int_recommendlevel","pct_roomfullrate","pct_htlfullrate","zonestari","intense")]) , HTL_lowstar_train$isfullroom ,alpha=0, lambda=0.001, family="binomial")
# lr.model2 <- glmnet(as.matrix(HTL_lowstar_train[,c("pct_last1ynoroomrate","pct_masterhtlnoroomrate")]) , HTL_lowstar_train$isfullroom ,alpha=0, lambda=0.001, family="binomial")
# lr.model$nulldev
# lr.model2$nulldev
# coef(lr.model)
# coef(lr.model2)
# 
# deviance(lr.model)
# 
# model =glm.fit(xframe[,active], y, family=binomial(link="logit"))
# 
# modelparams = StepWiseRidge(HTL_lowstar_train, vars, yvars)
# current_vars=c()
# candidate_vars = vars
# devs = numeric(length(vars))
# improvement = numeric(length(vars))
# current_dev=DeafultDeviance(HTL_lowstar_train[[yvars]])
# do_continue=TRUE
# min_improve=1e-6
# while(do_continue) {
#   iter = AddVar(HTL_lowstar_train, HTL_lowstar_train[[yvars]], current_vars, current_dev, candidate_vars)
#   current_vars = iter$current_vars
#   current_dev = iter$current_dev
#   count = length(current_vars)
#   devs[count] = current_dev
#   improvement[count] = iter$improvement
#   candidate_vars = setdiff(vars, current_vars)
#   #  print(current_vars)
#   do_continue = (length(candidate_vars) > 0) && (iter$improvement > min_improve)
# }


