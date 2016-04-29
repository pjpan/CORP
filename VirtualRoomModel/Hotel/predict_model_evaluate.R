library(gbm)
library(rJava)
library(RJDBC)
library(ggplot2)
library(dplyr)

##########Connect Hive##############

options(stringsAsFactors = FALSE)

# f1 <- list.files(paste("D:/project/【HTL】数据质量校验/HTLDataQualityMonitor/OlapMonitor/lib", sep=''),pattern="*jar$",full.names=T)
# jdbc <- JDBC("org.apache.hive.jdbc.HiveDriver",f1)
# # hiveserver <- c("jdbc:hive2://10.8.75.3:10000")
# hiveserver <- c("jdbc:hive2://192.168.63.8:10000")
# 
# hiveconn <- dbConnect(jdbc, hiveserver)

# sql <- paste("select * from tmp_htlquerydb.ppj_tmp_orderconfirmmodel where orderdate>='2014-10-01' and orderdate<='2014-10-31' ",sep='')

# sql <- paste("select * from tmp_htlquerydb.ppj_tmp_htlnoroomcriteria")
# 
# res = dbSendQuery(hiveconn, sql)
# totallresult <- fetch(res, n=-1)

# totalresult <- read.csv(file = "D:/project/【HTL】订单即时确认模型/模型/htltest0925_1011.txt" ,sep ='\t')

# htlrmcriteria <- read.csv(file = "D:/project/【HTL】订单即时确认模型/模型/htlrmcriteria.txt" ,sep ='\t')

# htltest <- read.csv(file = "D:/project/【HTL】Offline虚拟保留房模型/模型/data/htlnewtest01.txt" ,sep ='\t')
htltest <- read.csv(file = "D:/project/【HTL】Offline虚拟保留房模型/模型/data/htltrain01_05.txt" ,sep ='\t') # 1.1~1.5 新数据结果

datelist <- unique(htltest$d)

filter(htltest, hotelbelongto =='SHT') %>% group_by(d) %>% summarise(count = n())

# for(j in 1:length(datelist))
# {
# shtaccuracy=shtrecall=roc=NULL  
objectdate <- '2015-01-02'
test_simu <- filter(htltest, d == objectdate) # 只看HTL的结果

# 变量转换
test_simu$hotelroomquantity_log <- log(test_simu$hotelroomquantity, 10)  # log房间数，10个房间 =1 个房间
test_simu$roomquantity_log <- log(test_simu$roomquantity, 10) 
test_simu$last1ynoroomrate_trans <- as.numeric(test_simu$last1ynoroomrate)
test_simu$totalroompct <- as.numeric(test_simu$totalroompct)
test_simu$noroompct <- as.numeric(test_simu$noroompct)
test_simu$htlsmtpct <- as.numeric(test_simu$htlsmtpct)
test_simu$room_last14doccupyrate_trans <- as.numeric(test_simu$room_last14doccupyrate)
test_simu$room_last7doccupyrate_trans <- as.numeric(test_simu$room_last7doccupyrate)
test_simu$room_last3doccupyrate_trans <- as.numeric(test_simu$room_last3doccupyrate)
test_simu$room_last1doccupyrate_trans <- as.numeric(test_simu$room_last1doccupyrate)
test_simu$isurgency <- ifelse((test_simu$ordhour>= 21 | test_simu$ordhour<7 ) & test_simu$ordprior==0 ,1,0)
# test_simu$ordhour_new <- ifelse(test_simu$ordhour>=18 | test_simu$ordhour<=7,-1,test_simu$ordhour) # 21~7 晚上时间
test_simu$maxroomorder <- as.numeric(test_simu$maxroomorder)
test_simu$maxhtlorder <- as.numeric(test_simu$maxhtlorder)
test_simu$maxsmthtlpct <- as.numeric(test_simu$maxsmthtlpct)
test_simu$maxsmtroompct <- as.numeric(test_simu$maxsmtroompct)
# transform metrics
test_simu$ordroomnum <- ifelse(test_simu$ordroomnum >= 6, -1,test_simu$ordroomnum)  # 超过6的归结为一类，与其他的category来进行平衡；
test_simu$orddays <- ifelse(test_simu$orddays_trans >= 3, -1, test_simu$orddays_trans)  # 超过3的归结为一类，与其他的category来进行平衡；
test_simu$isoverhtlsmt <- ifelse(test_simu$htlsmtpct >= test_simu$maxsmthtlpct,1,0)
test_simu$isoverroomsmt <- ifelse(test_simu$totalroompct>=test_simu$maxsmtroompct,1,0)
test_simu$guarantee <- ifelse(test_simu$guarantee=='@' , 'N' , test_simu$guarantee) # @的量很少

test_simu$cprflag <- ifelse(test_simu$cprflag >=3 | is.null(test_simu$cprflag), -1 , test_simu$cprflag) # 平衡数据量值

test_simu$arrivalhour <- ifelse(test_simu$arrivalhour <= 17, 1 , test_simu$arrivalhour) # 最晚的到点时间一般性是从18:00开始的；

test_simu$recommendlevel <- ifelse(test_simu$recommendlevel>= 10 | test_simu$recommendlevel <= 0 | is.na(test_simu$recommendlevel), -1 
                             , test_simu$recommendlevel)  #  超过10 和小于0的看做一类别，跟其他类别的量级持平；

test_simu$level <- ifelse(test_simu$level>= 3 & test_simu$level <=5, -2, test_simu$level)  # 中间档的酒店订单量太小，合并

test_simu$issht <- ifelse(test_simu$hotelbelongto != 'SHT'| test_simu$hotelbelongto != 'HPP', 1 , 0)  # 是否供应商

# 转化预订距离节假日的天数， 以7 作为一档，防止新的数据集合没有相关的类别；
test_simu$daysbefhol <- ifelse(test_simu$orddaysbefhol>=1 &　test_simu$orddaysbefhol<=7,7
                         ,ifelse(test_simu$orddaysbefhol>7 & test_simu$orddaysbefhol <=14,14
                                 ,ifelse(test_simu$orddaysbefhol>14 & test_simu$orddaysbefhol <=21,21
                                         ,ifelse(test_simu$orddaysbefhol>21 & test_simu$orddaysbefhol <=35,30,test_simu$orddaysbefhol
                                         ))))

test_simu$daysafthol <- ifelse(test_simu$orddaysafthol>=1 &　test_simu$orddaysafthol<=7,7
                         ,ifelse(test_simu$orddaysafthol>7 & test_simu$orddaysafthol <=14,14
                                 ,ifelse(test_simu$orddaysafthol>14 & test_simu$orddaysafthol <=21,21
                                         ,ifelse(test_simu$orddaysafthol>21 & test_simu$orddaysafthol <=35,30,test_simu$orddaysafthol
                                         ))))

test_simu$avlbefhol <- ifelse(test_simu$avldaysbefhol>=1 &　test_simu$avldaysbefhol<=7,7
                        ,ifelse(test_simu$avldaysbefhol>7 & test_simu$avldaysbefhol <=14,14
                                ,ifelse(test_simu$avldaysbefhol>14 & test_simu$avldaysbefhol <=21,21
                                        ,ifelse(test_simu$avldaysbefhol>21 & test_simu$avldaysbefhol <=35,30,test_simu$avldaysbefhol
                                        ))))

test_simu$avlafthol <- ifelse(test_simu$avldaysafthol>=1 &　test_simu$avldaysafthol<=7,7
                        ,ifelse(test_simu$avldaysafthol>7 & test_simu$avldaysafthol <=14,14
                                ,ifelse(test_simu$avldaysafthol>14 & test_simu$avldaysafthol <=21,21
                                        ,ifelse(test_simu$avldaysafthol>21 & test_simu$avldaysafthol <=35,30,test_simu$avldaysafthol
                                        ))))


test_simu$ordprior <- ifelse(test_simu$ordprior>=1 &　test_simu$ordprior<=7,7
                       ,ifelse(test_simu$ordprior>7 & test_simu$ordprior <=14,14
                               ,ifelse(test_simu$ordprior>14 & test_simu$ordprior <=21,21
                                       ,ifelse(test_simu$ordprior>21 & test_simu$ordprior <=30,30,
                                               ifelse(test_simu$ordprior >30, 35, test_simu$ordprior)
                                       ))))



# 预测最后的的结果
test_simu$pred.fullroom <- predict(raw.gbm, newdata = test_simu, type='response')
gbm.roc.area(test_simu$isfullroom, test_simu$pred.fullroom)  # 0.8360774  # 0.8543391  # 0.6137155   1.2日效果奇差；

# 导出相应的结果
write.csv(test_simu, file ="D:/project/【HTL】Offline虚拟保留房模型/模型/test/gbm.adaboost.0213.test.csv",row.names = FALSE)

# 第二次测试集的结果
accuracy=recall=specificity=sensitivity=NULL

cutoffPoint=seq(0.0005,0.50,0.0005)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred.lr.test2 >=c1)
  pred[is.na(pred)] <- 0
  # accuracy[i] <- sum((pred==1) & (HTLtest$isfullroom == 1))/sum(pred == 1)
  #  recall[i] <- sum((pred==1) & (HTLtest$isfullroom==1))/sum(HTLtest$isfullroom==1)
  recall[i] <- sum((pred==0) & (test_simu$isfullroom==0))/sum(test_simu$isfullroom==0)  
  # accuracy[i] <- 1-sum((pred==0) & (test_simu$isfullroom==1))/sum((pred==0) & (test_simu$isfullroom==0))
  accuracy[i] <- sum((pred==0) & (test_simu$isfullroom==0))/sum(pred==0)
}
resultPred.test2 <- data.frame(cutoffPoint, accuracy, recall)

View(resultPred.test2 )

system.time(predict(raw.gbm, newdata = test_simu[1:500,], type='response'))
nrow(test_simu)

View(resultPred.test2)
summary(pred.lr.test2)

ggplot(data = resultPred.test2, aes(x= recall , y = accuracy)) + geom_line()+xlab("recall")+ylab("准确率")

p <- ggplot(data= resultPred.test2, aes(x=recall, y=accuracy))
p+geom_line()+xlab("召回率")+ylab("准确率") + ggtitle("09.26日测试数据结果" )








