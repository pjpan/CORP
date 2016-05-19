library(dplyr)
library(gbm)
library(data.table)
library(ggplot2)
library(readr)
library(xgboost)
# load("d:/Users/pjpan/Desktop/Xdat_force.RData")
Xdat <- fread(input = "xdata_force_20160518.txt"
              , na.strings = "NULL"
              , header = T
              , stringsAsFactors = F
              , data.table = F
              , integer64 = "character")

# 把房量等数据变成整形；
for(i in 30:78){
  Xdat[, i] <- as.integer(Xdat[, i])  
}

# 把紧张度变成数值型；
for(j in 79:84){
  Xdat[,j] <- as.numeric(Xdat[, j])  
}

# 
# Xdat %>% count(star, hotelbelongto, isnoroom)%>%mutate(pct =n/sum(n)) -> htlbelongstarstat
# Xdat %>% count(hotelbelongto,isnoroom)%>%mutate(pct =n/sum(n))
# Xdat %>% count(ord_hour,isnoroom)%>%mutate(pct =n/sum(n))

# saveGIF({
#   for (i in 6:12) {
#       
#       cat(names(Xdat)[i],"-",i, "-class type is " ,class(Xdat[,i]), "\n")
#       m <- ggplot(Xdat, aes(x = names(Xdat)[i]))
#       m <- m + ggtitle(paste("Each Feature Histogram -", names(Xdat)[i]))
#       m <- m + geom_density(alpha=.5, fill = "gray")
#       m <- m + xlim(c(-40, 50))
#       m <- m + ylim(c(0, 0.1))
#       m <- m + geom_vline(aes(xintercept=mean(Xdat[,i], na.rm=T)),   # Ignore NA values for mean
#                           color="red", linetype="dashed", size=1)
#       m <- m + geom_vline(aes(xintercept=median(Xdat[,i], na.rm=T)),   # Ignore NA values for mean
#                           color="blue", linetype="dashed", size=1)
#     
#       print(m)    
#   }
# }, interval = 0.5, movie.name = "tempDensity.gif", ani.width = 1024, ani.height = 860)


# 创建新的变量
Xdat <- mutate(Xdat, 
               # 母基础房型维度；
               mbr_fullrate_2w = mbr_full_ordnum_2w/(mbr_ordnum_2w+0.0001) # 母基础房型最近两周的满房率； 
               ,mbr_fullrate_force_2w = mbr_force_full_ordnum_2w/(mbr_force_ordnum_2w+0.001)
               # mbr_occupiedrate_2w = mbr_full_ordnum_2w/(mbr_max_rmqty_180d+0.001)
               # 母酒店维度
               ,mhtl_fullrate_2w = mhtl_full_ordnum_2w/(mhtl_ordnum_2w+0.001)
               ,mhtl_fullrate_force_2w = mhtl_force_full_ordnum_2w/(mhtl_force_ordnum_2w+0.001)
               # mhtl_occupiedrate_2w = mhtl_ordnum_2w/(mhtl_max_rmqty_180d+0.001),
               
               #细分到同一入住天的母基础房型维度；
               ,mbr_occupied_efdt2w = mbr_ordnum_efdt2w/(mbr_max_rmqty_180d+0.001)  #  报错
               ,mhtl_occupied_efdt2w = mhtl_ordnum_efdt2w/(mhtl_max_rmqty_180d+0.001)
               ,hotelbelongto = ifelse(hotelbelongto!="HTL" | hotelbelongto!='HPP', "OHTER", hotelbelongto)  # 维度稀疏性；
               )

# 把训练数据存储一下；
save(Xdat, file = "xdata_force.Rdata")
load(file = "xdata_force.Rdata")

# 区分两个模型，一个是历史上出现过强下马甲房型的数据；>=1，大概占比 40%；
Xdat%>%count(mhtl_full_ordnum_efdt2w>=1)%>%mutate(pct = n/sum(n))

# 看一下大概的订单占比
# mhtl_full_ordnum_efdt2w >= 1      n        pct
# (lgl)  (int)      (dbl)
# 1                        FALSE 231429 0.53538314
# 2                         TRUE 168784 0.39046147
# 3                           NA  32055 0.07415539
# mhtl_full_ordnum_efdt2w >= 1 isnoroom == 1      n       pct
# (lgl)         (lgl)  (int)     (dbl)
# 1                        FALSE         FALSE 168138 0.7265209
# 2                        FALSE          TRUE  63291 0.2734791
# 3                         TRUE         FALSE  94936 0.5624704
# 4                         TRUE          TRUE  73848 0.4375296
# 5                           NA         FALSE  23058 0.7193262
# 6                           NA          TRUE   8997 0.2806738

# 批量替换NA/NAN/INF为-1
Xdat[apply(is.na(Xdat), FUN = any, 1), ] = -1

# 过滤数据，把历史上同一个酒店出现过满房的房型及订单数据拿出来；
Xdat_highrisk <- filter(Xdat, mhtl_full_ordnum_efdt2w>=1)  # 同入住天同子酒店出现过满房的数据
Xdat_lowrisk <- filter(Xdat, mhtl_full_ordnum_efdt2w<1)   

# 对高风险的记录进行划分训练集和测试集；
train_high <- caret::createDataPartition(Xdat_highrisk$isnoroom, p = 0.7, list =F)
Xdat_highrisk_train <- Xdat_highrisk[train_high, ]  # 训练集
Xdat_highrisk_test <- Xdat_highrisk[-train_high, ]  #  测试集合

# 
ggplot(data = Xdat)+geom_density(aes(mbr_occupied_efdt2w ,fill = isnoroom))
summary(Xdat$mhtl_occupied_efdt2w)

Xdat_highrisk%>%arrange(desc(mhtl_occupied_efdt2w))%>%head()

# 训练模型；
gbm_model <- gbm(isnoroom~ 
                  mbasicroomi
                 +mhoteli
                 +roomi
                 +cityi
                 +zonei
                 +zonestari
                 +mbr_fullrate_2w
                 +mbr_fullrate_force_2w
                 +mhtl_fullrate_2w
                 +mhtl_fullrate_force_2w
                 +mbr_occupied_efdt2w
                 +mhtl_occupied_efdt2w
                  +factor(star)
#                  +factor(cprflag)
                  +factor(ordadvanceday2)
#                  +factor(goldstar)
#                  +factor(dayofweek)
#                  +factor(hotelbelongto)
                 , data = Xdat_highrisk_train 
                 , n.trees = 1500
                 , interaction.depth = 3
                 , shrinkage = 0.02
                 , distribution = 'adaboost'
                 , cv.folds = 3
                 , bag.fraction = 0.6       
                 , train.fraction= 0.5
                 , keep.data = F
)

# 查看模型结果；
summary(gbm_model)
gbm_model_bestiter <- gbm.perf(gbm_model, plot.it = T,method = 'cv', oobag.curve = F)

# 查看测试集合的预测效果
pred_gbm <- Xdat_highrisk_test$pred <- predict(gbm_model, newdata = Xdat_highrisk_test, type="response",n.trees = gbm_model_bestiter)
summary(pred_gbm)
obj <- Xdat_highrisk_test$isnoroom
gbm.roc.area(obj, pred_gbm)  # 0.8007468

# 查看PR曲线；
precision = recall = NULL
cutoffPoint=seq(0.01,1,0.002)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred_gbm >=c1)
  precision[i] <- sum((pred==1) & (obj == 1))/sum(pred == 1)
  recall[i] <- sum((pred==1) & (obj==1))/sum(obj==1)
}

 # 存储precision & recall
PR_result <- data.frame(cutoffPoint, precision, recall) 

# 画出PR曲线；
ggplot(data = PR_result)+geom_point(aes(x = recall, y = precision))















