
#  加载数据；
setwd('D:/gitcode/CORP/OP_fashionicon')
cat("load the data")
eid_info <- data.table::fread(input = './data/emp_info.csv', na.strings = c("NULL", "NA", NA))
uid_info <- data.table::fread(input = './data/uid_info.csv', na.strings = c("NULL", "NA", NA))
uid_op_services <- data.table::fread(input = './data/uid_op_services.csv', na.strings = c("NULL", "NA", NA))

#  deal eid_info :age,name,
library(dplyr)
library(data.table)
library(plyr)
options(stringsAsFactors = F)

# set the keys
setkey(eid_info, Eid)
setkey(uid_info, uid)
setkey(uid_op_services, uid, eid)

# 给每个uid匹配上相同数量个EID数量；
uid_transform_total <- NULL
for(i in 1:nrow(uid_info)){
  num_uid <- rep(uid_info$uid[i], nrow(eid_info))
  num_complaintimes <- rep(uid_info$complaintimes[i], nrow(eid_info))
  num_grade <- rep(uid_info$grade[i], nrow(eid_info))
  num_gender <- rep(uid_info$gender[i], nrow(eid_info))
  num_age <- rep(uid_info$age[i], nrow(eid_info))
  num_isboss <- rep(uid_info$isboss[i], nrow(eid_info))
  uid_transform <- as.data.frame(cbind(num_uid, num_complaintimes, num_grade, num_gender, num_age, num_isboss))  
  uid_transform_total <<- rbind(uid_transform,uid_transform_total)  
}

rm(num_uid, num_complaintimes, num_grade, num_gender, num_age, num_isboss, uid_transform)

cat('deal eid infomation')
# 计算一下网红的年龄，性别，公司司龄
library(lubridate)
eid_info$age <- as.integer(round(difftime(time2 = as.Date(eid_info$Birthday), time1 = Sys.time(), units = "weeks")/52,0))
eid_info$worktime <- as.integer(round(difftime(time2 = as.Date(eid_info$EntryTime), time1 = Sys.time(), units = "weeks")/52,0))

# 给不同维度进行赋权；
weight_isgendermatch <- c(-0.5) # 性别是否匹配
weight_zodiacuid <- c(-0.2)  # 成为专属客服次数
weight_workexperience <- c(0.2) # eid工作年限
weight_avgscore <- c(0.2)       # 网红的平均得分
weight_positivelevel <- c(1)  # uid和eid之间的正向评价指数,-1/0/1
weight_Servertimes <- c(-0.1)    # eid的服务次数
weight_Iszodiacuid <- c(0.5)    # 是否uid的
weight_AvgUidScore <- c(0.2)
weight_Iscitymatch <- c(0.2)
weight_IsProvincematch <- c(0.1)
weight_Avggift <- c(0.8)
weight_AgeGap <- c(-0.2)

save(eid_info, uid_info, uid_op_services, uid_transform_total, file = './data/uid_eid.rData')

# uid_eid_mapping
uid_canditie <- c('2031738135')
uid_eid_mapping <- cbind(uid_transform_total%>%
                           filter(num_uid %in% uid_canditie), eid_info%>%
                           select(Eid, age, worktime, zodiac_uid, Gender))

# join上uid和eid之间的服务关系；
uid_eid_mapping <- left_join(uid_eid_mapping, uid_op_services, by = c("num_uid"="uid", "Eid"="eid"), copy = F)

# NA赋值成0
uid_eid_mapping[is.na(uid_eid_mapping$score), "score"] <- 3
uid_eid_mapping[is.na(uid_eid_mapping$times), "times"] <- 0

#  Feature变化
uid_eid_mapping <- uid_eid_mapping%>%
    mutate(isgendermatch = num_gender==Gender) # eid和uid的性别是否相同；
uid_eid_mapping <- uid_eid_mapping%>%
    mutate(positivelevel= ifelse(score>3,1,ifelse(score<3,-1,0))) # 如果评分<3分，则为负推荐；

# 生成agegap;
uid_eid_mapping <- uid_eid_mapping%>%
    mutate(agegap = (as.integer(num_age)-age)/100)

# uid_eid_mapping%>%View()

# 对年龄进行分桶
# uid_eid_mapping <- uid_eid_mapping %>%
#   mutate(
#     age_cln = ifelse(age < 14 | age > 100, -1, age),
#     age_bucket = cut(age, breaks = c(min(age_cln), 19, 24,
#                                      29, 34, 39, 44, 49, 54,
#                                      59, 64, 69, 74, 79, 84,
#                                      89, 94, 99, max(age_cln)
#     ))
#     ,age_bucket = mapvalues(age_bucket,
#                            from=c("(19,24]", "(24,29]", "(29,34]", "(34,39]",
#                                   "(39,44]", "(44,49]", "(49,54]", "(54,59]",
#                                   "(59,64]", "(64,69]", "(69,74]", "(74,79]",
#                                   "(79,84]", "(84,89]", "(89,94]", "(94,99]", "(99,150]"),
#                            to=c("20-24", "25-29", "30-34", "35-39",
#                                 "40-44", "45-49", "50-54", "55-59",
#                                 "60-64", "65-69", "70-74", "75-79",
#                                 "80-84", "85-89", "90-94", "95-99", "100+"))
#   )

# 计算出最后的得分
uid_eid_mapping%>%
  mutate(totalscore = isgendermatch*weight_isgendermatch
                      +score*weight_AvgUidScore
                      +times*weight_Servertimes
                      +worktime*weight_workexperience
                      +weight_zodiacuid*zodiac_uid
                      +positivelevel*weight_positivelevel
                      +agegap*weight_AgeGap)%>%
  arrange(desc(totalscore))%>%
  View()