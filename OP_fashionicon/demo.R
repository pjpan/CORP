
#  加载数据；
setwd('D:/gitcode/CORP/OP_fashionicon')
cat("load the data")
eid_info <- data.table::fread(input = './data/emp_info.csv', na.strings = c("NULL", "NA", NA))
uid_info <- data.table::fread(input = './data/uid_info.csv', na.strings = c("NULL", "NA", NA))
uid_op_services <- data.table::fread(input = './data/uid_op_services.csv', na.strings = c("NULL", "NA", NA))


#  deal eid_info :age,name,
library(dplyr)
library(data.table)
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

# uid_eid_mapping
uid_canditie <- c('2031738135')
uid_eid_mapping <- cbind(uid_transform_total%>%filter(num_uid %in% uid_canditie), eid_info%>%select(Eid, age, worktime, zodiac_uid, Gender))

# join上uid和eid之间的服务关系；
uid_eid_mapping <- left_join(uid_eid_mapping, uid_op_services, by = c("num_uid"="uid", "Eid"="eid"), copy = F)


uid_eid_mapping <- uid_eid_mapping%>%mutate(isgendermatch = num_gender==Gender) # eid和uid的性别是否相同；

uid_eid_mapping <- uid_eid_mapping%>%mutate(agegap = num_age-age ) # eid和uid的性别是否相同；


















