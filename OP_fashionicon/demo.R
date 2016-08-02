setwd('D:/project/【SRV】网红项目/')
cat("load the data")
eid_info <- data.table::fread(input = './data/emp_info.csv', na.strings = c("NULL", "NA", NA))
uid_info <- data.table::fread(input = './data/uid_info.csv', na.strings = c("NULL", "NA", NA))
uid_op_services <- data.table::fread(input = './data/uid_op_services.csv', na.strings = c("NULL", "NA", NA))





