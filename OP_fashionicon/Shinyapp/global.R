# Load required libraries
library(shiny)
library(data.table)
library(DT)
# library(shinythemes)
# library(plotly)
# library(viridis)
library(dplyr)

# Read a subset of the train data
# setwd('/Users/ppj/Documents/CORP/OP_fashionicon')
setwd("D:/gitcode/CORP/OP_fashionicon")
load("./data/uid_eid.rData")

# Extract the unique place ids for subsetting in the app graphs
# placeIds <- unique(dataSubset$place_id)
uids <- table(uid_info$uid)
uidslist <- names(sort(uids, decreasing = TRUE))

# 给不同维度进行赋权；
weight_isgendermatch <- c(-0.5) # 性别是否匹配
weight_zodiacuid <- c(-0.2)  # 成为专属客服次数
weight_workexperience <- c(0.4) # eid工作年限
weight_avgscore <- c(0.2)       # 网红的平均得分
weight_positivelevel <- c(1)  # uid和eid之间的正向评价指数,-1/0/1
weight_Servertimes <- c(0.2)    # eid的服务次数
weight_Iszodiacuid <- c(0.5)    # 是否uid的
weight_AvgUidScore <- c(0.2)
weight_Iscitymatch <- c(0.2)
weight_IsProvincematch <- c(0.1)
weight_Avggift <- c(0.4)
weight_AgeGap <- c(-0.2)


# 给不同维度进行赋权；
weight_isgendermatch_low <- c(-0.5) # 性别是否匹配
weight_zodiacuid_low <- c(-0.2)  # 成为专属客服次数
weight_workexperience_low <- c(0) # eid工作年限
weight_avgscore_low <- c(0.1)       # 网红的平均得分
weight_positivelevel_low <- c(1)  # uid和eid之间的正向评价指数,-1/0/1
weight_Servertimes_low <- c(0.2)    # eid的服务次数
weight_Iszodiacuid_low <- c(0.5)    # 是否uid的
weight_AvgUidScore_low <- c(0.2)
weight_Iscitymatch_low <- c(0.2)
weight_IsProvincematch_low <- c(0.1)
weight_Avggift_low <- c(0.2)
weight_AgeGap_low <- c(-0.3)

