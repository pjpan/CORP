# cancelreason=@  score = 0.00609027   old :0.01493215
# calcReservedRoom("1470145015,5,0,0,6,-1.0,0,5,0,1,0,6,0,1,124,0,6,17,0,0,16,27,0.04032258064516129,-1.0,0.0,0.04838709677419355,0.8333333333333334,0.0,0.0,-1.0,0.0,0.0,-1.0,0.0,0.0,0.00398406374501992,0.0020876826722338203,-1.0,-1.0,0.0,0.04838709677419355,0.35294117647058826,-1.0,0.12903225806451613,0.5925925925925926,0.12455999851226807,0.001139998435974121,0.11824999749660492,-0.0015100017189979553,0.07440999895334244,0.07440999895334244,-0.005640000104904175,-0.08004999905824661,-1,16,1,2,1,2,PPFirstDay,3,3,-1,-1,23,0,5,2,HPP,2,21,-1,19,-1,0,24")
# cancelreason=NOROOM  score=0.0314641
# calcReservedRoom("1552505450,0,0,0,6,0.0,0,0,0,0,0,4,0,4,10,2,0,7,0,0,0,14,0.0,0.0,0.0,0.6,0.0,-1.0,0.0,0.0,-1.0,-1.0,0.0,0.0,0.0,0.017391304347826087,0.017391304347826087,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1,32,1,0,1,1,PPFull,5,1,-1,-1,1,0,2,0,HPP,2,-1,7,-1,7,0,23")

calcReservedRoom <- function(inParam)
{
  # package申明；
  if(!require(gbm)) install.packages("gbm")
  # if(!require(dplyr)) install.packages("dplyr")
  #package的引用
  library(gbm)
  # library(dplyr)
  options(stringsAsFactors = FALSE) #  char类型不要自动转化成 factor
  # setwd(getwd())
  # load(paste0("./ProdScripts/ReservedRoomv6.0.RData"))
  # rm(list=ls())
  
  # debug 
  # load("/home/hotelUser/Hotel_API_SOA/ReservedRoom/R/ReservedRoomv2.0.RData")
  
  # load("/home/pjpan/virtualroom/DirectModelRetrain/ProdScripts/ReservedRoomv5.0.RData") 
  
  # load data-Versions1
  if(is.character(inParam)) 
  {
    rawdata <- strsplit(c(inParam), ",")
    data <- as.data.frame(do.call(rbind, rawdata))
  }
  
  #  #  版本2.0的结果
  # data <- eval(parse(text = paste0("cbind(",paste0('x', 1:76,collapse=","),")")))   #  拼接参数
  # data <- as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27
  #                             ,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50,x51
  #                             ,x52,x53,x54,x55,x56,x57,x58,x59,x60,x61,x62,x63,x64,x65,x66,x67,x68,x69,x70,x71,x72,x73,x74,x75,x76))
  
  # data <- sapply(data, as.character)
  
  if(length(data) == 76)
  {
    names(data) <- c('orderid',
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
                     'arrivalhour'                
    )
    
    # transform the data
    data$cityi = as.numeric(data$cityi)
    data$gcity = as.numeric(data$gcity)
    data$zonei = as.numeric(data$zonei)
    data$gzone = as.numeric(data$gzone)
    data$zonestari = as.numeric(data$zonestari)
    data$intense = as.numeric(data$intense)
    # data$elongroomstatus = as.integer(data$elongroomstatus)
    #  data$elongroomstatus = ifelse(data$elongroomstatus>0 , 1, ifelse(data$elongroomstatus<0, -1, data$elongroomstatus))
    data$int_orddays = as.integer(data$orddays)
    data$int_orddays = ifelse(data$int_orddays >= 3, 3, data$int_orddays)
    data$hotelroomquantity = ifelse(as.integer(data$hotelroomquantity)<0,0,as.integer(data$hotelroomquantity))   # 酒店的房间数
    data$roomquantity = as.integer(data$roomquantity)  # 此基础房型的房间数
    data$room_sumorder = as.integer(data$room_sumorder)
    data$room_noroomsumorder = as.integer(data$room_noroomsumorder)
    data$htl_sumorder = as.integer(data$htl_sumorder)
    data$htl_noroomsumorder = as.integer(data$htl_noroomsumorder)                  
    data$pred.roomqty = ifelse(data$roomquantity <=0 ,ifelse(as.integer(data$maxroomorder_r30d)>0, as.integer(data$maxroomorder_r30d),as.integer(data$maxhtlorder_r30d)), data$roomquantity)  # 如果没有房型的房间数，用历史出现的最大房量来替代；
    data$int_recommendlevel= as.integer(data$recommendlevel)  # 推荐级别，与满房率负相关
    data$int_recommendlevel=ifelse(data$int_recommendlevel>=10,10,data$int_recommendlevel)
    data$int_ordroomnum = ifelse(as.integer(data$ordroomnum) >= 4, 4,as.integer(data$ordroomnum))                  
    data$ftr_cprflag = as.numeric(data$cprflag)
    data$pct_last1ynoroomrate = ifelse(as.numeric(data$last1ynoroomrate)>1, 1, as.numeric(data$last1ynoroomrate))
    data$pct_masterhtlnoroomrate = ifelse(as.numeric(data$masterhtlnoroomrate)>1, 1, as.numeric(data$masterhtlnoroomrate))
    data$pct_htlnoroompct = ifelse(as.numeric(data$htlnoroompct) >= 1, 1, as.numeric(data$htlnoroompct))
    data$pct_htlroompct = ifelse(as.numeric(data$htlroompct) >= 1,1 ,as.numeric(data$htlroompct))
    data$pct_htlsmtpct = as.integer(data$htl_sumorder)/(as.integer(data$hotelroomquantity)+0.001)
    data$pct_htlsmtpct = ifelse(data$pct_htlsmtpct>1, 1, data$pct_htlsmtpct)
    data$pct_htlsmtpct = ifelse(data$pct_htlsmtpct<0, -1, data$pct_htlsmtpct)
    data$pct_roompct = ifelse(as.numeric(data$room_sumorder)/(as.numeric(data$pred.roomqty)+0.001)>1, 1 ,as.numeric(data$room_sumorder)/(as.numeric(data$pred.roomqty)+0.001))
    data$pct_roompct = ifelse(data$pct_roompct>1, 1 , data$pct_roompct)
    data$pct_roompct = ifelse(data$pct_roompct<0, -1 , data$pct_roompct)
    data$pct_roomnoroomrate = as.numeric(data$room_noroomsumorder)/(as.numeric(data$room_sumorder)+0.001)  # 同基础房型满房在店房间数/总在店房间数
    data$pct_roomnoroomrate = ifelse(data$pct_roomnoroomrate>1,1,data$pct_roomnoroomrate)
    data$pct_htlnoroomrate = as.numeric(data$htl_noroomsumorder)/(as.numeric(data$htl_sumorder)+0.001) # 同子酒店在店房间数/总在店房间数
    data$pct_htlnoroomrate = ifelse(data$pct_htlnoroomrate>1,1,data$pct_htlnoroomrate)
    #     data$room_3_7 = (as.numeric(data$room_last3dsumorder) + as.numeric(data$room_last7dsumorder))/2        # 
    #     data$room_7_30 = (as.numeric(data$room_last7dsumorder) + as.numeric(data$room_sumorder))/2             # 
    #     data$hotel_7_30 = (as.numeric(data$htl_last7dsumorder) + as.numeric(data$htl_sumorder))/2            # 
    # data$hotel_3_7 = (as.numeric(data$htl_last7dsumorder) + as.numeric(data$htl_last3dsumorder))/2       # 
    data$pct_roomfullrate = as.integer(data$room_sumorder)/(as.integer(data$maxroomorder)+0.001)
    data$pct_roomfullrate = ifelse(data$pct_roomfullrate>1,1,ifelse(data$pct_roomfullrate<0,-1,data$pct_roomfullrate))  # 超过1看做1，没有历史满房最大值
    data$pct_htlfullrate = as.integer(data$htl_sumorder)/(as.integer(data$maxhtlorder)+0.001)
    data$pct_htlfullrate = ifelse(data$pct_htlfullrate>1,-1,data$pct_htlfullrate)  # 如果没有酒店房间数，这会超过1,，看做1
#   data$pct_roomhtlsmtpct = as.integer(data$room_sumorder)/(as.integer(data$htl_sumorder)+0.001)
#   data$pct_roomhtlsmtpct = ifelse(data$pct_roomhtlsmtpct>1,1,data$pct_roomhtlsmtpct)
#   data$pct_roomhtlsmtpct = ifelse(data$pct_roomhtlsmtpct<0,-1,data$pct_roomhtlsmtpct)
    # data$ftr_arrivalhour = ifelse(as.integer(data$arrivalhour)<= 18, 18 , as.integer(data$arrivalhour))  #  factor
    data$ftr_level = as.integer(data$level)
    data$int_ordhour = as.integer(data$ordhour)
    data$int_ordhour = ifelse(data$int_ordhour<=6, 6, data$int_ordhour)
    # data$ftr_isweekday = as.integer(data$isweekday)
    data$ftr_goldstar = as.integer(data$goldstar)
    data$ftr_star = as.integer(data$star)
    data$ftr_star = ifelse(data$ftr_star<0, 0, data$ftr_star)  # 会出现star=-1的情况；	
    data$int_ordprior = ifelse(as.integer(data$ordprior)>=60, 61, as.integer(data$ordprior))
    #     data$pct_room_last3doccupyrate = ifelse(as.numeric(data$room_last3dsumorder)/(as.numeric(data$pred.roomqty)+0.001)>=1, 1 
    #                                             ,as.numeric(data$room_last3dsumorder)/(as.numeric(data$pred.roomqty)+0.001))
    #     data$pct_room_last3doccupyrate = ifelse(data$pct_room_last3doccupyrate<0, -1, data$pct_room_last3doccupyrate)
    #     data$pct_room_last7doccupyrate = ifelse(as.numeric(data$room_last7dsumorder)/(as.numeric(data$pred.roomqty)+0.001)>=1, 1 
    #                                             ,as.numeric(data$room_last7dsumorder)/(as.numeric(data$pred.roomqty)+0.001))
    #     data$pct_room_last7doccupyrate = ifelse(data$pct_room_last7doccupyrate<0, -1, data$pct_room_last7doccupyrate)
    #     data$bin_isoverroommax = ifelse((as.integer(data$room_sumorder)+as.integer(data$ordroomnum)) >= data$maxroomorder, 1, 0)
    #     data$bin_isoverhtlmax = ifelse(as.integer(data$htl_sumorder) > as.integer(data$maxhtlorder), 1, 0)
    # 满房临界点进行预处理；
    data$maxroomorder = as.integer(data$maxroomorder)
    data$maxroom_noroomsumorder = as.integer(data$maxroom_noroomsumorder)
    data$maxroompct = ifelse(as.numeric(data$maxroomorder)/(as.numeric(data$pred.roomqty)+0.001) >=1, 1, as.numeric(data$maxroomorder)/(as.numeric(data$pred.roomqty)+0.001))
    data$maxhtlorder = as.integer(data$maxhtlorder)
    data$maxhtl_noroomsumorder = as.integer(data$maxhtl_noroomsumorder)
    data$maxhtlpct = ifelse(as.integer(data$maxhtlorder)/(data$hotelroomquantity+0.001)>=1, 1, as.integer(data$maxhtlorder)/(data$hotelroomquantity)+0.001)
    data$maxhtlpct = ifelse(data$maxhtlpct<0, -1, data$maxhtlpct)
    data$maxhtlroompct = ifelse(as.numeric(data$maxhtlroompct)>=1, 1, as.numeric(data$maxhtlroompct))
    #     data$maxroomnoroompct = as.numeric(data$maxroomnoroompct)
    #     data$maxroomnoroompct = ifelse(data$maxroomnoroompct>1, 1, data$maxroomnoroompct)
    #     data$maxhtlnoroompct = as.numeric(data$maxhtlnoroompct)
    #     data$maxhtlnoroompct = ifelse(data$maxhtlnoroompct>1, 1, data$maxhtlnoroompct)
    #     data$maxroomhtlsmtpct = as.numeric(data$maxroomhtlsmtpct)
    #     data$maxroomhtlsmtpct = ifelse(data$maxroomhtlsmtpct>1, 1, data$maxroomhtlsmtpct)
    #     data$maxroomorder_r30d =as.integer(data$maxroomorder_r30d)
    #     data$maxhtlorder_r30d = as.integer(data$maxhtlorder_r30d)
    #     data$ispromotion = as.integer(data$ispromotion)
    #     data$avldaysbefhol = as.integer(data$avldaysbefhol)
    #     data$avldaysafthol = as.integer(data$avldaysafthol)
    data$province = as.integer(data$province)
    data$room_noroomsumorder = ifelse(data$room_noroomsumorder>2, 2, data$room_noroomsumorder)
    data[is.na(data)] <- -1       # # 把一些因子的NA变成 -1
    
    # 预测最后的的结果 ，只用一种模型来进行预测；
    #   data$pred <- predict(raw.gbm, newdata = data, type='response',n.trees = best.raw) # old model
    data$pred <- predict(direct.gbmmodel, newdata = data, type='response',n.trees = direct.ntrees)  # new model
    
    return(list(orderid= data$orderid, score= data$pred))
    
  }  else  {
    return(list(orderid= -1, score= 99))
  }
  
}
