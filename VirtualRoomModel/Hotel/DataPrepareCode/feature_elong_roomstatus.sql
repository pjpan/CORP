--目的：胡淏房态预测模型特征值提取：ElongRoom 的历史有无房
--粒度：天,key：子房型 
--时间：最近3个月，2014-09-15 ~ 2014-12-15
--开发时间：2014-12-31
--开发者：zlp
--目的表：OrderConfirmModelFeature1
--房型过去3、30天ElongRoom>0的次数和房型过去3天ElongRoom>0（有房）的次数以及ElongRoom=0（无房）的次数
--源表：PD提供的ELONG数据,DW_Htl_TimeDB.factunbookabledailyreportjoinpdhourlydata

use dw_htldb;
--艺龙有无房间MR输入表
--drop table if exists OrderConfirmModelELongHasRoomMRInput;
create table IF NOT EXISTS OrderConfirmModelELongHasRoomMRInput(
Room int comment '房型ID',
calDate string comment '操作日期',
hasRoomNum int comment '房间有房次数',
noRoomNum int comment '房间满房次数'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/hh/OrderConfirmModelELongMRInput";
--insert 数据
insert overwrite table OrderConfirmModelELongHasRoomMRInput
select room,d,
sum(
case
when elongroom > 0 then 1
else 0
end),
sum(
case
--=0是满房，-1是没有爬到 >0是有房
when elongroom =0 then 1
else 0
end)
from
DW_Htl_TimeDB.factunbookabledailyreportjoinpdhourlydata
where 
d>='2014-09-15' and d<'2014-12-15'
group by room,reportversion;

----艺龙有无房间MR输出表
--drop table if exists OrderConfirmModelELongHasRoomMROutput;
create external table IF NOT EXISTS OrderConfirmModelELongHasRoomMROutput(
Room int comment '房型ID',
calDate string comment '操作日期',
30hasRoomNum int comment '最近30天房间有房次数',
30noRoomNum int comment '最近30天房间满房次数',
3hasRoomNum int comment '最近3天房间有房次数',
3noRoomNum int comment '最近3天房间满房次数'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/hh/OrderConfirmModelELongMROutput";

