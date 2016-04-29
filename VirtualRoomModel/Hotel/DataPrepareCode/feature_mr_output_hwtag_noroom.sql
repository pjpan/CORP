--目的：准备特征值MR计算的输出数据，酒店是否度假酒店，历史下单时目标房型已预订最大间数，已经出现的最大满房订单数量
--粒度：订单
--时间：最近14个月，2013-10-01 ~ 2014-11-01
--开发时间：2014-12-31
--开发者：zlp
--目的表：OrderConfirmModelMROutputHotel、OrderConfirmModelMROutputOrder
--源表：订单源表,dw_htldb.facthtlordersnap

--MR计算结果商务型酒店、度假型酒店判断
use dw_htldb;
drop table if exists OrderConfirmModelMROutputSubOrder;
create external table IF NOT EXISTS OrderConfirmModelMROutputSubOrder(
orderId bigint comment '订单ID',
BaseRoom int comment '房间ID',
effectDate String comment '住在店中日期',
orderDate String comment '住在店中日期',
ordRoomNums int comment '订房间数',
cancel  String comment '取消原因'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/mrOutput/subOrder/";
--MR计算结果商务型酒店、度假型酒店判断
use dw_htldb;
drop table if exists OrderConfirmModelMROutputHotel;
create external table IF NOT EXISTS OrderConfirmModelMROutputHotel(
MasterHotelID int comment '母酒店ID',
HWTag String comment 'H:度假型酒店，商务型酒店',
WeekdayTotalOrdRooms int comment '最近一年周中房间间数',
WeekendTotalOrdRooms int comment '最近一年周末房间间数'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/mrOutput/htlHW/";

--Hive统计历史下单时目标房型已预订最大间数,已经出现的最大满房订单数量
drop table if exists OrderConfirmModelMROutputOrder;
create external table IF NOT EXISTS OrderConfirmModelMROutputOrder(
OrderId bigint comment '订单ID',
BaseRoom int comment '基础房型ID',
totalRoomNums int comment '预订日期之前30天已经预订的间数',
noRoomNums  int comment '预订日期之前3天最大满房订单数量'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/mrOutput/orderSum";
