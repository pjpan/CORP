--目的：准备特征值MR计算的输入数据，酒店是否度假酒店，历史下单时目标房型已预订最大间数，已经出现的最大满房订单数量
--粒度：订单
--时间：最近14个月，2013-10-01 ~ 2014-11-01
--开发时间：2014-12-31
--开发者：zlp
--目的表：OrderConfirmModelMrInput
--源表：订单源表,dw_htldb.facthtlordersnap

use dw_htldb;
set hive.support.concurrency=false;
drop table if exists OrderConfirmModelMrInput;
create table IF NOT EXISTS OrderConfirmModelMrInput(
OrderID bigint comment '订单ID',
OrderDate string comment '预订日期 ',
Arrival string comment '预订最晚到店时间',
ETD  string comment '实际离店时间',
BasicRoomTypeID int comment '基础房型ID',
MasterHotelID int comment '母酒店ID',
ordRoomNum int comment '预订间数',
ordDays int comment '入住天数',
CancelReason string comment '取消原因CCANCEL：客户通知 NOSHOW：NoShow JOURALTER：行程改变 NOROOM：满房 NOTFULFIL：无法满足需求 COTHER：其它 OTHERORDER：其它途径预订 OTHER：其他 DBLORDER：重复预订 TESTING：test HHALFDATA：入住登记不详细 '
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/mrInput";

insert overwrite table OrderConfirmModelMrInput
select 
f.OrderID,
f.OrderDate,
to_date(f.Arrival),
to_date(f.ETD),
case
when r.BasicRoomTypeID <=0 or r.BasicRoomTypeID is null then f.room
else r.BasicRoomTypeID
end 
as BasicRoomTypeID,
case
when h.MasterHotelID <=0 then f.hotel
else h.MasterHotelID
end 
as MasterHotelID,
f.ordroomnum,
f.orddays,
regexp_replace(f.CancelReason, ",", " ")
from
OrderConfirmModelFactHtlOrder f
left outer join
dim_htldb.dimroom r
on f.room=r.room
left outer join
dim_htldb.dimhtlhotel h
on f.hotel=h.hotel;




