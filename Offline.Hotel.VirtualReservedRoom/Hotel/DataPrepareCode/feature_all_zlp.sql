--目的：酒店订单即时确认特征值一期
--粒度：订单
--时间：最近12个月，2014年01~12月
--开发时间：2014-12-31
--开发者：zlp
--目的表：OrderConfirmModelFeature1

--计算订单假期
use dw_htldb;
drop table if exists OrderConfirmModelTmpOrderHolidays;
create table IF NOT EXISTS OrderConfirmModelTmpOrderHolidays 
as select
o.OrderId,
case
when v.HolidaySolar is null or v.HolidaySolar='' then v.HolidayLunar
else v.HolidaySolar
end as holiday
from
OrderConfirmModelMROutputSubOrder o
left outer join
OrderConfirmModelDimHolidayView v
on(o.EffectDate=v.datesolar);

insert overwrite table OrderConfirmModelTmpOrderHolidays
select orderId,max(holiday)
from
OrderConfirmModelTmpOrderHolidays
group by orderId;

----汇总订单临时计算
drop table if exists OrderConfirmModelTmpCal;
create table  IF NOT EXISTS OrderConfirmModelTmpCal
as
select
o.orderId,
o.cityId,
o.room,
o.hotel,
o.uid,
v.MasterHotelID,
'' as hwtag
from
OrderConfirmModelFactHtlOrder o
left outer join
OrderConfirmModelDimHotelView v
on(o.hotel=v.hotel);

insert overwrite table OrderConfirmModelTmpCal
select o.orderId,
o.cityId,
o.room,
o.hotel,
o.uid,
o.MasterHotelID,
mt.hwtag
from
OrderConfirmModelTmpCal o
left outer join
OrderConfirmModelMROutputHotel mt
on o.MasterHotelID = mt.MasterHotelID;

----汇总订单临时计算2
drop table if exists OrderConfirmModelTmpCal2;
create table IF NOT EXISTS OrderConfirmModelTmpCal2 as
select
f.orderId,
f.MasterHotelID,
mo.BaseRoom,
cv.level,
hv.confirmType,
hv.RoomQuantity as HotelRoomQuantity,
hv.GoldStar,
hv.CustomerEval,
hv.CPRFlag,
hv.hotelBelongTo,
hv.IsOpened,
hv.star,
dc.ComplaintTimes,
th.holiday,
f.HWTag,
mo.totalRoomNums,
mo.noRoomNums
from
OrderConfirmModelTmpCal f
left outer join
OrderConfirmModelDimCityView cv
on f.cityId=cv.cityId
left outer join
OrderConfirmModelDimRoomView rv
on f.room=rv.room
left outer join
OrderConfirmModelDimHotelView hv
on f.hotel=hv.hotel
left outer join
OrderConfirmModelDimCustomer dc
on f.uid=dc.uid
left outer join
OrderConfirmModelTmpOrderHolidays th
on f.OrderId=th.OrderId
left outer join
OrderConfirmModelMROutputOrder mo
on f.OrderId=mo.OrderId;

drop table if exists OrderConfirmModelFeature1;
create table IF NOT EXISTS OrderConfirmModelFeature1(
OrderID bigint comment '订单ID',
OrderDate string comment '预订日期 ',
ConfirmDate string comment '确认时间(订单确认) ',
Arrival string comment '预订最晚到店时间',
ArrivalTime string comment '预订最晚到店时间,精确到秒',
ETD  string comment '实际离店时间',
Room int comment '子房型ID',
Hotel int comment '子酒店ID',
Uid String comment '下订单用户ID',
VipGrade int comment 'VIP级别',
RecommendLevel int comment '酒店推荐等级',
CityId int comment '城市id',
BalanceType String comment '收款方式,FG 客户前台支付,PP 携程预收款,AD 为免房',
IsEbooking string comment '是否EBooking确认订单',
submitFrom string comment '订单提交来源',
ordRoomStatus string comment '房态状态',
ordRoomNum int comment '预订间数',
ordDays int comment '入住天数',
ordPrior int comment '提前预订天数',
ordHour int comment '下单时刻',
ReferenceBy string comment '是否来源修改单',
CancelReason string comment '取消原因CCANCEL：客户通知 NOSHOW：NoShow JOURALTER：行程改变 NOROOM：满房 NOTFULFIL：无法满足需求 COTHER：其它 OTHERORDER：其它途径预订 OTHER：其他 DBLORDER：重复预订 TESTING：test HHALFDATA：入住登记不详细 ',
HoldRoomType string comment '保留房类型 F：不保留,H：酒店保留',
FreeSale string comment '房型属于可自由预订,F：否,T：是',
remarks String comment '是否有客人的特殊备注说明',
IsStraightConnect String comment '是否直连酒店',
isFriday boolean comment '是否是周五',
isSaturday boolean comment '是否是周六',
isWeekDay boolean comment '是否是周中',
istestaccount smallint comment '是否测试订单 0：非测试账号 1：测试账号',
orderstatus string comment '订单状态',
reference bigint comment '修改前订单号',
ciiamount double comment '订单预订金额 ',
Guarantee  string comment '是否要担保',
ModifyReason String comment '修改原因',
MasterHotelID int comment '母酒店ID',
BaseRoom int comment '基础房型ID',
level int comment '城市是几线',
confirmType String comment '确认方式（P电话，F传真,C核对)',
HotelRoomQuantity int comment '酒店房间数',
GoldStar String comment '酒店金银牌',
CustomerEval float comment '用户评分钻级',
CPRFlag String comment 'CPR酒店类型',
hotelBelongTo String comment '维护部门',
IsOpened String comment '是否集团对接',
star int comment '星级',
ComplaintTimes int comment '用户投诉次数',
Holiday String comment '公立农立假日',
hwtag String comment 'W=商务型酒店,H=度假型酒店,HW=商务度假型酒店',
totalRoomNums int comment '预订日期之前30天已经预订的间数',
noRoomNums  int comment '预订日期之前3天最大满房订单数量'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/feature1";

insert overwrite table OrderConfirmModelFeature1
select 
f1.*,
f.MasterHotelID,
f.BaseRoom,
f.level,
f.confirmType,
f.HotelRoomQuantity,
f.GoldStar,
f.CustomerEval,
f.CPRFlag,
f.hotelBelongTo,
f.IsOpened,
f.star,
f.ComplaintTimes,
f.holiday,
f.HWTag,
f.totalRoomNums,
f.noRoomNums
from
OrderConfirmModelFactHtlOrder f1
left outer join
OrderConfirmModelTmpCal2 f
on f.orderId = f1.orderId;