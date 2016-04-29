--目的：准备特征值，订单事实表
--粒度：订单
--时间：最近14个月，2013-10-01 ~ 2014-11-01
--开发时间：2014-12-31
--开发者：zlp
--目的表：OrderConfirmModelFactHtlOrder
--源表：订单源表,dw_htldb.facthtlordersnap

--订单事实表
use dw_htldb;
drop table if exists OrderConfirmModelFactHtlOrder;
create table IF NOT EXISTS OrderConfirmModelFactHtlOrder(
OrderID bigint comment '订单ID',
OrderDate string comment '预订日期 ',
ConfirmDate string comment '确认时间(订单确认) ',
Arrival string comment '预订最晚到店时间',
ArrivalTime string comment '预订最晚到店时间,精确到秒',
ETD  string comment '实际离店时间',
Room int comment '房型ID',
Hotel int comment '酒店ID',
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
ModifyReason String comment '修改原因'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY '\001'
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location "/user/bihtl/orderconfirmmodel/OrderConfirmModelFactHtlOrder";

insert overwrite table OrderConfirmModelFactHtlOrder
select
f.OrderID,
f.OrderDate,
f.ConfirmDate,
to_date(COALESCE(f.Arrival,'0000-00-00')),
f.Arrival,
to_date(COALESCE(f.ETD,'0000-00-00')),
f.Room,
f.Hotel,
f.Uid,
f.VipGrade,
f.RecommendLevel,
f.CityId,
regexp_replace(f.BalanceType, ",", " "),
regexp_replace(f.IsEbooking, ",", " "),
regexp_replace(f.submitfrom, ",", " "),
f.ordroomstatus,
f.ordroomnum,
f.orddays,
datediff(to_date(f.Arrival),to_date(f.OrderDate)) as ordPrior,
hour(f.OrderDate) as ordHour,
regexp_replace(f.referenceby, ",", " "),
regexp_replace(f.CancelReason, ",", " "),
regexp_replace(f.HoldRoomType,",", " "),
f.freesale,
regexp_replace(f.remarks, ",", " "),
f.IsStraightConnect,
if(d.DayOfWeek=6 or (f.ordDays>1 and d.DayOfWeek+f.ordDays-1 >= 6), true, false),
if(d.DayOfWeek=7 or (f.ordDays>1 and d.DayOfWeek+f.ordDays-1 >= 7), true, false),
if(d.DayOfWeek<6 or (f.ordDays>1 and d.DayOfWeek+f.ordDays-1 > 8), true, false),
f.istestaccount,
f.orderstatus,
f.reference,
f.ciiamount,
f.Guarantee,
f.ModifyReason
from
dw_htldb.facthtlordersnap f
left outer join 
OrderConfirmModelDimHolidayView d
on to_date(COALESCE(f.Arrival,'0000-00-00'))=d.datesolar
where f.OrderDate>='2014-06-01' and f.OrderDate<'2015-12-01' 
and f.channelID=4 and ordDays>0 and istestaccount=0;




