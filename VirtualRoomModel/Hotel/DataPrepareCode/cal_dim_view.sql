--目的：维度视图和数据
--粒度：
--时间：
--开发时间：2014-12-31
--开发者：zlp
--目的表：
--源表：

use dw_htldb;
--城市level表
create external table IF NOT EXISTS OrderConfirmModelDimCityLevelInfo(
cityName string comment "城市名",
provinceName string comment "省会名",
level int comment "等级"
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
location '/user/bihtl/orderconfirmmodel/dim/dimCityLevel/';

--新建城市维度视图
drop view if exists OrderConfirmModelDimCityView;
create view IF NOT EXISTS OrderConfirmModelDimCityView as 
select 
distinct c.cityId,c.cityName,c.province,c.provinceName,l.level
from Dim_pubDB.DimCity c 
left outer join
OrderConfirmModelDimCityLevelInfo l 
on(l.cityName=c.cityName and c.provinceName=l.provinceName);

--新建假期维度视图
drop view if exists OrderConfirmModelDimHolidayView;
create view IF NOT EXISTS OrderConfirmModelDimHolidayView as 
select 
distinct(datesolar) as datesolar,
HolidaySolar,
HolidayLunar,
DayOfWeek
from dim_pubdb.dimtime;

--新建房型维度视图
drop view if exists OrderConfirmModelDimRoomView;
create view OrderConfirmModelDimRoomView as
select 
room,
hotel,
RoomQuantity
from Dim_HtlDB.dimroom;

--新建酒店维度视图
drop view if exists OrderConfirmModelDimHotelView;
create view IF NOT EXISTS OrderConfirmModelDimHotelView as
select 
hotel, --子酒店ID
GoldStar, --金银牌 
CustomerEval,--携程用户评级(0---6)表示钻级 
CPRFlag,--	0：否（与酒店合作不是很紧密）1：价格优势（与酒店合作很紧密）
hotelbelongto,--宾馆维护类别 HPP：预付—ctrip与酒店自己谈的数据,HTL：现付,PKG：度假维护,SHT：供应商预付
RoomQuantity,--酒店房间数 
IsStraightConnect,--是否直连 
IsOpened,
confirmType,----确认方式（P电话，F传真,C核对)
Star,--星级
case
when MasterHotelID <=0 or MasterHotelID is null then hotel
else MasterHotelID
end 
as MasterHotelID
from  dim_htldb.DimhtlHotel ;

--用户维度表
drop table if exists OrderConfirmModelDimCustomer;
create table IF NOT EXISTS OrderConfirmModelDimCustomer(
uid string comment '客户ID',
vipGrade int comment 'grade等级 10：金牌 20：白金 30：钻石',
ComplaintTimes int comment '投诉次数'
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE;
--insert 用户维度表
insert overwrite table OrderConfirmModelDimCustomer 
select 
uid,
max(grade), --取最近时间
COALESCE(max(ComplaintTimes),0)
from dw_pubdb.vwcustomeradhoc 
group by uid;




