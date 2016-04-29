--紧张度数据汇总
--源表：DW_Htl_TimeDB.factunbookabledailyreportjoinpdhourlydata，dw_htldb.facthtlordersnap
--作者：jh.li


use tmp_htldb;
--1.房态日期格式转化
drop table tmpfactunbookabledailyreportjoinpdhourlydata_date;
--（13226034 Rows）
create table tmpfactunbookabledailyreportjoinpdhourlydata_date
as
select 
concat(concat_ws('-',substring(reportversion,1,4),substring(reportversion,5,2),substring(reportversion,7,2)),' ',substring(reportversion,9,2)) as  reportversion
,checkindate
,city
,max(cityi) cityi
,max(gcity) gcity
,zone
,max(zonei) zonei
,max(gzone) gzone
,star
,max(zonestari) zonestari
,max(gzonestar) gzonestar
,max(intense) intense
,max(gintense) gintense
,case when max(elongroom) > 0 then 1
when max(elongroom) <0 then -1
else 0 end as elongroomstatus
from DW_Htl_TimeDB.factunbookabledailyreportjoinpdhourlydata
where d>='2014-08-01' and d<'2014-11-01'
group by concat(concat_ws('-',substring(reportversion,1,4),substring(reportversion,5,2),substring(reportversion,7,2)),' ',substring(reportversion,9,2)) 
,checkindate
,city
,zone
,star
--2.筛选订单表
use tmp_htldb;
drop table tmpfacthtlordersnap_zonestar;
--(19288321 Rows )
create table tmpfacthtlordersnap_zonestar
as
select a.orderid,a.orderdate,a.arrival,a.cityid,b.zone,b.star
from
(
--(筛选出的订单：19288321 Rows)
select
orderid,
orderdate,
hotel,
substring(arrival,1,10) arrival,
cityid
from dw_htldb.facthtlordersnap
where d>='2014-08-01' and d<'2014-11-01' and channelID=4
) a
 join
(select 
hotel
,zone
,star
from dim_htldb.dimhtlhotel 
group by hotel,zone,star
)b
on 
a.hotel=b.hotel;

--3.获取订单id对应的city，zone，star,分批进行(19288321 Rows )
use dw_htldb;
drop table factunbookabledailyreportjoinpdhourlydata_cityzonestar;
--(19288321 Rows)
create  table factunbookabledailyreportjoinpdhourlydata_cityzonestar
as
select 
a.orderid,a.orderdate,a.arrival
,e.reportversion,e.checkindate,a.cityid,e.cityi,e.gcity
,a.zone,e.zonei,e.gzone
,a.star,e.zonestari
,e.intense,e.gintense,e.intenseprec
,e.elongroomstatus
from
(select
orderid,
orderdate,
arrival,
cityid,
zone,
star
from tmp_htldb.tmpfacthtlordersnap_zonestar
) a
left outer join
(select 
reportversion
,checkindate
,city
,cityi
,gcity
,zone
,zonei
,gzone
,star
,zonestari
,intense
,gintense
,case when  gintense is null  then null
else intense*1.0/(intense-gintense) end as intenseprec
,elongroomstatus
from tmp_htldb.tmpfactunbookabledailyreportjoinpdhourlydata_date
where reportversion>='2014-08-01' and reportversion<'2014-11-01'
)e
on 
if(hour(a.orderdate) % 2 <> 0,from_unixtime(unix_timestamp(a.orderdate)-3600,'yyyy-MM-dd HH'),substring(a.orderdate,1,13))= e.reportversion 
and a.arrival=substring(e.checkindate,1,10)  and a.cityid=e.city and a.zone=e.zone and a.star=e.star

--获得最终结果表（19288321 Rows )
use  dw_htldb;
drop table FactUnbookableRoomTensity;
create table FactUnbookableRoomTensity
as
select 
orderdate,orderid,cityi,gcity
,zonei,gzone,zonestari
,intense,gintense,intenseprec,elongroomstatus
from
dw_htldb.factunbookabledailyreportjoinpdhourlydata_cityzonestar


--校验数据
  select 
reportversion
,checkindate
,city,gcity
,zonei,gzone,zonestari
,intense,gintense,elongroomstatus
from
tmp_htldb.tmpfactunbookabledailyreportjoinpdhourlydata_date
where reportversion = '2014-08-20 18' and city=1 and zone = 651 and star=4 and checkindate ='2014-08-21 00:00:00'