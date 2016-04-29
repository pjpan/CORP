--子酒店订单平均确认时间(12696153 Rows)
--源表：dw_htldb.facthtlordersnap
--订单时间：2013-11-01~2014-11-01
--作者：jh.li

drop table if exists FactUnbookableHotelOrderAVGTime; 
create table FactUnbookableHotelOrderAVGTime
as
select substring(orderdate,1,10) orderdate,hotel,
count(orderid) as sumorder,
sum((unix_timestamp(confirmdate)/60)-(unix_timestamp(orderdate)/60)) as ordertime_sum,
avg((unix_timestamp(confirmdate)/60)-(unix_timestamp(orderdate)/60)) as ordertime_avg
from dw_htldb.facthtlordersnap
where orderdate>='2013-11-01' and orderdate<'2014-11-01' and channelid=4 and confirmdate is not null
group by substring(orderdate,1,10),hotel
--订单平均确认时间(12696153 Rows),保留两位小数
drop table if exists FactUnbookableHotelOrderAvgConfirmTime;
create table FactUnbookableHotelOrderAvgConfirmTime
as
select orderdate,hotel,sumorder
,round(ordertime_sum,2) ordertime_sum
,round(ordertime_avg,2) ordertime_avg
from FactUnbookableHotelOrderAVGTime