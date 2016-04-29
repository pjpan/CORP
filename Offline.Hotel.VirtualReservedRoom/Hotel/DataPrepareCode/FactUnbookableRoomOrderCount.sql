
--子房型订单满房表(21857784 Rows)
--源表：dw_htldb.facthtlordersnap 
--订单时间：2013-11-01~2014-11-01
--作者：jh.li

drop table if exists FactUnbookableRoomOrderCount;
create table FactUnbookableRoomOrderCount
as
select  substring(orderdate,1,10) orderdate,
substring(arrival,1,10) arrival,
hotel,room,
count(case when cancelreason='NOROOM' then orderid end) as noroomsumorder,
count(orderid) as sumorder
from dw_htldb.facthtlordersnap
where orderdate>='2013-11-01' and orderdate<'2014-11-01' and channelid=4 and holdroomtype='F' and freesale='F'
group by d,substring(arrival,1,10),hotel,room