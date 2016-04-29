--子酒店在近2小时出现在满房数据中的操作次数
--源表：ods_htlrepldb.Roominfo_Rec
--作者：jh.li

use dw_htldb;
create table OrderConfirmModelroominfooperate as
select a.d,b.hotel,
  sum(case when hour(a.operatetime) >=0 and hour(a.operatetime)<2 
              then 1 else 0 end)  a2,
  sum(case when hour(a.operatetime) >=2 and hour(a.operatetime)<4 
              then 1 else 0 end)  a4,
  sum(case when hour(a.operatetime) >=4 and hour(a.operatetime)<6
              then 1 else 0 end)  a6,
  sum(case when hour(a.operatetime) >=6 and hour(a.operatetime)<8
              then 1 else 0 end)  a8,
  sum(case when hour(a.operatetime) >=8 and hour(a.operatetime)<10
              then 1 else 0 end)  a10,
  sum(case when hour(a.operatetime) >=10 and hour(a.operatetime)<12
              then 1 else 0 end)  a12,
  sum(case when hour(a.operatetime) >=12 and hour(a.operatetime)<14
              then 1 else 0 end)  a14,
  sum(case when hour(a.operatetime) >=14 and hour(a.operatetime)<16
              then 1 else 0 end)  a16,
  sum(case when hour(a.operatetime) >=16 and hour(a.operatetime)<18
              then 1 else 0 end)  a18,
  sum(case when hour(a.operatetime) >=18 and hour(a.operatetime)<20
              then 1 else 0 end)  a20,
  sum(case when hour(a.operatetime) >=20 and hour(a.operatetime)<22
              then 1 else 0 end)  a22,
  sum(case when hour(a.operatetime) >=22 and hour(a.operatetime)<24
              then 1 else 0 end)  a24,
  count(a.operatetime) alls
from ods_htlrepldb.Roominfo_Rec a 
join dim_htldb.dimroom b  on a.room=b.room and a.d>='2014-08-01'and a.d<='2014-11-30' and a.new_roomstatus='N'
group by a.d,b.hotel;
