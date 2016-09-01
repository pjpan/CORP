library(ggmap)
library(DT)
library(dplyr)
library(data.table)
# 
cat("load files")
# options(stringsAsFactors = F)
# setwd("D:/gitcode/PPJUtils/utils/")
# city_lonlat <- fread("./citycountry_lonlat_fixed.txt", sep ='\t')
# dim_city <- fread("./dimcity.txt", sep ='\t')
# 
# #  读取历史的
# cat("读取上海到其他地区的走势")
# shanghai_movetrends <- fread("D:/project/【tmp】临时任务/潜力城市挖掘20160818/shanghai_sort.csv")

setwd("D:/gitcode/CORP/citymovetrends")
load('./shanghaimovetrends.RData')

# # 截取走势的结果join上经纬度；
# setkey(shanghai_movetrends, city)
# setkey(dim_city, cityname)
# 
# # 根据城市名称来进行join
# shanghai_movetrends_addprovince <- left_join(shanghai_movetrends, filter(dim_city,country==1), by = c("city"="cityname"))
# 
# # 拼接城市和省份结果；
# shanghai_movetrends_addprovince <- shanghai_movetrends_addprovince%>%mutate(countryname2 = paste0(provincename, city))
# shanghai_movetrends_addprovince%>%head(10)%>%View()  # 查看数据
# 
# # 把经纬度信息和上海的目的地趋势结合在一起
# shanghai_movetrends_addlonlat <- inner_join(shanghai_movetrends_addprovince, city_lonlat, by = c("countryname2" = "countryname"))%>%
#   select(city, ratio, countryname2, lon, lat)
# shanghai_movetrends_addlonlat%>%arrange(desc(ratio))%>%tail(100)%>%View()  # 查看数据

# 开始作图,根据不同的分位数来进行分析
ratio_threfhold <- quantile(shanghai_movetrends_addlonlat$ratio, seq(from = 0, to = 1, by =0.1))

# 查看80%分位数的结果；
shanghai_movetrend_90 <- filter(shanghai_movetrends_addlonlat, ratio>=ratio_threfhold[10])
ggmap(get_googlemap(center = "China", zoom = 4, maptype = "terrain"))+
  geom_point(aes(x = lon, y = lat, size = ratio), colour = "red", data = shanghai_movetrend_90, alpha = 1)+
  geom_text(aes(label = ifelse(ratio>0.3,as.character(city), '')),hjust=0, vjust=0, data = shanghai_movetrend_90)


