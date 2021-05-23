#########################################
#### 다람쥐버스 노선별 정류장 전처리 ####
####    노선별 정류장 지도 시각화    ####
#########################################

# 노선별 정류장 정리, 노선 지도시각화, 노선별 이용객 플랏

## 디렉토리설정과 패키지 불러오기 코드는 생략
library(plyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(progress)
setwd("C:/Users/hjh05/Desktop/P-sat/귀여운 선대팀/주분_다람쥐버스/daramg/cute_daramg_data")


# 다람쥐버스 노선별 정류장 정리 =========================
## 2020년 9월 데이터 이용
data=fread("2020년_버스노선별_정류장별_시간대별_승하차_인원_정보(09월).csv", header=T, 
               stringsAsFactors = F,
               data.table = F)
data=data[,c(1,2,4,5,21,22,23,24,25,26)]
data %>% colnames #사용년월, 노선번호, 정류장id, 정류장이름, 7~9승하차승객수

darams=c("8221","8441", "8552", "8761", "8771", "8551", "8331")

station_ARS=list(NA)
for (i in 1:7){
  station_ARS[i] = data %>% filter(노선번호==darams[i]) %>% select(버스정류장ARS번호)
}
names(station_ARS)=darams

station_ARS #노선별 정류장 ARS번호 list



# 다람쥐버스 정류장 좌표 정리 =========================
station_info = fread("서울특별시 버스정류소 위치정보.csv", header=T, 
                     stringsAsFactors = F,
                     data.table = F)
station_info = station_info[,c(1,3,4)]
station_info %>% head

darams

## ver1 : 노선 각각의 정류장 위치정보 df -----------------
for (i in 1:7){
  df_name = paste0("daram",darams[i])
  df = data.frame(matrix(nrow=length(station_ARS[[i]]),ncol=3))
  for (k in 1:nrow(df)){
    df[k,]=station_info %>% filter(정류소번호==station_ARS[[i]][k])
  }
  assign(df_name, df)
}

## ver2 : 7개 노선의 모든 정류장을 하나의 df로 저장------
daram_bus = NULL
for (i in 1:7){
  df = data.frame(matrix(nrow=length(station_ARS[[i]]),ncol=4))
  for (k in seq_along(station_ARS[[i]])){
    df[k,1]=darams[i]
    df[k,2:4]=station_info %>% filter(정류소번호==station_ARS[[i]][k])
  }
  daram_bus=rbind(daram_bus, df)
}


# 지도시각화 =========================
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

h <- readOGR('Z_SOP_BND_ADM_DONG_PG.shp')
crs = CRS('+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m')

proj4string(h) <- crs

to_crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

map = spTransform(h, to_crs)

map = fortify(map, region = "ADM_DR_CD")
map$id <- as.numeric(map$id)

map = map %>% filter(id <= 1174099)

daram_bus$X1 = as.factor(daram_bus$X1)
pal = c("#ec524b","#fca652","#ffa5a5","#28abb9","#ADB36E","#a37eba","#47ADF3")
ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                        fill = '#F8F3E4', color = '#735C4F',alpha=0.7)+
  geom_point(data=daram_bus,aes(x=X3,y=X4,col=X1),size=1.5)+
  scale_color_discrete(name="다람쥐버스")+
  theme_classic()



# 2020년 월별 노선 이용객 플랏=========================
## 코드 중간 생략
## 이때 _total은 월별 다람쥐버스 이용객 총합을 정리한 데이터프레임

daramdaram=NULL
daramdaram=merge(bus8221_total,bus8441_total)
daramdaram=merge(daramdaram,bus8552_total)
daramdaram=merge(daramdaram,bus8761_total)
daramdaram=merge(daramdaram,bus8771_total)
daramdaram=merge(daramdaram,bus8551_total)
daramdaram=merge(daramdaram,bus8331_total)


daramdaram %>% 
  gather(key='darambus', value='value',bus8221,bus8441, bus8552, bus8761, bus8771, bus8551, bus8331) %>% 
  group_by(darambus) %>% 
  ggplot(aes(x=MONTH, y=value,color=darambus))+
  geom_line(size=2, alpha=0.6)+
  geom_point(size=2.5)+
  scale_x_discrete(limits=paste(1:10,"월"))+
  ggtitle("2020년 다람쥐버스 이용인원(하차기준")+
  xlab("2020년 1월~10월")+
  ylab("하차인원")+  
  scale_color_manual(values=pal)+
  theme_classic ()
