##########################################
#### 다람쥐버스 수요 분담 노선 만들기 ####
####   8221번보완 동대문구 노선 예시  ####
#########################################

# 동대문구 노선 정리, 정류장 클러스터링 & 시각화

# 패키지 불러오기
rm(list=ls())
library(plyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(progress)
library(geosphere)



# 동대문구 버스 정보 정리 
## 동대문구 버스 정류장ARS 정리 =========================
### 전처리에서 정리한 정류장별 행정동 데이터 이용 (관련코드는 깃에 없음)
bus_dong=fread("버스정류장행정동_승차.csv", header=T, 
                 stringsAsFactors = F,
                 data.table = F)
bus_dong %>% colnames

### 8221번이 지나는 답십리1동, 답십리2동,장안1동, 장안2동으로 분석 범위 한정
ddmg_name = c("답십리1동", "답십리2동","장안1동", "장안2동")
ddmg_ars = bus_dong %>% filter(행정동 %in% ddmg_name) %>% select(버스정류장ARS번호_nu) %>% unlist
ddmg_ars = unique(ddmg_ars)
ddmg_ars #행정동내 버스 정류장 ars 99개


## 동대문구 정류장 좌표 정리 =========================
station_info = fread("서울특별시 버스정류소 위치정보.csv", header=T, 
                     stringsAsFactors = F,
                     data.table = F)
station_info = station_info[,c(1,3,4)]
station_info$정류소번호 = as.numeric(station_info$정류소번호)
station_info %>% head

bus_ddmg = data.frame(
  station = ddmg_ars,
  x = NA, y=NA)

for (k in 1:nrow(bus_ddmg)){
  bus_ddmg[k,2:3]= filter(station_info,정류소번호==bus_ddmg[k,1])[,2:3]
}


## 동대문구 정류장 승하차 인원수 =========================
### 2020년 9월 데이터 이용
data=fread("2020년_버스노선별_정류장별_시간대별_승하차_인원_정보(09월).csv", header=T, 
           stringsAsFactors = F,
           data.table = F)
data=data[,c(2,5,21,22,23,24,25,26)]
data$버스정류장ARS번호 = as.numeric(data$버스정류장ARS번호)
data %>% colnames #노선번호, 정류장ars, 7~9승하차승객수

bus_ddmg$take = NA
bus_ddmg$getoff = NA

for (i in 1:nrow(bus_ddmg)){
  bus_ddmg$take[i]= filter(data,버스정류장ARS번호==bus_ddmg[i,1])[,c(3,5,7)] %>% sum
  bus_ddmg$getoff[i]= filter(data,버스정류장ARS번호==bus_ddmg[i,1])[,c(4,6,8)] %>% sum
}

bus_ddmg$total = bus_ddmg$take + bus_ddmg$getoff
bus_ddmg %>% head


## 정류장별 버스노선수 =================================
bus_ddmg$bus_num= NA
for (i in 1:nrow(bus_ddmg)){
  bus_ddmg$bus_num[i]=filter(data,버스정류장ARS번호==bus_ddmg[i,1]) %>% nrow
}


## 지하철역(답십리/장한평)과의 거리 ====================
### 지하철역 위치정보 데이터
train=fread("train_popl_With_add.csv", header=T, 
                stringsAsFactors = F,
                data.table = F)
train_ddmg=train %>% filter(역명%in%c("답십리","장한평")) %>% dplyr::select(역명,경도,위도)

bus_ddmg$to_dap = distm(bus_ddmg[,2:3], train_ddmg[1,2:3], fun = distHaversine) %>% as.vector()
bus_ddmg$to_jang = distm(bus_ddmg[,2:3], train_ddmg[2,2:3], fun = distHaversine) %>% as.vector()
bus_ddmg$nearest=apply(bus_ddmg[,8:9],1,min) #둘 중 가까운 역까지의 거리

bus_ddmg
bus_ddmg %>% dim # 99 x 10


## 다람쥐버스가 지나지 않는 동대문구내 버스 정류장 ====================
station8221 = data %>% filter(노선번호=="8221") %>% select(버스정류장ARS번호) %>% unlist
bus_ddmg2=bus_ddmg[!(bus_ddmg$station %in% station8221),]
bus_ddmg2 = bus_ddmg2[,c(1,6,7,10)]
bus_ddmg2 %>% head #클러스터링에 이용할 변수만 선택
bus_ddmg2 %>% dim # 84 x 4





# 버스 정류장 클러스터링
## 클러스터링 패키지 추가로 불러오기 =========================
library(gridExtra)
library(factoextra)
library(cluster)

corrplot::corrplot(cor(bus_ddmg2[,-1]), method = 'number') 
cluster = scale(bus_ddmg2[,-1]) %>% as_tibble()  

## PAM 클러스터링 ===================================
fviz_nbclust(x = cluster, FUNcluster = pam, method='wss') 
fviz_nbclust(x = cluster , FUNcluster = pam, method = "silhouette")
pam1 <- pam(cluster, 7)
fviz_cluster(pam1)

## K-means 클러스터링 ===================================
fviz_nbclust(x = cluster, FUNcluster = kmeans, method='wss') 
fviz_nbclust(x = cluster, FUNcluster = kmeans, method = "silhouette")
kmeans1 <- kmeans(cluster, nstart = 1, iter.max = 15, centers = 5)
fviz_cluster(kmeans1, cluster)


## 클러스터링 결과 저장===================================
### 실루엣값이 더 잘 나온 k-means사용
### 클러스터가 2일때 가장 좋지만, 특성을 세분화하기 위해 5로 설정함
result<- cbind(kmeans1$cluster,bus_ddmg2[,1])
result <- as.data.frame(result)





# 클러스터별 box plot
result8221=rbind(result, data.frame(V1=0, V2=station8221))
result8221=merge(bus_ddmg,result8221,by.x="station",by.y="V2")
dim(result8221)

result8221$V1=as.factor(result8221$V1)
p1=result8221 %>% group_by(V1) %>% 
  ggplot(aes(x=V1, y=total))+
  geom_boxplot(fill=c("#735C4F","#ec524b","#E3D955","#ADB36E","#28abb9","#a37eba"))+
  theme_classic()

p2=result8221 %>% group_by(V1) %>% 
  ggplot(aes(x=V1, y=nearest))+
  geom_boxplot(fill=c("#735C4F","#ec524b","#E3D955","#ADB36E","#28abb9","#a37eba"))+
  theme_classic()


grid.arrange(p1,p2,nrow=1)