##########################################
#### 다람쥐버스 승하차 인원 시각화 ####
####       8221번 노선 예시        ####
#########################################

# 정류장별 승하차 인원 파악 bar plot

## 디렉토리설정과 패키지 불러오기
library(plyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(progress)


# 8221번 버스 정보 정리 =========================
## 2020년 9월 데이터 이용
data=fread("2020년_버스노선별_정류장별_시간대별_승하차_인원_정보(09월).csv", header=T, 
           stringsAsFactors = F,
           data.table = F)
data=data[,c(2,5,6,21,22,23,24,25,26)]
data %>% colnames #노선번호, 정류장ars, 정류장이름, 7~9승하차승객수
data %>% head
bus8221 = data %>% filter(노선번호 == "8221") #8221번 버스 정보만 저장
station8221 = bus8221 %>% select(역명) #8221번 버스 정류장 이름


sh8221 = data.frame(
  station = station8221,
  take=NA,getoff=NA)

for (i in 1:nrow(sh8221)){
  sh8221[i,2]= filter(bus8221,역명==sh8221[i,1])[,c(4,6,8)] %>% sum
  sh8221[i,3]= filter(bus8221,역명==sh8221[i,1])[,c(5,7,9)] %>% sum
}

names(sh8221)[1] = "station"
sh8221

# 8221번 버스 20년 9월 승하차 인원 시각화 =========================
sh8221%>%
  gather(key='inout',value='value',take,getoff) %>% 
  group_by(station) %>% 
  ggplot(aes(x=station, y=value, fill=inout))+
  geom_bar(stat="identity")+
  ggtitle("8221번 버스 정류장기준 승하차인원(20.09)")+
  xlab("정류장")+
  ylab("승하차인원")+
  theme_classic()+
  scale_fill_manual(values=c("#F2762E","#735C4F"))+
  theme(axis.text.x = element_text(size=6,face="bold",angle=30))

# 지하철역에 하차가 집중됨
# 같은 방식으로 다른 시기도 시각화했을 때, 그 경향은 차이가 없었음