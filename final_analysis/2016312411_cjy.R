

##############################################################################################
#GETTING STARTED

library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library('ggplot2')
library(extrafont) 
font_import()


DATAGOKR_KEY ='....%...%.%..%.'
KAKAKOAPI_KEY ='.....'
getwd()
setwd("/Users/janechoi/Desktop/2020SKKU/위치기반데이터분석/final")


##############################################################################################
#READ CSV FILE FOR HOSPITAL INFORMATION 

hos1 <- read_csv("국민안심병원_운영기관_현황.csv")
hos2<- read_csv('선별진료소목록.csv')
car<- read_csv('승차검진_선별진료소_목록.csv')


#1 only choose address and make column kind to specify what kind of hospital it is 

add1 <- as.data.frame(hos1$주소)
add2<- as.data.frame(hos2$주소)
add3<- as.data.frame(car$주소)

add1$kind <- '국민안심병원'
add2$kind <- '선별진료소'
add3$kind <- '승차검진_선별진료소'

colnames(add1) <- c('주소', 'Kind')
colnames(add2) <- c('주소', 'Kind')
colnames(add3) <- c('주소', 'Kind')


#concat the address into one df 

df <- rbind(add1,add2,add3)

head(df)

#2 using gmap get the long, lat info 

library(ggmap)

my_gokey <- 'AIzaSyCoU1Vfn902r7-59HK_LbCPzthwiCN_wsE'
register_google(key = my_gokey)



##save all the addresses into one vector 
add_v =c()

for (atmp in df['주소']){
  add_v <- atmp 
}

#vector of address -- needs to be a character vector 
add_v  <- as.character(add_v)

add_v 


##geocode for the addresses :long, lat info 
gc <- geocode(enc2utf8(add_v)) 

for (i in 1:nrow(df)){
  df$위도[i] <- gc$lat[i]
  df$경도[i] <-gc$lon[i]
}


head(df) #now we have the address and the long,lat info of the hospitals 


#3 change types in the df 

df$주소 <- as.character(df$주소)


#4 deal with NA values 
#df =read.csv('df.csv')

colSums(is.na(df)) #2 NA valuse 

df <-na.omit(df)  #delete na values 

head(df)

summary(df)



#5 use ggmap to visualize the datset


center = c(lon=median(df$경도) , lat = median(df$위도))



#at the center of dataset -- showing Korea 
qmap(location = c(lon= center[1], lat = center[2]),
     zoom=7,
     maptype= 'terrain',
     source ='google') +
  geom_point(data=df, mapping =aes(x=경도, y= 위도  , color = Kind ), shape= 19 , size =2) +
  theme(legend.position = 'top')


#showing seoul 
qmap(location = c(lat=37.5, lon = 127),
     zoom=10,
     maptype= 'terrain',
     source ='google') +
  geom_point(data=df, mapping =aes(x=경도, y= 위도  , color = Kind ), shape= 19 , size =2) +
  theme(legend.position = 'top')

levels(df$Kind ) #"국민안심병원"        "선별진료소"          "승차검진_선별진료소" 

#order of the red, green , blue 

#6 cut only seoul for analysis 

seoul <- df %>% 
  filter(str_detect(주소, "서울특별시"))


head(seoul)

#7 draw a map for seoul 

center = c(lon=median(seoul$경도) , lat = median(seoul$위도))

qmap(location = c(lon= center[1], lat = center[2]),
     zoom=11,
     maptype= 'terrain',
     source ='google') +theme_minimal(base_family = "NanumGothic") +
  geom_point(data=seoul, mapping =aes(x=경도, y= 위도  , color = Kind ), shape= 19 , size =2) +
  theme(legend.position = 'top')


#save the info into a csv file for qgis 
write.csv(seoul,'seoul_hos.csv', row.names = F)



##############################################################################################
#BRING THE URL FOR MASK DATA



#1 make loop for collecting mask data 
page <-seq(1,55) #use all 55 pages

mask<-data.frame() #make an empty dataframe



for (i in page){
  print(i)
  
  #get url with page numbers 
  url<-paste('https://8oi9s0nnth.apigw.ntruss.com/corona19-masks/v1/stores/json?page=',i)
  #print(url)

  
  #get the json result 
  json_result<- GET(url=url, query= list(serviceKey=DATAGOKR_KEY %>% I() ))
  #json_result 
    
    
  #convert to text file 
  txtfile<- json_result %>% content(as= 'text', encoding = 'UTF-8') %>% fromJSON() 
  #txtfile
    
    #convert needed info to df 
  df<- txtfile$storeInfos
  df  
  mask <- rbind(mask,df)
  Sys.sleep(time=1 ) #서버를 생각해서, 1초 단위로 가져오기 
    #print(i)
    
  
  
}
  



#2 (55 pages) decrease dataset into locs only in seould 
head(mask)


#3 choose only 0622 data from the dataset 

seoul_mask <-mask %>% filter(str_detect(addr, '서울특별시'))

summary(seoul_mask)


#4 preprocess dataset 

seoul_mask =seoul_mask[,-2] #delete code 
seoul_mask =seoul_mask[,-5]
colnames(seoul_mask) = c('주소','lat','lon','name')


#5 use qmap to visualize

center = c(lon=median(seoul_mask$lon) , lat = median(seoul_mask$lat))

qmap(location = c(lat=37.55, lon = 127), #center 보다 이게 더 잘 그려짐 
     zoom=11,
     maptype= 'terrain',
     source ='google') +
  theme_minimal(base_family = "NanumGothic") +
  geom_point(data=seoul_mask, mapping =aes(x=lon, y= lat), shape= 19 , size =2) +
  theme(legend.position = 'top')


##############분석


#seoul_hos

seoul$gu<-substring(seoul$주소,7,9)
seoul$gu= ifelse(seoul$gu == '연희로','서대문구', seoul$gu) #wrong data
seoul$gu= ifelse(seoul$gu == '동대문','동대문구', seoul$gu)
seoul$gu= ifelse(seoul$gu == '영등포','영등포구', seoul$gu)
seoul$gu= ifelse(seoul$gu == '서대문','서대문구', seoul$gu)
seoul$gu = as.factor(seoul$gu) #25 levels 
seoul$gu 


#seoul_mask
seoul_mask$gu<-substring(seoul_mask$주소,7,9)
head(seoul_mask$gu)

seoul_mask$gu= ifelse(seoul_mask$gu == '동대문','동대문구', seoul_mask$gu)
seoul_mask$gu= ifelse(seoul_mask$gu == '영등포','영등포구', seoul_mask$gu)
seoul_mask$gu= ifelse(seoul_mask$gu == '서대문','서대문구', seoul_mask$gu)
seoul_mask$gu= ifelse(seoul_mask$gu == '중구 ','중구', seoul_mask$gu)

seoul_mask$gu = as.factor(seoul_mask$gu) #25 levels 
seoul_mask$gu 

str_trim


levels(seoul$gu  ) == levels(seoul_mask$gu )


#6 save dataset 

write.csv(seoul,'seoul.csv', row.names = F)
write.csv(seoul_mask,'seoul_mask.csv', row.names = F)




#Bar Plots for hos 
seoul_hos_gu<- table(seoul$gu)
sort(seoul_hos_gu,decreasing = T)
seoul_hos_gu_df <- as.data.frame(sort(seoul_hos_gu,decreasing = T))

colnames(seoul_hos_gu_df) <- c('Gu', 'Hos_num')


#use korean font :NanumGothic
ggplot(seoul_hos_gu_df, aes(x= Gu ,  y= Hos_num))+coord_flip() +
  theme_bw()+
  ggtitle('구별 코로나-19 관련 병원 수')+
  xlab('서울내의 구(25)')+ylab('병원 수')+
  theme_minimal(base_family = "NanumGothic") +
  geom_text(aes(label=Hos_num), size=3, hjust=-.35) + 
  geom_bar(stat = "identity",fill= 'pink')

#Bar Plots for mask 

seoul_mask_gu<- table(seoul_mask$gu)
sort(seoul_mask_gu,decreasing = T)
seoul_mask_gu_df <- as.data.frame(sort(seoul_mask_gu,decreasing = T))

colnames(seoul_mask_gu_df) <- c('Gu', 'pharm_num')


#use korean font 
theme_set(theme_grey(base_family='AppleMyungjo'))

ggplot(seoul_mask_gu_df, aes(x= Gu ,  y= pharm_num))+  coord_flip() +
  theme_bw()+
  ggtitle('구별 공적마스크 약국 수')+
  xlab('서울내의 구(25)')+ylab('약국 수')+
  theme_minimal(base_family = "NanumGothic") +
  geom_text(aes(label=pharm_num), size=3, hjust=-.01) + 
  geom_bar(stat = "identity",fill= 'skyblue')


