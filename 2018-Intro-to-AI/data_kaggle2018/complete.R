
require(plyr)
require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot) # correlation plot

setwd('C:/Users/최재영/Desktop/data')
train <- read.csv('train.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv', stringsAsFactors = FALSE)

## N/A 값 일괄 처리 train에만 saleprice존재 

## 데이터 합산 
sum(is.na(train$Id))
sum(is.na(train$SalePrice))
sum(is.na(test$Id))

combined.data<- rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))
dim(combined.data)



##N/A값 확인    ##n/a가 있는 열은 34개이다! 
n.a_column<- which(colSums(is.na(combined.data)) > 0)
length(n.a_column)
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)
    #0 MSZoning 항목 결측치 처리 

combined.data[is.na(combined.data$MSZoning),c('MSZoning','MSSubClass')]##1916,2217,2251,2905 행 결측치

table(combined.data$MSZoning, combined.data$MSSubClass) ##20의 경우 RL이 압도적으로 많음, 30,70은 RM/최빈값대체  


combined.data$MSZoning[c(2217, 2905)] = 'RL'
combined.data$MSZoning[c(1916, 2251)] = 'RM'

##확인 - MSZoning 관련 항목 n/a값 0
sum(is.na(combined.data$MSZoning))

    ##1 PoolQC(pool quality) 결측치 확인 

combined.data[is.na(combined.data$PoolQC),c('PoolQC','PoolArea')]



sum(is.na(combined.data$PoolQC))  ##has 2909 N.A(총데이터는 2919개, 그중 2909개가 결측치)

##poolarea(면적)가 0인 곳은 수영장이 없는 곳, 0보다 크다면 수영장이 있음!  

sum(combined.data$PoolArea>0 & is.na(combined.data$PoolQC)) ## 수영장이 존재하지만 품질이 n/a값인 곳=3개 

  ## finding the row with above 

combined.data[(combined.data$PoolArea > 0) & is.na(combined.data$PoolQC),c('PoolQC','PoolArea')]
      ## row 2421, 2504, 2600 have N/A in poolQC which have pool areas 

##pool area 평균 poolqc로 결측치 채우기 

combined.data[,c('PoolQC','PoolArea')] %>%  ##data의 poolqc, poolarea로 묶음 
    group_by(PoolQC)%>% ##poolQC를 기준으로 
  summarise(mean = mean(PoolArea), counts = n())  ## poolarea의 평균으로

## ex, fa, gd는 각각으로 나뉘어 등급 정해짐/ 결측치 존재하는 3개를 알맞게 채워준다! 

combined.data[(combined.data$PoolArea > 0) & is.na(combined.data$PoolQC),c('PoolQC','PoolArea')]

## 2421는 368이므로 ex에 가깝고, 2504는 444이므로 ex에 가깝다. 2600은 561으로 fa에 가까움 나머지 결측치는 none으로 채움 

combined.data[2421,'PoolQC'] = 'Ex'
combined.data[2504,'PoolQC'] = 'Ex'
combined.data[2600,'PoolQC'] = 'Fa'
combined.data$PoolQC[is.na(combined.data$PoolQC)] = 'None'
        ##확인하기 
sum(is.na(combined.data$PoolQC))
sum(is.na(combined.data$PoolArea))


    ##2 garage관련 n/a값 처리 (GarageYrBlt/GarageFinish/GarageQual/GarageCond/GarageType/GarageArea)

## 집 yearbuilt 데이터 = garage built year와 동일할 것 - garageyrblt 처리 XX -> 창고가 없는 곳이라 N/A였음 -처리 
length(which(combined.data$GarageYrBlt == combined.data$YearBuilt)) ##2919 obs 중 2216이 동일

na_garageyrblt <- which(is.na(combined.data$GarageYrBlt))

sum(is.na(combined.data$GarageYrBlt)) 
combined.data[is.na(combined.data$GarageYrBlt)==T,]
combined.data[na_garageyrblt, 'GarageYrBlt'] <- 0
      ##확인하기 
sum(is.na(combined.data$GarageYrBlt))

## merginge n/a left garage data 
garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')

  #garagecond의 결측치 확인
combined.data[is.na(combined.data$GarageCond),garage.cols] ##2127열만 garage area, garagecar, type 표기 , 그외는 garage가 없는것 

##2127열과 비슷한 값 구하기 
a <- which(((combined.data$GarageArea < 370) & (combined.data$GarageArea > 350)) & (combined.data$GarageCars == 1))

sapply(combined.data[a, garage.cols], function(x) sort(table(x))) 
##garagequal:TA   GarageFinish:Unf GarageCond:TA 가 많은 값으로 보임 
 
## 2127열을 채운다! 

combined.data[2127,'GarageQual'] = 'TA'
combined.data[2127, 'GarageFinish'] = 'Unf'
combined.data[2127, 'GarageCond'] = 'TA'

##그외 값 채우기   
###숫자는 0으로 채우고, N.A는 none으로 채움 -> 열이 0이 있으면 그 외 0으로 채우고, N/A면 none으로 채움 
#####2577은 N.A 임 -> none으로 표기 
for (col in garage.cols){
  if (sapply(combined.data[col], is.numeric) == TRUE){
    combined.data[sapply(combined.data[col], is.na), col] = 0
  }
  else{
    combined.data[sapply(combined.data[col], is.na), col] = 'None'
  }
}

##확인 - garage/pool 관련 모두 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    ##3kitchenqual / electrical 모두 1개 씩 n/a값이 있음 - 가장 가까운 값 대체 

combined.data[is.na(combined.data$KitchenQual)==T,] ##1556행이 na 값 존재  ->이때 kitchenAbvGr이 1 
one<-combined.data[combined.data$KitchenAbvGr==1,c("KitchenQual","KitchenAbvGr")]

table(one)  ## TA가 가장 많기에 TA로 대체 

combined.data$KitchenQual[is.na(combined.data$KitchenQual)] = 'TA'


combined.data[is.na(combined.data$Electrical)==T,] ##1380행이 na / 연관있는 변수가 없어 보여 최빈값 대체 

str(combined.data$Electrical)
length(which(combined.data$Electrical=='SBrkr')) ##2919개 중 2671개가 sbrkr이므로 대체 
length(which(combined.data$Electrical=='FuseA'))
       
combined.data$Electrical[is.na(combined.data$Electrical)] = 'SBrkr'

##확인 - electrial/ kitchenqual n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    ##4 basement 관련 항목 N/A값 처리해보자! 

bsmt.column<-c('BsmtExposure','BsmtQual', 'BsmtCond', 'BsmtFinType1', 'BsmtFinSF1', 'BsmtFinType2','BsmtFinSF2', 'BsmtUnfSF' ,'TotalBsmtSF', 'BsmtFullBath' ,'BsmtHalfBath')

##bsmtexposure 이 결측치인 것
combined.data[is.na(combined.data$BsmtExposure),bsmt.column]

##949, 1488, 2349 row has N/A but others are filled 

B <- which(((combined.data$BsmtQual =='Gd'& combined.data$BsmtCond=='TA')))

sapply(combined.data[B, bsmt.column], function(x) sort(table(x)))###BsmtExposure 대부분 No 

combined.data[c(949, 1488, 2349), 'BsmtExposure'] = 'No' 


##그 외 N/A 값은 bsmt가 없는 것 

###숫자형변수가 있으면, na값에 0 대입, 그외는none으로 대입
for (col in bsmt.column){
  if (sapply(combined.data[col], is.numeric) == TRUE){
    combined.data[sapply(combined.data[col], is.na),col] = 0
  }
  else{
    combined.data[sapply(combined.data[col],is.na),col] = 'None'} } ##2121 row has only N.A

##확인 - bsmt 관련 항목 n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    ##5exterior1st and Exterior2nd은 n/a값이 각각 하나씩 있다. 

exterior.column<-c('Exterior1st','Exterior2nd')

combined.data[is.na(combined.data$Exterior1st),exterior.column]
combined.data[is.na(combined.data$Exterior2nd),exterior.column] #2152가 두 값 모두 n/a 이다 


##exterior 관련 항목이 없으므로 other으로 표기 

combined.data$Exterior1st[is.na(combined.data$Exterior1st)] = 'Other'
combined.data$Exterior2nd[is.na(combined.data$Exterior2nd)] = 'Other'


##확인 - exteiror 관련 항목 n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    #6 SaleType, Functional Utilities 항목 N/A 처리 
##salestype-salescondition과 연관지어 처리 

combined.data[is.na(combined.data$SaleType),'SaleCondition'] ##2490 행이 결측치 

table(combined.data$SaleCondition, combined.data$SaleType) ###Salecondition=normal은 대부분 saletype가 WD(2314개)

combined.data$SaleType[is.na(combined.data$SaleType)] = 'WD'

##확인 - SaleType 관련 항목 n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

##functional 관련 항목 2개의 결측값 
  
combined.data[is.na(combined.data$Functional),]   ##2217, 2474 행이 결측값 -관련있는 항목부재-최빈값대체 

table(combined.data$Functional) 
combined.data$Functional[is.na(combined.data$Functional)] = 'Typ'


##utilities 항목 결측값 2개 
combined.data[is.na(combined.data$Utilities),] ##1916,1946 행 결측
table(combined.data$Utilities)    #1개빼고 allpub으로, 최빈값 대체 

## outlier 찾기 
combined.data[(combined.data$Utilities=='NoSeWa'),] #945행 

combined.data$Utilities[is.na(combined.data$Utilities)] = 'AllPub'
combined.data<-combined.data[-945,]

##확인 - utilities 관련 항목 n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)



    
    #7 MasVnrArea,MasVnrType 처리 

## 하나라도 N/A 존재하면 출력 :: n/a만 있는 값은 벽돌(?) 없는 것 
combined.data[is.na(combined.data$MasVnrType) | is.na(combined.data$MasVnrArea),c('MasVnrType','MasVnrArea')] ##2611만 area값 존재 

## N/A 값만 있는 곳 처리 
combined.data$MasVnrType[is.na(combined.data$MasVnrType)] = 'None'
combined.data$MasVnrArea[is.na(combined.data$MasVnrArea)] = 0

##2611행처리 -- area면적에 따라 결측치 채우기! 
c<-na.omit(combined.data[,c('MasVnrType','MasVnrArea')]) 

C <- which((c$MasVnrArea < 206) & (c$MasVnrArea > 190)) ##198과 가까운 area넓이의 최빈 type 찾기 

sapply(combined.data[C, ], function(x) sort(table(x))) ##BrkFace가 41로 많음 

combined.data[2611, 'MasVnrType'] = 'BrkFace'


##확인 - masvnr 관련 항목 n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    #8 LotFrontage: Linear feet of street connected to property 결측치 

##비슷한 동네에 비슷한 property가 있을 것이다. 동네를 범주형자료로 처리! 

combined.data['Nbrh.factor'] <- factor(combined.data$Neighborhood, levels = unique(combined.data$Neighborhood))

combined.data$Neighborhood

lot.neighbor <- combined.data[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage,na.rm = TRUE)) ##na 반드시 제거하여 중간값 구한다 

lot.neighbor ###각 동네의 lotfrontage 중앙값을 구함 


d = which(is.na(combined.data$LotFrontage)) #lotFrontage가 NA인 값들에 관해 

for (i in d){
  lot.median <- lot.neighbor[lot.neighbor == combined.data$Neighborhood[i],'median']  ##i행의 동네와 같다면, 중앙값 출력 
  combined.data[i,'LotFrontage'] <- lot.median[[1]]}  ##lot.median은 같은 동네=중앙값 동일, lotgrontage에 중앙값 넣기  

##확인 - lotgrontage 관련 항목 n/a값 0
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    #9 Fence: Fence quality N/A 처리 -관련 데이터 없음 (그냥 fence가 없는 것으로 처리)
str(combined.data$Fence)
table(combined.data$Fence) ##없음 항목이 없다! 

combined.data$Fence[is.na(combined.data$Fence)] = 'None'


##확인 - fence 관련 항목 n/a값 = 없음으로 처리 /data설명 
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

    #10 MiscFeature 관련 항목 n/a 처리/data설명에 n/a=none이라고 함  

combined.data$MiscFeature[is.na(combined.data$MiscFeature)] = 'None'

##확인 - MiscFeature 관련 항목 n/a값 = 없음으로 처리 /data설명 
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)


    #11 fireplace 관련; Fireplaces: Number of fireplaces, FireplaceQu: Fireplace quality -fireplace가 있어야 품질이 있지

which((combined.data$Fireplaces > 0) & (is.na(combined.data$FireplaceQu))) ##fireplace가 있는데, qu가 na처리된 것은 없다! 모두 fireplace가 없는것 

combined.data$FireplaceQu[is.na(combined.data$FireplaceQu)] = 'None'

##확인 - FireplaceQu 관련 항목 n/a값 = 없음으로 처리 /data설명 
sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)


    #12 Alley 관련 ; data설명에서 N/A는 alley 접근 어려운 곳이라고 함 

combined.data$Alley[is.na(combined.data$Alley)] = 'None'

##확인 - Alley 관련 항목 n/a값 = 없음으로 처리 /data설명 

sort(colSums(sapply(combined.data[n.a_column], is.na)), decreasing = TRUE)

###############N/A 최종 확인 
sum(is.na(combined.data))










##더 깔끔하게 정리 




##더 깔끔하게 정리 

num_features <- names(which(sapply(combined.data, is.numeric)))
cat_features <- names(which(sapply(combined.data, is.character)))

numberic.data <- combined.data[num_features]  #수치형자료만을 가지고 만든 데이터 


#####팩터화 


##qual 열은 모두 none, po,fa,ta,gd,ex로 구성-> 카테고리로 분류 
# 11.22 poolqc, BsmtCond 추가
qual.cols <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual', 'BsmtCond','PoolQC')

qual.list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

map <- function(cols, list, df){
  for (i in cols){
    df[i] <- as.numeric(list[combined.data[,i]])
  }
  return(df)
}   ##cols의 i 에 데이터의 i열을 수치형변수로 리스트한 df를 만들어라 

numberic.data <- map(qual.cols, qual.list, numberic.data) #각각대응


####Bsmtexposure 팩터화 
table(combined.data$BsmtExposure)
group.prices('BsmtExposure')
bsmt.list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)  #bsmt의 경우 이렇게 점수 부여
numberic.data <- map(c('BsmtExposure'), bsmt.list, numberic.data)

#####bsmtfintype 팩터화 변수설명 참고 
table(combined.data$BsmtFinType1)
table(combined.data$BsmtFinType2)

bsmt.fin.list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
numberic.data <- map(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list, numberic.data)

## functional 팩터화 

functional.list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4, 'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)
numberic.data <- map('Functional', functional.list, numberic.data)

## garage 팩터화 
table(combined.data$GarageFinish)
garage.fin.list <- c('None' = 0,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)
numberic.data <- map('GarageFinish', garage.fin.list, numberic.data)

## fence 팩터화
table(combined.data$Fence)

fence.list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)
numberic.data <- map('Fence', fence.list, numberic.data)

# 2918   52

library(plyr)
## MSdwelling 팩터화

# 오류의 주범 (아래)
numberic.data$MSSubClass <- as.factor(numberic.data$MSSubClass)
numberic.data$MSSubClass <- revalue(numberic.data$MSSubClass, c('20' = 1, '30' = 0, '40' = 0, 
                                                                '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, 
                                                                '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0))
numberic.data$MSSubClass <- as.numeric(gsub("2","0",numberic.data$MSSubClass))
numberic.data$MSSubClass <- as.numeric(numberic.data$MSSubClass)  # 1 아니면 2로 바뀜
sum(is.na(numberic.data$MSSubClass))


## MSZoning
table(combined.data$MSZoning)
aggregate(SalePrice~MSZoning, train, mean)
zoning.list <- c('C (all)' = 0, 'RH' = 1, 'FV' = 2, 'RM' = 3, 'RL' = 4)
numberic.data <- map('MSZoning', zoning.list, numberic.data)
sum(is.na(numberic.data$MSZoning))

## LotShape
table(combined.data$LotShape)
aggregate(SalePrice~LotShape, train, mean)
char.list <- c('IR3' = 2, 'IR2' = 3, 'IR1' = 1, 'Reg' = 0)
numberic.data <- map('LotShape', char.list, numberic.data)

## LandContour
table(combined.data$LandContour)
aggregate(SalePrice~LandContour, train, mean)
char.list <- c('Low' = 3, 'Bnk' = 1, 'HLS' = 4, 'Lvl' = 2)
numberic.data <- map('LandContour', char.list, numberic.data)


#drop
col.drops <- c('Utilities')
numberic.data <- numberic.data[,!names(numberic.data) %in% c('Utilities')]

## LotConfig
table(combined.data$LotConfig)
aggregate(SalePrice~LotConfig, train, mean)
char.list <- c('FR3' = 4, 'FR2' = 2, 'CulDSac' = 5, 'Corner' = 3, 'Inside' = 1)
numberic.data <- map('LotConfig', char.list, numberic.data)

## LandSlope
table(combined.data$LandSlope)
aggregate(SalePrice~LandSlope, train, mean)
char.list <- c('Sev' = 3, 'Mod' = 2, 'Gtl' = 1)
numberic.data <- map('LandSlope', char.list, numberic.data)

## Neighborhood  # 가격별로 아주좋음(4) 좋음(3) 보통(2) 나쁨(1) 별로(0)
table(combined.data$Neighborhood)
aggregate(SalePrice~Neighborhood, train, mean)
char.list <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
               'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
               'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
               'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
               'NridgHt' = 4)
numberic.data <- map('Neighborhood', char.list, numberic.data)

## Condition1
table(combined.data$Condition1)
aggregate(SalePrice~Condition1, train, mean)
char.list <- c('RRAe'=1, 'Feedr'=2, 'Artery'=3, 'Norm'=4, 'RRAn'=5,'RRNn'=6, 'RRNe'=7, 'PosN'=8, 'PosA'=9)
numberic.data <- map('Condition1', char.list, numberic.data)

## Condition2
table(combined.data$Condition2)
aggregate(SalePrice~Condition2, train, mean)
char.list <- c('RRAe'=1, 'Feedr'=2, 'Artery'=3, 'Norm'=4, 'RRAn'=5,'RRNn'=6, 'RRNe'=7, 'PosN'=8, 'PosA'=9)
numberic.data <- map('Condition2', char.list, numberic.data)

## BldgType 
table(combined.data$BldgType)
aggregate(SalePrice~BldgType, train, mean)
char.list <- c('1Fam'=5, '2fmCon'=1, 'Duplex'=3,  'Twnhs'=2, 'TwnhsE'=4) 
numberic.data <- map('BldgType', char.list, numberic.data)

## HouseStyle  # 가격별로 높->낮(1) 
table(combined.data$HouseStyle)
aggregate(SalePrice~HouseStyle, train, mean)
char.list <- c('1.5Fin'=2, '1.5Unf'=1, '1Story'=3, '2.5Fin'=4, '2.5Unf'=2, '2Story'=4, 'SFoyer'=1, 'SLvl'=3)
numberic.data <- map('HouseStyle', char.list, numberic.data)

## RoofStyle  # 가격별 1~3
table(combined.data$RoofStyle)
aggregate(SalePrice~RoofStyle, train, mean)
char.list <- c('Flat'=3,'Gable'=2, 'Gambrel'=1, 'Hip'=3,  'Mansard'=2, 'Shed'=1)
numberic.data <- map('RoofStyle', char.list, numberic.data)

## RoofMatl # 가격별 1~4
table(combined.data$RoofMatl)
aggregate(SalePrice~RoofMatl, train, mean)
char.list <- c('ClyTile'=1, 'CompShg'=2, 'Membran'=4, 'Metal'=2, 'Roll'=1, 'Tar&Grv'=3, 'WdShake'=3, 'WdShngl'=4)
numberic.data <- map('RoofMatl', char.list, numberic.data)

## Exterior1st # 가격별 1~4
table(combined.data$Exterior1st)
aggregate(SalePrice~Exterior1st, train, mean)
char.list <- c('AsbShng'=1, 'AsphShn'=1, 'BrkComm'=2, 'BrkFace'=3,  'CBlock'=4, 'CemntBd'=4,
               'HdBoard'=2, 'ImStucc'=2, 'MetalSd'=1, 'Other'=4, 'Plywood'=3, 'Stone'=2,  
               'Stucco'=3, 'VinylSd'=3, 'Wd Sdng'=1, 'WdShing'=3) 
numberic.data <- map('Exterior1st', char.list, numberic.data)

## Exterior2nd
table(combined.data$Exterior2nd)
aggregate(SalePrice~Exterior2nd, train, mean)
char.list <- c('AsbShng'=1, 'AsphShn'=1, 'Brk Cmn'=2, 'BrkFace'=3,  'CBlock'=4, 'CmentBd'=4,
               'HdBoard'=2, 'ImStucc'=2, 'MetalSd'=1, 'Other'=4, 'Plywood'=3, 'Stone'=2,  
               'Stucco'=3, 'VinylSd'=3, 'Wd Sdng'=1, 'Wd Shng'=3)
numberic.data <- map('Exterior2nd', char.list, numberic.data)
sum(is.na(numberic.data$Exterior2nd))

## MasVnrType
table(combined.data$MasVnrType)
aggregate(SalePrice~MasVnrType, train, mean)
char.list <- c('BrkCmn'=1, 'BrkFace'=2, 'None'=0, 'Stone'=3) 
numberic.data <- map('MasVnrType', char.list, numberic.data)

## Foundation  # 가격별 1~3
table(combined.data$Foundation)
aggregate(SalePrice~Foundation, train, mean)
char.list <- c('BrkTil'=2, 'CBlock'=2, 'PConc'=3, 'Slab'=1,'Stone'=3,'Wood'=1) 
numberic.data <- map('Foundation', char.list, numberic.data)

## Heating  # 가격별 1~3
table(combined.data$Heating)
aggregate(SalePrice~Heating, train, mean)
char.list <- c('Floor'=2, 'GasA'=2,  'GasW'=3, 'Grav'=1,'OthW'=3, 'Wall'=1)
numberic.data <- map('Heating', char.list, numberic.data)

## CentralAir
table(combined.data$CentralAir)
aggregate(SalePrice~CentralAir, train, mean)
char.list <- c('N'=0,'Y'=1)
numberic.data <- map('CentralAir', char.list, numberic.data)

## Electrical
aggregate(SalePrice~Electrical, train, mean)
char.list <- c('Mix' = 1, 'FuseP' = 2, 'FuseF' = 3, 'FuseA' = 4, 'SBrkr'=5)
numberic.data <- map('Electrical', char.list, numberic.data)

## GarageType
aggregate(SalePrice~GarageType, train, mean)
char.list <- c('None' = 0, 'CarPort' = 1, '2Types' = 2, 'Basment' = 2, 'BuiltIn' = 3, 'Detchd' = 1, 'Attchd' = 3)
numberic.data <- map('GarageType', char.list, numberic.data)
head(numberic.data$GarageType)
sum(is.na(numberic.data$GarageType))

## PavedDrive # what does it mean?
aggregate(SalePrice~PavedDrive, train, mean)
char.list <- c('P' = 0, 'N' = 0, 'Y' = 1)
numberic.data <- map('PavedDrive', char.list, numberic.data)

## MiscFeature # 가격별 1~4
aggregate(SalePrice~MiscFeature, train, mean)
char.list <- c('None' = 0, 'TenC' = 4, 'Othr' = 1, 'Gar2' = 3, 'Shed' = 2)
numberic.data <- map('MiscFeature', char.list, numberic.data)

## SaleType # 가격별 1~3
table(combined.data$SaleType)
aggregate(SalePrice~SaleType, train, mean)
char.list <- c('Con' = 3, 'Oth' = 1, 'ConLw' = 1, 'WD'=2, 'COD'=2,
               'ConLI' = 2, 'CWD' = 3, 'ConLD' = 1, 'New'=3)
numberic.data <- map('SaleType', char.list, numberic.data)
sum(is.na(numberic.data$SaleType))

## SaleCondition  # 다른 사람 참고
aggregate(SalePrice~SaleCondition, train, mean)
char.list <- c('Abnorml'=1, 'Alloca'=2, 'AdjLand'=3, 'Family'=4, 'Normal'=5, 'Partial'= 0)
numberic.data <- map('SaleCondition', char.list, numberic.data)


#### ONEHOT

## Street 
table(combined.data$Street)
char.list <- c('Pave' = 0, 'Grvl' = 1)
numberic.data <- map('Street', char.list, numberic.data)

## Alley
char.list <- c('None' = 0, 'Pave' = 0, 'Grvl' = 1)
numberic.data <- map('Alley', char.list, numberic.data)

#### CHECK

dim(numberic.data)  # 2918 79 util 빼면 78


##데이터 합치기/처음에 rbind로 train부터 했기때문에 이렇게 뽑아내기 가능 

##training data 추출
train.data <- cbind(numberic.data[1:1460,], train['SalePrice'])   
head(train.data)  

test.data <- cbind(numberic.data[1461:2919,])   

sum(is.na(numberic.data))



#### Data Preprocessing

all_data <- numberic.data


# transform SalePrice target to log form
train$SalePrice <- log(train$SalePrice + 1)

# for numeric feature with excessive skewness, perform log transformation
# first get data type for each feature
feature_classes <- sapply(names(all_data),function(x){class(all_data[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character"])

num_features <- names(which(sapply(all_data, is.numeric)))
cat_features <- names(which(sapply(all_data, is.character)))



# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

library(timeDate)
# determine skew for each numeric feature
num_features <- names(which(sapply(all_data, is.numeric)))
skewed_feats <- sapply(num_features, function(x){skewness(all_data[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  all_data[[x]] <- log(all_data[[x]] + 1)
}


# 이것 때문인가? categori 에러
col.drops <- c('Utilities')
character.data <- character.data[,!names(numberic.data) %in% c('Utilities')]


# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])


X_train <- all_data[1:nrow(train),]
X_test <- all_data[(nrow(train)+1):nrow(all_data),]

y <- train$SalePrice  #y는 column이 아니라 숫자여야함

X_train$SalePrice<-y

################################################전처리완료 

#####jaeyoung modeling#####

colSums(is.na(X_train))

#1modeling-stepwise

library(leaps)


a<-lm(SalePrice~.,data=X_train)
summary(a)

b1<-step(a,direction = 'backward')

