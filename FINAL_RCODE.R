setwd('C:/Users/최재영/Desktop/datascienceR2')

data<-wage

##0 seeing glimpse of data 

dim(wage)
summary(wage)
head(wage)




##1-1 "wage.RData”를 이용하여 다음의 변수를 종속변수와 독립변수로 하는 회귀분석을 실시

model1<- lm(wage~age, data=data)

summary(model1)

##1-2 단순회귀분석의 결과를 이용하여 아래의 질문에 대한 답을 제시하시오.

#가. 모형의 설명력을 나타내는 통계량의 값을 제시하고 그 결과를 해석하시오. 특히, 해당 통계량이 의미하는 것(어떤 모형과 비교한 결과인지를 반드시 제시할 것)

summary(model1) #F-statistic = 32.05



##comparing it with model0
model0<- lm(wage~1,data=data)
anova(model1,model0)  #you can see that F-statistic of model1 & anova's F is same 



#나. 임금에 대한 연령이 어떤 효과성이 있는지를 설명할 것. 설명시 반드시 그 이유를 제시할 것

summary(model1)

model1$coefficients





##2-1 “wage.RData”를 이용하여 다음의 변수를 종속변수와 독립변수로 하는 회귀분석을 실시

model2<- lm(wage~age+tenure, data=data)

summary(model2) ##F-statistic=60.67


##2-2 임금에 대한 연령과 경력이 어떤 효과성이 있는지를 설명할 것. 설명시 반드시 그 이유를 제시할 것


model2$coefficients



##3-1 단순회귀분석과 다중회귀분석 결과를 각기 다른 객체로 저장해서 두 모형이 임금에 대해 가지고 있는 설명력(분산)에 대한 검증을 실시하시오.

anova(model1,model2)


##3-2 검증 결과를 토대로 단순회귀분석과 다중회귀분석 중 어떤 모형이 더 유의미한지 설명하시오. 설명 시 그 근거를 제시하시오. 다만, 분산분석 결과만을 해석하지 말고 독립변수들의 회귀계수의 유의도를 종합적으로 판단하여 결론을 도출할 것

model3<- lm(wage~tenure, data=data)
summary(model3)

AIC(model1,model2)

