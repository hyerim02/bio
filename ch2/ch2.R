#CH2. 데이터 분석의 시작
library(tidyverse)
dat <- read.csv("C:\\Users\\phl02\\Desktop\\P\\bio\\ch2\\Ch2_chb.csv")
class(dat) 
## 3.1 데이터 훑어보기
View(dat)
### 크기 확인
dim(dat)
### 열 이름 확인
colnames(dat)
### 행 이름 확인
rownames(dat) 
### 기본 기조 확인인
str(dat)

## 3.2 데이터 요약 보기
### 첫 6줄 확인
head(dat)
### 첫 10줄 확인
head(dat, 10)
### 마지막 6줄 확인
tail(dat)
### 마지막 8줄 확인
tail(dat,8)
### 패키지로 요약하기
glimpse(dat)

# 4 데이터 인덱싱, 슬라이싱
## 열 선택하기
#dat[  ,age]==>에러
dat[  , 'age']  
dat[, 4]
## $ㅣ이용
dat$age  
## 비교하기
a1 <- dat[ ,'age'] 
a1
class(a1)  

b1 <- dat$age
b1
class(b1)

## 데이터 프레임?벡터?
is.data.frame(b1)
is.vector(b1)
## []이용할 때 벡터로 반환환
a2 <-dat[ ,'age', drop=TRUE]
a2
class(a2)

## 4.2 Column 여러개 선택
### 열 선택
dat[ ,c('age','treat_gr','lc','hcc')]  
var <- c('age','treat_gr','lc','hcc')
dat[ ,var]
dat[ , c(4,6,7,8)] 
# 필요한 열 선택
temp <- dat[ ,c(1:10, 12:20)]
head(temp)
temp <- dat[ , -11]
head(temp)

### 2.4.3 Row 한개 선택

pt1 <- dat[1, ]  
pt1

### 2.4.4 Row 여러개 선택

pt2 <- dat[1:4, ]
pt2

pt <- dat[c(1,4,9), ]
pt           


### 2.4.5 Row, column 동시 선택

temp <- dat[c(1:6), c(1:8)]
temp
dim(temp)   
temp <- dat[c(1:6,11:16), c(1:8)]
head(temp, 12) 
temp <- dat[c(1:6), c(1:6, 12:18)]
head(temp)
temp <- dat[c(1:6, 16:21), c(1:6, 12:18)]
head(temp,10)

### 2.4.6 특정 조건으로 선택하기

dat[12, 'alt']
dat[dat$age>=50, ]
dat$age[dat$age>=50]
dat[dat$age>=50 & dat$gender=="M", ]

### 2.4.7 Subset

subset(dat, subset=(age>=50 & gender=='M'))
subset(dat, select=c('lc','alt','bil','inr','alb'))
subset(dat, 
       subset=(age>=50 & lc==1 & gender=='M'),
       select=c('id','alb','bil','cr'))
high.risk<-subset(dat, 
                  subset=(age>=50 & lc==1 & gender=='M'),
                  select=c('id','alb','bil','cr'))
high.risk

#2.5 Factor 다루기
##2.5.1 Factor로 변환하기

dat1<-dat
class(dat1$gender)
table(dat1$gender)

dat1$sex<-factor(dat1$gender)
class(dat1$sex)
table(dat1$sex)

dat1$sex1<-as.factor(dat1$gender)
class(dat1$sex1)
table(dat1$sex1)

dat1$sex2<-as_factor(dat1$gender)
class(dat1$sex2)
table(dat1$sex2)

##2.5.2 Factor level 이해하기

dat1$risk_gr1<-as.factor(dat1$risk_gr)
levels(dat1$risk_gr1)

dat1$risk_gr2<-factor(dat1$risk_gr,
                      levels=c('low','intermediate','high'))
levels(dat1$risk_gr2)

##2.5.3 forcat 패키지

dat1$region<-factor(dat1$region)
table(dat1$region)
## 빈도순으로 level 정하기
dat1$region1<-fct_infreq(dat1$region)
levels(dat1$region1)
## 원하는 값으로 level 변경
dat1$region2 <-fct_recode(dat1$region,
                          'north' = 'seoul',
                          'north' = 'incheon',
                          'south' = 'gwangju',
                          'south' = 'jeju',
                          'south' = 'sejong',
                          'south' = 'daejun',
                          'east' = 'daegu',
                          'east' = 'busan')
## 여러 개 level병합
dat1$region3 <- fct_collapse(dat1$region,
                             'north' = c('seoul','incheon'),
                             'south' = c('gwangju','jeju','sejong','daejun'),
                             'east' = c('daegu','busan'))
dat1 %>% 
  count(region, region2, region3)
## 여러 개 level병합하되, 다수만 남기고 싶을 때
dat1$region4 <- fct_lump(dat1$region, n=4)

dat1 %>% 
  count(region,region4)
dat1$region4 <- fct_lump(dat1$region, n=2)

dat1 %>% 
  count(region,region4)

##2.6 기술 통계
## 평균,중앙값,표준편차,분산 등등
mean(dat$age)
median(dat$age)
sd(dat$age)
var(dat$age)
min(dat$age)
max(dat$age)
range(dat$age)
IQR(dat$age)
quantile(dat$age)
quantile(dat$age, prob=c(0.3,0.6)) 
quantile(dat$age, prob=c(1:10/10))
prob <- c(1:10/10)  
## 요약
summary(dat$age)
## Hmisc 
Hmisc::describe(dat$age)
## 데이터 종류 및 자료 형태 확인
str(dat)
## 간단하게 확인
sapply(dat, class)
##자료형태 확인
class(dat$age)
## 자료형태 확인
is.character(dat$age)  
is.character(dat$gender)
is.numeric(dat$age)  
is.numeric(dat$gender)
is.integer(dat$age) 

##2.7 데이터 수정 및 결측치

class(dat$dna)
dat$dna

## 미검출 데이터 0으로 변경
dat[dat$dna=='undetectable',]   
dat[19, 'dna'] <- '0'  
dat$dna

##종류 확인
class(dat$dna)    
dat$dna <- as.numeric(dat$dna) 
class(dat$dna)   

##2.7.1 결측값 확인하기
##결측값을 제외하고 평균 계산
mean(dat$alb, na.rm=T)
## 결측값이 존재하는지 확인
is.na(dat$alb) 
## 결측값 개수 구하기
sum(is.na(dat$alb))

##2.7.2 결측값 한번에 확인하기
## 전체 데이터 한 번에 확인
rowSums(is.na(dat))
## 그래프로 확인
barplot(rowSums(is.na(dat)))
## apply 함수와 barplot이용하기
na.count <- apply(dat, 2, function(x) sum(is.na(x)))
na.count
barplot(na.count[na.count>0]) 

##2.7.3 VIM 패키지 이용하기

library(VIM)

missing <- aggr(dat, col=c('navyblue','yellow'),
                numbers=TRUE, sortVars=TRUE,
                labels=names(dat1), cex.axis=.7,
                gap=3,
                ylab=c('Missing data','Pattern'))
missing

##2.8.1 tapply

tapply(dat$age, dat$gender, mean)
## tidyverse
dat %>% 
  group_by(gender) %>% 
  summarise(mean(age))

##2.8.2 sapply

sapply(dat, class)

##2.8.3 lapply

lapply(dat[,c('age','gender')],class)
##2.9 if, for
##2.9.1 If

age<-60

if(age>=50){
  print('old age')
}else{
  print('young age')
}

age<-45
if(age>=50){
  print('old age')
}else if(age<30){
  print('young age')
}else{
  print('middle age')
}

age<-c(40, 50, 60)
ifelse(age<40, 'young', 'old')

##2.9.2 for

for(i in 1:10){
  print(i)
}
for(i in 1:10){
  if(i %% 2 ==0){
    print(i)
  }
}

##2.10 중복 결과값 다루기
##2.10.1 Unique

dat$cr
length(dat$cr)
unique(dat$cr)
length(unique(dat$cr))

unique(dat$gender, dat$cr)

unique(dat[,c('gender','cr')])

dat %>% 
  distinct(gender, cr)

##2.10.2 Duplicated

dat$gender
duplicated(dat$gender)

temp<-rbind(dat, dat[c(3,6,10),])
dim(temp)
tail(temp)

temp$id
duplicated(temp$id)
!duplicated(temp$id)

temp1<-temp[!duplicated(temp$id),]
unique(temp1$id)

##2.11 Table 1 만들기
##2.11.1 moonBook 패키지

library(moonBook)
mytable(treat_gr~age+gender+lc+alt+bil+
          inr+cr+plt+alb+eag+dna_log,
        data=dat)

mytable(treat_gr~age+gender+lc+alt+bil+
          inr+cr+plt+alb+eag+dna_log, 
        data=dat, method=2)

mytable(treat_gr~age+gender+lc+alt+bil+
          inr+cr+plt+alb+eag+dna_log, 
        data=dat, show.total=T)

mytable(~age+gender+lc+alt+bil+
          inr+cr+plt+alb+eag+dna_log, 
        data=dat)  

table1 <-mytable(treat_gr~age+gender+lc+alt+bil+
                   inr+cr+plt+alb+eag+dna_log, 
                 data=dat, method=2)
mycsv(table1, file='C:\\Users\\phl02\\Desktop\\P\\bio\\ch2\\table1.csv',row.names=FALSE)

##2.11.2 tableone 패키지

library(tableone)
listVars <- names(dat[, c('age','gender','lc','alt','bil',
                          'inr','cr','plt','alb','eag','dna_log')]) 
catVars <- c('gender','lc') 
table1 <- CreateTableOne(vars = listVars,
                         factorVars = catVars,
                         strata = c('treat_gr'),
                         data = dat)
table1
summary(table1)

write.csv(print(table1), file='C:\\Users\\phl02\\Desktop\\P\\bio\\ch2\\tableone.csv',  
          row.names=TRUE,col.names =TRUE)   

##2.11.3 gtsummary 패키지

library(gtsummary)

dat.temp<-dat %>%       
  select(treat_gr,gender, age, lc, bil, inr, cr, dna_log) 
#기본 테이블
tbl_summary(dat.temp)
#두 그룹으로 나누기
tbl_summary(dat.temp, by=treat_gr)
#결측값 숨기기
tbl_summary(dat.temp, by=treat_gr, missing='no')
#결측값 다른 문구로 표시
tbl_summary(dat.temp, by='treat_gr', missing_text='missing value')
#평균과 표준편차로 나타내기
tbl_summary(dat.temp, by=treat_gr,
            statistic=all_continuous()~ '{mean} \u00b1 {sd}',
            missing='no')
#p값 표시
tbl_summary(dat.temp, by=treat_gr,
            missing='no') %>%
  add_p()
#전체 N수 표시
tbl_summary(dat.temp, by=treat_gr,
            missing='no') %>%
  add_n()
#전체 환자 특성 같이 보여주기
tbl_summary(dat.temp, by=treat_gr,
            missing='no') %>%
  add_overall()
#테이블 제목 변경
dat.temp %>% 
  tbl_summary(by=treat_gr,
              missing='no') %>% 
  modify_caption('**Baseline Characteristics**')
#테이블 소제목 변경
dat.temp %>% 
  tbl_summary(by=treat_gr,
              missing='no') %>% 
  modify_spanning_header( c('stat_1','stat_2')~'**Antiviral Treatment**') 
#최종 출판용으로 정리된 테이블
tbl_summary(dat.temp,
            by=treat_gr,
            missing='no') %>% 
  add_p() %>% 
  add_overall() %>% 
  modify_spanning_header( c('stat_1','stat_2')~'**Antiviral Treatment**') %>% 
  modify_caption('**Baseline Characteristics**')



