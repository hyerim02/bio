#CH3. 데이터 핸들링
library(tidyverse)
dat <- read.csv("C:\\Users\\phl02\\Desktop\\P\\bio\\ch3\\Ch3_chb.csv")
dat1 <- dat 

class(dat1) 

dim(dat1)  

colnames(dat1)


#3.2 Select
## 원하는 변수 선택 
dat1 %>% 
  select(id, b_alt, b_bil, b_alb) %>%  
  head(n=4)

dat1[ , c('id','b_alt','b_bil','b_alb')]

dat1 %>% 
  select(1, 8, 9, 13) %>% 
  head(n=4)

dat1 %>% 
  select(1:4, 7:9, 13) %>% 
  head(n=4)

dat1 %>% 
  select(m6_alt:m6_plt) %>% 
  head(n=4)

## 특정 변수 제외하기  
dat1 %>% 
  select(-id, -index_date) %>% 
  colnames()

colnames(dat1)

dat1 %>% 
  select(-1, -2) %>%     
  colnames()

dat1 %>% 
  select(-(1:4), -(10:13)) %>%  
  ncol()

## 같은 특성을 가진 변수만 선택 
dat1 %>% 
  select(b_alt, m6_alt, m12_alt) %>% 
  head(n=4)

dat1 %>% 
  select(contains('alt')) %>%   
  head(n=4)

dat1 %>% 
  select(contains('m6')) %>% 
  head(n=4)

dat1 %>% 
  select(starts_with('m6')) %>%   
  head(n=4)

dat1 %>% 
  select(ends_with('cr')) %>%    
  head(n=4)

## 변수 이름 변경  
dat1 %>% 
  select(sex = gender) %>% 
  head(n=4)

dat1 %>% 
  rename(sex = gender) %>% 
  head(n=4)

dat1 %>% 
  rename(emr_id = id, baseline_date = index_date, sex = gender) %>% 
  colnames()

## 변수의 순서 변경
dat1 %>% 
  select(treat_gr, hcc, id, everything()) %>%   
  colnames()

## 동시에 기능 사용하기
dat1 %>% 
  select(id, gender, contains('m12')) %>%
  rename(sex = gender) %>%
  select(sex, everything()) %>%
  head(n=4)

dat1 %>% 
  select(sex = gender, id, contains('m12')) %>%
  head(n=4)

dat1 %>% 
  select(sex = gender,
         id,
         contains('m12')) %>% 
  head(n=4)

## 흔히 범하는 오류
dat1 %>% 
  select(-4:7) %>% 
  colnames()

dat1 %>% 
  select(-c(4:7)) %>%   
  colnames()

#3.3 Filter
## 특정 조건을 만족하는 환자(row) 선택하기
dat1 %>% 
  filter(id<=20) %>% 
  head(n=4)

dat1 %>% 
  filter(id<=20) %>% 
  select(id, starts_with('b_')) %>% 
  head(n=4)

dat1 %>% 
  filter(gender =='M') %>% 
  head(n=4)

## 복합조건 사용하기
dat1 %>% 
  filter(age>=50, gender=='M') %>% 
  head(n=4)

dat1 %>% 
  filter(age >=30 & age<=60) %>% 
  head(n=4)

dat1 %>% 
  filter(between (age, 30, 60)) %>% 
  head(n=4)

dat1 %>% 
  filter(age >=50 | lc==1) %>% 
  head(n=4)

dat1 %>% 
  filter( (age>=60 & gender=="M") | (age<=50 & gender=="F")) %>% 
  head(n=4)

## 조건을 만족하지 않는 경우 골라내기  
dat1 %>% 
  filter(gender!="M") 

dat1 %>% 
  filter(lc !=1 & age >=50) 

## count기능
dat1 %>% 
  count(gender)

dat1 %>% 
  filter(gender == 'M' & hcc ==1) %>% 
  count()

dat1 %>% 
  filter(gender == 'M' & lc ==1) %>% 
  count(hcc)  

dat1 %>% 
  filter(gender == 'M' & lc ==1) %>% 
  count(treat_gr, hcc)

## 결측값 다루기 
dat1 %>% 
  filter(is.na(b_inr)) %>%  
  count()

dat1 %>% 
  filter(!is.na(b_inr)) %>%  
  head(n=4)

dat1 %>% 
  filter(!is.na(b_inr),
         !is.na(b_alt),
         !is.na(b_plt)) %>% 
  count()

##  결측값이 존재하지 않는 케이스만 남기기
dat1 %>% 
  drop_na(b_inr, b_alt, b_plt) %>% 
  count()

dat1 %>% 
  drop_na() %>% 
  count()

na.count <- apply(dat1, 2, function(x)sum(is.na(x)))
na.count

#3.4 Mutate
## 기존 변수를 이용해서 새로운 변수 만들기
dat1 %>% 
  mutate(alt_plt = b_alt / b_plt) %>% 
  select(b_alt, b_plt, alt_plt) %>% 
  head(n=4)

dat1 %>% 
  mutate(alt_plt = b_alt /b_plt) %>% 
  select(id, b_alt, b_plt, alt_plt) %>% 
  filter(is.na(b_alt) | is.na(b_plt))

dat1 %>% 
  drop_na(b_alt, b_plt) %>% 
  mutate(alt_plt = b_alt /b_plt) %>% 
  select(id, b_alt, b_plt, alt_plt) %>% 
  head(n=4)
## 조건을 이용해서 새로운 변수 만들기  
dat1 %>% 
  mutate(age_gr=ifelse(age >=50,'above_50','below_50')) %>%
  select(id, age, age_gr) %>% 
  head(n=6)

dat1 %>% 
  mutate(age_gr=ifelse(age >=50,'above_50','below_50')) %>%
  count(age_gr)

dat1 %>% 
  mutate(bil_gr=ifelse(b_bil<2,'A',ifelse(b_bil<3,'B','C'))) %>%  
  select(id, b_bil, bil_gr) %>% 
  head(n=6)

dat1 %>% 
  mutate(bil_gr=ifelse(b_bil<2,'A',
                       ifelse(b_bil<3,'B','C'))) %>%  
  count(bil_gr)

dat1 %>% 
  mutate(risk_gr=ifelse(age>=50 & lc==1,'high_risk',
                        ifelse(age<50 & lc==0, 'low_risk', 'intermediate_risk'))) %>% 
  count(risk_gr)

## 새로운 변수 만들고 나머지 변수 제거 
dat1 %>% 
  transmute(risk_gr=ifelse(age>=50 & lc==1,'high_risk',
                           ifelse(age<50 & lc==0, 'low_risk', 'intermediate_risk'))) %>% 
  head(n=4)

## 변수값의 순서를 새로운 변수로 만들기
dat1 %>% 
  mutate(age_rank = min_rank(age)) %>% 
  select(id, age, age_rank) %>% 
  head(n=6)

dat1 %>% 
  mutate(age_rank = min_rank(desc(age))) %>% 
  select(id, age, age_rank) %>% 
  head(n=6)

## 순서를 정할 때 중간에 gap없이 새로운 변수 만들기
dat1 %>% 
  mutate(age_rank = dense_rank(age)) %>% 
  select(id, age, age_rank) %>% 
  head(n=6)

## 퍼센트 순위로 새로운 변수 만들기
dat1 %>% 
  mutate(age_rank = percent_rank(age)) %>% 
  select(id, age, age_rank) %>% 
  arrange(age_rank) %>% 
  head(n=6)

## 누적합계를 변수로 만들기  
dat1 %>% 
  mutate(id_sum = cumsum(id)) %>% 
  select(id, id_sum) %>% 
  head(n=4)

## 동시에 여러 개의 새로운 변수 만들기
dat1 %>% 
  mutate(age_gr=ifelse(age>=50,'above 50','below 50')) %>% 
  mutate(bil_gr=ifelse(b_bil<2,'A',
                       ifelse(b_bil>3,'C','B'))) %>% 
  select(age, age_gr, b_bil, bil_gr) %>% 
  head(n=6)

## 연속형 변수를 일정 범위마다 그룹화하기
dat1 %>% 
  mutate(age_gr=ifelse(age>=60,'>=60',
                       ifelse(age>=50,'>=50',
                              ifelse(age>=40,'>=40',
                                     ifelse(age>=30,'>=30','<30'))))) %>% 
  select(age, age_gr) %>% 
  head(n=6)

dat1 %>% 
  mutate(age_gr=cut(age,
                    c(-Inf,30,40,50,60,Inf),
                    c('<30','>=30','>=40','>=50','>=60'))) %>%    
  select(age, age_gr) %>% 
  head(n=6)

dat1 %>% 
  mutate(age_gr=cut_width(age, width=10)) %>% 
  select(age, age_gr) %>% 
  head(n=6)

dat1 %>% 
  mutate(age_gr=cut_width(age, width=10, boundary=0)) %>%
  mutate(age_gr2=cut_width(age, width=10, boundary=9)) %>% 
  select(age, age_gr, age_gr2) %>% 
  head(n=6)

## 연속형 변수를 균일한 범위를 가지는 그룹으로 나누기  
dat1 %>% 
  mutate(age_gr=cut_interval(age, n=4)) %>% 
  select(age, age_gr) %>% 
  count(age_gr)

## 연속형 변수를 원하는 개수의 그룹으로 나누기
dat1 %>% 
  mutate(age_gr = cut_number(age, n=5)) %>% 
  select(age, age_gr) %>% 
  head(n=5)

dat1 %>% 
  mutate(age_gr = cut_number(age, n=5)) %>% 
  select(age, age_gr) %>% 
  count(age_gr)

#3.5 Arrange
## 오름차순
dat1 %>% 
  arrange(age) %>% 
  head(n=4)

dat1 %>% 
  arrange(desc(age)) %>%  
  head(n=4)
## 여러 개 변수를 오름차순
dat1 %>% 
  arrange(age, b_alt) %>% 
  select(age, b_alt)

#3.6. Summarise
## 원하는 변수의 평균 구하기  
mean(dat1$age)

dat1 %>% 
  summarise(mean_age = mean(age))

## 여러 개의 요약정보 동시에 구하기
dat1 %>% 
  summarise(mean_age = mean(age),
            median_age = median(age),
            iqr_age = IQR(age))

## 고유값 개수 확인  
dat1 %>% 
  summarise(n_distinct(age))

unique(dat1$age)

length(unique(dat1$age))

## 발생률 구하기

dat1 %>% 
  summarise( patient_number = n(),
             event = sum(hcc),
             person_year = sum(hcc_yr),
             incidence_rate = event / person_year)

#3.7 Group_by
## 원하는 변수로 나누어 각각 값 계산 
dat1 %>% 
  group_by(gender) %>% 
  summarise(mean_age = mean(age))

## 해당 조건을 만족하는 환자의 숫자를 변수로 만들기
dat1 %>% 
  group_by(gender, lc) %>% 
  summarise(patient_no = n(),
            mean_age = mean(age))

dat1 %>% 
  group_by(gender, lc) %>% 
  summarise(patient_no = n(),
            mean_inr = mean(b_inr))

dat1 %>% 
  group_by(gender, lc) %>% 
  summarise(patient_no = n(),
            mean_inr = mean(b_inr, na.rm=T))

## summary table 만들기
dat1 %>% 
  group_by(gender, lc) %>% 
  summarise( N_alt = sum(!is.na(b_alt)),
             MEAN_alt = mean(b_alt, na.rm=T),
             MEDIAN_alt = median(b_alt, na.rm=T),
             MIN_alt = min(b_alt, na.rm=T),
             MAX_alt = max(b_alt, na.rm=T))
