# 패키지
library(tidyverse)
library(survival)
library(survminer)
library(ggsci)
library(ggsignif)
library(gtsummary)
library(forestmodel)

#테이블 만들기
dt1<-read_csv("C:\\bio_R\\Ch1_table1.csv") 

dt1 %>%
  select(-id, -hcc_yr, -m6_alb) %>%
  tbl_summary(by=LC,missing='no') %>% 
  add_p() %>% 
  add_overall() %>% 
  modify_spanning_header( c('stat_1','stat_2')~'**Liver Function**') 

# Multivariable analysis table
dt2<-read_csv("C:\\bio_R\\Ch1_multi.csv")

fit.multi <- glm(Group~CurrentUser+Age+RaceGroup+Gender+
                   HBV+HCV+Cirrhosis+IBD+Diabetes+Obesity+
                   NAFLD+Smoking,
                 family=binomial, data=dt2)

tbl_regression(fit.multi, exponentiate = T) %>% 
  bold_labels() %>% 
  bold_p()

#Forest plot
forest_model(fit.multi)

#NEJM bar 그래프

ggplot(dt1,aes(x= Sex, y=Platelet, fill=factor(HCC)))+
  geom_bar(stat='identity', position='dodge')+
  theme_bw()+
  scale_fill_nejm()+
  geom_signif(comparisons = list(c('F','M')))

#JCO KM 그래프

km1<-survfit(Surv(hcc_yr, HCC)~LC, data=dt1)

ggsurvplot(km1,palette = 'jco',risk.table = T)

#Waterfall 그래프

dt1 %>% 
  filter(id<=30) %>% 
  select(id,Sex,Albumin,m6_alb) %>% 
  mutate(delta_alb=Albumin-m6_alb) %>% 
  ggbarplot(x='id', y='delta_alb',
            fill='Sex',
            sort.val='desc',
            sort.by.groups = FALSE)


