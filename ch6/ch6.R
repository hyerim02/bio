#Chapter 6. 임상 연구관련 의학 통계
#6.1 회귀 분석
#6.1.1 단순 선형 회귀 분석

library(tidyverse)
dt<-read_csv("C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_regression.csv")
head(dt)

# 1) 상관관계 알아보기 
plot(osm~na, data=dt)

# 2) 회귀식 추정 
fit<-lm(osm~na, data = dt)
fit

plot(osm~na, data=dt)
abline(fit, col='red', lwd=2)

# 3) 결정계수 찾고 4)유의한 회귀식 모형인지 검증
summary(fit)

# 6) 기본 가정이 충족하는지 확인
par(mfrow=c(2,2))
plot(fit)

#6.1.2 다중 회귀 분석
# 1) 산점도 이용하여 데이터 분포 살펴보기 
par(mfrow=c(2,2))
plot(osm~bun, data=dt, main="BUN")
plot(osm~glucose, data=dt, main="Glucose")
plot(osm~height, data=dt, main="Height")
plot(osm~weight, data=dt, main="Weight")

# 2) 다중 선형 회귀식 추정 
fit.multi<-lm(osm~na+bun+glucose+height+weight, data=dt)
fit.multi

# 3) 유의성 및 결정계수 검정
summary(fit.multi)

# 4) 가정 검정
par(mfrow=c(2,2))
plot(fit.multi)

# 6) 선택의 기준: AIC
fit1<-lm(osm~na, data=dt)
f1<-summary(fit1)
f1$adj.r.squared

fit2<-lm(osm~na+bun, data=dt)
f2<-summary(fit2)
f2$adj.r.squared

fit3<-lm(osm~na+bun+glucose, data=dt)
f3<- summary(fit3)
f3$adj.r.squared

fit4<-lm(osm~na+bun+glucose+height, data=dt)
f4<-summary(fit4)
f4$adj.r.squared

fit5<-lm(osm~na+bun+glucose+height+weight, data=dt)
f5<-summary(fit5)
f5$adj.r.squared

# 한 번에 하는 방법
fit.multi<-lm(osm~na+bun+glucose+height+weight, data=dt)
step(fit.multi)

# 7) 또 다른 방법 
library(olsrr)

fit.multi<-lm(osm~na+bun+glucose+height+weight, data=dt)
# 모델 평가 
ols_step_all_possible(fit.multi)
#단계적 선택법으로 모델 비교
ols_step_best_subset(fit.multi)
#전진 선택법 
ols_step_forward_aic(fit.multi)
plot(ols_step_forward_aic(fit.multi))+theme_bw()
# 후진 선택법 
ols_step_backward_aic(fit.multi)
plot(ols_step_backward_aic(fit.multi))+theme_bw()

#6.2 일반화 선형 분석
#6.2.1 로지스틱 회귀 분석
# 1) 로지스틱 회귀분석의 기본 
dt1<-read_csv("C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_logistic.csv")
head(dt1)

plot(dt1$id, dt1$group)


# 4) 우도비검정
library(moonBook)
mytable(group~aspirin+ibd+diabetes+gender+age, data=dt1)
# 5) 공식
fit<-glm(group~age+gender+ibd+cirrhosis+diabetes+htn+aspirin, 
         family=binomial, data=dt1)
fit
summary(fit)
# 6) 유의한 독립변수만 포함 
step(fit, type='backward')

final.fit<-glm(group~age+ibd+cirrhosis+aspirin, 
               family=binomial, data=dt1)

extractOR(final.fit)

# 7) 회귀모형 평가 
library(fmsb)
NagelkerkeR2(final.fit)

#6.2.2 모형의 성능

library(performance)
library(see)
library(patchwork)
# 1)Nagelkerke 결정계수
r2_nagelkerke(final.fit)
# 2) Hosmer-Lemeshow goodness-of fit test
performance_hosmer(final.fit)
# 3) 회귀모형 가정에 위배되는지 확인 
fit<-lm(osm~na+bun+glucose+height+weight, data=dt)
check_model(fit)
# 4) 더 나은 모형 선택 
model_performance(fit)

fit<-lm(osm~na+bun+glucose+height+weight, data=dt)
fit1<-lm(osm~na+bun+glucose, data=dt)
compare_performance(fit, fit1, rank = TRUE)

#6.3 ROC 관련 분석
#6.3.4 ROC 곡선 직접 그려보기

library(ggplot2)
roc.ex<-read_csv("C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_afp.csv")
head(roc.ex)
roc.ex<-roc.ex %>% 
  arrange(desc(afp))
roc.ex


## 책 예시를 보고 직접 코드를 짜서 그려봄 
ggplot(roc.ex,aes(x=afp,fill=factor(group)))+
  geom_density(alpha=0.5)+xlim(0,20)

sensitivity <-c(0,0.1,0.2,0.3,0.8,0.9,1,1)
FPR <-1-c(1,1,1,0.9,0.9,0.8,0.6,0)
result <- data.frame(cbind(sensitivity,FPR))
ggplot(result,aes(x=FPR,y=sensitivity))+geom_point()+
  geom_abline(intercept = 0,slope = 1,color='red',linetype=3)
#6.3.5 pROC 패키지

library(pROC)

# 1) ROC 객체 생성 
afp<-roc(roc.ex$group, roc.ex$afp, ci=TRUE)
afp
plot(afp)

afp<-roc(roc.ex$group, roc.ex$afp)
plot(afp,legacy.axes=TRUE)
# 2) 겹쳐 그리기 
afp<-roc(roc.ex$group, roc.ex$afp)
pivka<-roc(roc.ex$group, roc.ex$pivka)
plot(afp, col='blue', legacy.axes=TRUE)
plot(pivka, col='red', legacy.axes=TRUE, add=TRUE)
legend(0.3, 0.2, legend=c("AFP", "PIVKA"),
       col=c("blue", "red"), lty=1:1, cex=0.8)
# 3) ROC비교 
roc.test(afp, pivka)

# 4) 최적의 cut-off 찾기

ci.thresholds(afp, conf.level=0.95, boot.n=1000,
              thresholds='best')

# 5) 특정 cut-off에서 민감도, 특이도 계산하기

metric<-c('sensitivity','specificity','ppv','npv')

afp.cutoff<-ci.coords(afp, x=5, input="threshold", metric)
afp.cutoff

afp.cutoff$sensitivity
afp.cutoff$specificity
afp.cutoff$ppv
afp.cutoff$npv

#6.3.6 Epi 패키지

library(Epi)

roc.ex$new_gr<-ifelse(roc.ex$group=='HCC',1,0)
ROC(form=new_gr~afp, data=roc.ex, plot='ROC')
ROC(form=new_gr~pivka, data=roc.ex, plot='ROC')
# 1) 2개 진단검사를 포함한 ROC  
ROC(form=new_gr~afp+pivka, data=roc.ex, plot='ROC')

#6.4 생존 분석
#6.4.1 Time to event 분석
# 2) 생존함수와 위험함수 
library(survival)
library(lubridate)
suv.dt<-read_csv('C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_survival.csv')
suv.dt
# 3) 추적관찰기간 계산 
suv.dt$hcc_period <- suv.dt$hcc_date - suv.dt$start_date
suv.dt$hcc_period <- as.numeric(suv.dt$hcc_period)
summary(suv.dt$hcc_period)
# 일 단위를 년 단위로 
suv.dt$hcc_period <- suv.dt$hcc_period / 365.25
summary(suv.dt$hcc_period)
# 한 번에 코드로 
suv.dt <- suv.dt %>% 
  mutate(hcc_period = hcc_date - start_date) %>% 
  mutate(hcc_period = as.numeric(hcc_period)/365.25)


library(ggpubr)
suv.dt1 <- suv.dt %>% 
  mutate(hcc_period = hcc_date - start_date) %>% 
  mutate(hcc_period = as.numeric(hcc_period)/365.25)%>%
  mutate(hcc_period=round(hcc_period,2))%>%
  mutate(hcc = factor(hcc))
ggbarplot(suv.dt1,y='hcc_period',x='id',fill='hcc',sort.val = 'asc',sort.by.groups = F, label = TRUE,lab.pos = "in",orientation = "horiz")

#6.4.2 Kaplan-Meier 곡선
#1) 생존함수 객체 만들기
f1<-survfit(Surv(hcc_period, hcc)~1, data=suv.dt)
plot(f1,
     xlab='Obervation period',
     ylab='Survival probability')
# 2) survminer패키지 
library(survminer)
ggsurvplot(f1, risk.table = TRUE)

#6.4.3 5년 생존율 계산

summary(f1,times=5)

plot(f1, conf.int=FALSE)
points(x=5, y=0.665, pch=19)
segments(5,-0.1, 5,0.665, col='red')
segments(-1,0.665, 5,0.665,col='red', lty=2)
text(x=0+0.1, y=0.665+0.1, labels=c('66.5%'), col='red')

#6.4.4 Median survival 계산
median(suv.dt$hcc_period)

f1
plot(f1, conf.int=FALSE)
points(x=8, y=0.5, pch=19)
segments(8,-0.1, 8,0.5, col='red')
segments(-1,0.5, 8,0.5,col='red', lty=2)
text(x=8, y=0.6, labels=c('50.0%'), col='red')

#6.4.5 두그룹에서 생존 함수 비교
# log-rank test
survdiff(Surv(hcc_period, hcc)~lc, data=suv.dt)
f2<-survfit(Surv(hcc_period, hcc)~lc, data=suv.dt)
plot(f2, conf.int=FALSE, col=c('blue','red'))
text(x=9, y=0.8, labels=c('lc=0'), col='blue')
text(x=9, y=0.2, labels=c('lc=1'), col='red')

#6.4.6 Survminer 패키지

suv.dt1<-read_csv('C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_survival1.csv')
dim(suv.dt1)
# 1) 가장 기본 km곡선 그리기 
f1<-survfit(Surv(death_yr, death)~1, data=suv.dt1)
ggsurvplot(f1)

f1.hcc<-survfit(Surv(death_yr, death)~hcc, data=suv.dt1)
ggsurvplot(f1.hcc)
# 2) 누적 발생률 
f2<-survfit(Surv(death_yr, death)~1, data=suv.dt1)
ggsurvplot(f2,
           conf.int = FALSE,
           fun = 'event',
           ylim=c(0,1),
           ggtheme=theme_bw())

f2.hcc<-survfit(Surv(death_yr, death)~hcc, data=suv.dt1)
ggsurvplot(f2.hcc,
           fun='event',
           pval=TRUE,
           risk.table='abs_pct',
           palette=c('red','blue'),
           break.time.by=1,
           legend='top',
           legend.title='HCC',
           legend.labs=c('None','Present'),
           xlab=c('Years after treatment'),
           ylab=c('Cumulative incidence of HCC'),
           ylim=c(0,1),
           surv.median.line = 'hv',
           ncensor.plot=TRUE)

#6.4.7 Cox 비례위험모형
# 1)cox model 
library(moonBook)
suv.dt2 <-read_csv('C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_survival2.csv')

f1.lc<-coxph(Surv(hcc_yr, hcc)~lc, data=suv.dt2)
extractHR(f1.lc)

f1.lc<-survfit(Surv(hcc_yr, hcc)~lc, data=suv.dt2)
ggsurvplot(f1.lc,
           fun='event',
           pval=TRUE,
           risk.table=TRUE,
           break.time.by=1,
           xlab=c('Year after treatment'),
           ylab=c('Cumulative incidence of HCC'),
           ylim=c(0,1))

#6.4.8 Cox 모형을 이용하여 단변량, 다변량 분석
# 1) moonBook 패키지- 단변량  

suv.dt2$TS<-Surv(suv.dt2$hcc_yr,suv.dt2$hcc)

mycph(TS~gender+age+lc+dm+hbeag, data=suv.dt2)

# 2) gtsummary 패키지 - 단변량 

library(gtsummary)
suv.dt2 %>% 
  select(-id, -TS, -death, -death_yr) %>% 
  tbl_uvregression(method=coxph,
                   y=Surv(hcc_yr, hcc),
                   exponentiate=TRUE)
# 3) 다변량 분석 결과 제시 
f1.multi<-coxph(Surv(hcc_yr,hcc)~age+gender+lc+dm+hbeag, data=suv.dt2)
extractHR(f1.multi)

f1.final<-step(f1.multi, direction = 'backward')
extractHR(f1.final)
# 4) gtsummary 패키지- 다변량 
coxph(Surv(hcc_yr, hcc==1)~age+gender+lc+dm+hbeag, data=suv.dt2) %>% 
  tbl_regression(exponentiate=TRUE)

cox.uni<-suv.dt2 %>% 
  select(hcc, hcc_yr, age, gender, lc, dm, hbeag) %>% 
  tbl_uvregression(method=coxph,
                   y=Surv(hcc_yr, hcc),
                   exponentiate = TRUE)
cox.uni

cox.multi<-coxph(Surv(hcc_yr, hcc)~age+lc, data=suv.dt2) %>% 
  tbl_regression(exponentiate=TRUE)
cox.multi

cox.table<-tbl_merge(
  tbls = list(cox.uni, cox.multi),
  tab_spanner = c("**Univariate analysis**","**Multivariable analysis**")
)
cox.table

#6.4.9 Forest plot 그리기
library(forestmodel)
f1.cox<-coxph(Surv(hcc_yr, hcc==1)~age+gender+lc+dm+hbeag, data=suv.dt2)
ggforest(f1.cox)


#6.4.10 Cox 모형 검증

f1.cox<-coxph(Surv(hcc_yr, hcc==1)~age+gender+lc+dm+hbeag, data=suv.dt2)
cox.zph(f1.cox)

ftest<-cox.zph(f1.cox)
ggcoxzph(ftest)

#6.5 Time dependent Cox model

dt.time<-read_csv('C:\\Users\\phl02\\Desktop\\P\\bio\\ch6\\Ch6_survival3.csv')
head(dt.time)

f1.hcc<-survfit(Surv(hcc_yr, hcc)~alt_nl, data=dt.time)
ggsurvplot(f1.hcc,
           fun='event',
           risk.table=TRUE,
           break.time.by=1,
           xlim=c(0,5),
           ylim=c(0,0.3),
           pval = TRUE)

dt.time1<-tmerge(dt.time, dt.time, id=id, HCC=event(hcc_yr, hcc))

dt.time1<-tmerge(dt.time1, dt.time1, id=id, ALT=tdc(alt_duration, alt_nl))

dt.time1$ALT[is.na(dt.time1$ALT)]<-c('abnormal')

head(dt.time[,c('id','hcc_yr','hcc','alt_nl','alt_duration')])
head(dt.time1[,c('id','hcc_yr','hcc','alt_nl','alt_duration','tstart','tstop','HCC','ALT')],11)

f1.time<-coxph(Surv(tstart, tstop, HCC==1)~ALT+cluster(id), data=dt.time1)
extractHR(f1.time)

