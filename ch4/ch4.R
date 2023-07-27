#CH3. 데이터 핸들링
library(tidyverse)
dat <- read.csv("C:\\Users\\phl02\\Desktop\\P\\bio\\ch4\\Ch4_chb.csv")
# 4.1.1 rbind

a <- c(1,2,3)
b <- c(4,5,6)
c <- rbind(a,b)
c 
a <- c(1,2,3,4)
b <- c(5,6,7)
c <- rbind(a,b)
c

##실제 데이터프레임 형태의 데이터를 이용해서 실습
temp1 <- dat %>% 
  select(id, age, lc, hcc) %>% 
  filter(id<5) %>% 
  print()

temp2 <- dat %>% 
  select(id, age, lc, hcc) %>% 
  filter(id>6) %>% 
  print()

temp3 <- rbind(temp1, temp2)
temp3

#4.1.2 cbind

a <- c(1,2,3)
b <- c(4,5,6)
c <- cbind(a,b)
c

##실제 데이터프레임 형태의 데이터를 이용해서 실습
temp4 <- dat %>% 
  select(id, age, lc, hcc) %>% 
  filter(id<5) %>% 
  print()

temp5 <- dat %>% 
  filter(id<5) %>% 
  select(b_alt, b_bil, b_inr) %>% 
  print()

temp6 <- cbind(temp4, temp5)
temp6


#4.1.3 merge

temp1 <- dat %>% 
  select(id, age, lc, hcc) %>% 
  filter(id <5) %>% 
  print()
temp2 <- dat %>% 
  select(id, b_alt, b_bil, b_inr) %>% 
  filter(id <5) %>% 
  arrange(desc(id)) %>% 
  print()
temp3 <- cbind(temp1, temp2)
temp3

temp4 <- merge(temp1, temp2, by='id')  
temp4


temp1 <- dat %>% 
  filter(id <4) %>% 
  select(id, age, gender) %>% 
  print()
temp2 <- dat %>% 
  filter(id %in% c(1,2,4,5,6)) %>%  
  select(id, lc, hcc) %>% 
  print()

merge(temp1, temp2, by='id')
merge(temp1, temp2, by='id', all=TRUE) 
merge(temp1, temp2, by='id', all.x=TRUE)
merge(temp1, temp2, by='id', all.y=TRUE)

#4.2 Tidyverse를 이용한 merge

#4.2.1 inner join

inner_join(temp1, temp2, by='id')

#4.2.2 full join

full_join(temp1, temp2, by='id')

#4.2.3 left join

left_join(temp1, temp2, by='id')

#4.2.4 right join

right_join(temp1, temp2, by='id')

#4.2.5 semi join, anti join

semi_join(temp1, temp2, by='id')
anti_join(temp1, temp2, by='id')


#4.2.6 intersect, union, setdiff

temp.x <- dat %>% 
  select(id, age, gender) %>% 
  filter(id<4) %>% 
  print( )
temp.y <- dat %>% 
  select(id, age, gender) %>% 
  filter(between (id, 2, 4)) %>% 
  print( )
intersect(temp.x, temp.y)
union(temp.x, temp.y)
setdiff(temp.x, temp.y)

#4.2.7 bind_rows, bind_cols

bind_rows(temp.x, temp.y)
bind_cols(temp.x, temp.y)
bind_rows(entecavir=temp.x, tenofovir=temp.y, .id = 'treatment')

#4.3 Tidy 데이터

library(tidyverse)
dat <- read_csv('C:\\Users\\phl02\\Desktop\\P\\bio\\ch4\\Ch4_chb2.csv')
#4.3.2 Tidy 데이터 만들기 연습
# long 형태로 변형
alt.long <- dat %>% 
  gather(2:6, key='observation', value='alt_result') %>% 
  arrange(id) 
head(alt.long,10)
dim(dat)
dim(alt.long)
# wide 형태로 변형
alt.wide<-alt.long %>% 
  spread(observation, alt_result)
head(alt.wide,10)
dim(alt.long)
dim(alt.wide)
