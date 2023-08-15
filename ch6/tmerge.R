id <- c(1,2,3)
age <- c(40,20,50)
d1 <- data.frame(cbind(id,age))

tm_surg <- c(5,8,NA)
futime <- c(10,20,30)
event <- c(0,1,1)
d2 <- data.frame(cbind(id,tm_surg,futime,event))

#create start/stop time
step1 <- tmerge(data1=d1, data2=d2, id=id,
                death=event(futime, event))
step1
# create time-dependent covariate
step2 <- tmerge(data1=step1, data2=d2,id=id,
                surgery=tdc(tm_surg))
step2