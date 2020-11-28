
source(file = "BF_EVENT.R")

# First test

lev<-c(1,2,3,4,5)


set.seed(412)

data1<-sample(x = lev,size = 100,replace = T);data1
table(data1)


BF_min(data1,3,"x**2")

BF_min(data1,3,"sqrt(x)")


set.seed(154)

prob<- c(0.4,rep(0.2/3,3),0.4)
prob
data2<-sample(x = lev,size = 100,replace = T,prob = prob);data2


BF_min(data2,3,"x**2")

BF_min(data2,3,"sqrt(x)")

set.seed(563)

prob<- c(0.1,0.1,0.5,0.1,0.1)

data3<-sample(x = lev,size = 100,replace = T,prob = prob)


BF_min(data3,3,"x**2")


BF_min(data3,3,"sqrt(x)")


