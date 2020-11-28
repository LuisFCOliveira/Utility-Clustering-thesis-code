

source("../Definitivo/1D/MinMax.R")

set.seed(454)

lev<-c(0,1,2,3)

#data simulation
n<-15


nivel<- sample(x = lev,size = n,replace = T,prob = rep(1/length(lev),length(lev)))

nivel
par(mfrow=c(1,2))
part_data(nivel,3,all = F)






#second exemplo

lev<-c(0,1,2,3,4)

n<-50


nivel<- sample(x = lev,size = n,replace = T,prob = rep(1/length(lev),length(lev)))

nivel
par(mfrow=c(2,2))
part_data(nivel,3,all = F)








