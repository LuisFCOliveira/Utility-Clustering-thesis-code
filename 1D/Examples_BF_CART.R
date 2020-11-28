
source("BF_CART.R")


# utility functions plot

x<- seq(0,2,by = 2/25)
plot(x,x**2,col="blue",type = "l",ylab="f(x)", main="Utility functions",lty=1,ylim=c(-2,4))
lines(x,sqrt(x),col="red",lty=2)
lines(x,x**3,col="purple",lty=3)
lines(x,log(x),col="green",lty=4)
legend("topleft",legend = c("quadratic","square root","cubic","log"),col = c("blue","red","purple","green"),cex=0.7,lty=c(1,2,3,4))




#ex1 # inserido




set.seed(56)

lev<- c(1:12)

dataset<- sample(lev,size = 150,replace = T);print(dataset)
barplot(table(dataset)/150)

data_graphs(dataset,func = "x**2",k1 = 6)


data_graphs(dataset,func = "sqrt(x)",k1 = 6)



####################################################################################################


#ex 2  # inserido


set.seed(845)

lev<- c(1:12)
prob= c(0.2,rep(0.6/10,10),0.2);prob
dataset<- sample(lev,size = 150,replace = T,prob=prob);print(dataset)
barplot(table(dataset)/150)



data_graphs(dataset,func = "x**2",k1 = 7)

data_graphs(dataset,func = "sqrt(x)",k1 = 7)




####################################################################################3


#ex 3


set.seed(345)

lev<- c(1:12)
prob= c(rep(0.35/5,5),0.3,rep(0.35/6,6))
dataset<- sample(lev,size = 150,replace = T,prob=prob);print(dataset)
barplot(table(dataset)/150)
length(unique(dataset)) #12


data_graphs(dataset,func = "x**2",k1 = 7)

data_graphs(dataset,func = "sqrt(x)",k1 = 7)






##############################################################################################33

#ex 4 Normal distribution


set.seed(412)



dataset<- round(rnorm(n = 500,mean = 50,sd = 5),0);dataset
length(unique(dataset)) #29
barplot(table(dataset))


data_graphs(dataset,func = "x**2",k1 = 6,k2=3)

data_graphs(dataset,func = "sqrt(x)",k1 = 6,k2=3)
###############################################################################################

#ex 4 Two Normal distributions


set.seed(32)


data1<- round(rnorm(n = 100,mean = 12,sd = 4),0);data1  
data2<- round(rnorm(n = 100,mean = 50,sd = 5),0);data2
dataset<- c(data1,data2)
barplot(table(c(data1,data2)))
length(unique(dataset))# 35

data_graphs(dataset,func = "x**2",k1 = 5,k2=2)

data_graphs(dataset, func = "sqrt(x)", k1 = 5, k2= 2)



###################################################################################################


#gama distribution

set.seed(412)


dataset<-round(rgamma(n = 500,shape = 3,scale = 5),0);dataset 
length(unique(dataset)) #46
barplot(table(dataset))


data_graphs(dataset,func = "x**2",k1 = 5,k2=4)

data_graphs(dataset,func = "sqrt(x)",k1 = 5,k2=4)



##################################################################################################







