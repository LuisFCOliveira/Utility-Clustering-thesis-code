
source("Desktop/Estudo/Mestrado/Tese/Codigo/Definitivo/2D/BF_CART_2D.R")


?optimize()

ster(9,2)
ster(9,3)
ster(9,4)

ster(10,2)
ster(10,3)
ster(10,4)

ster(11,2)
ster(11,3)
ster(11,4)


ster(12,2)
ster(12,3)
ster(12,4)

ster(16,2)
ster(16,3)
ster(16,4)

#ex 1 feito

set.seed(523)

lev1<- c(1,1.01,1.02)


var1<- sample(lev1,size = 200,replace = T)

var2<- sample(lev1,size = 200,replace = T)


df<- data.frame(var1=var1,var2=var2)

order<-data.frame(var=c(lev1,lev1),factor=c(c(1:3),c(1:3)))

order

data_graphs_2D(df,order= order,k1 = 7,u1 = lev1**2 ,u2= lev1**2,runtime = T)

data_graphs_2D(df,order= order,k1 = 7,u1 = lev1**2 ,u2= sqrt(lev1),runtime = T)



#ex 2 feito

set.seed(123)


var1<- sample(lev1,size = 200,replace = T,prob = c(0.45,0.1,0.45))

var2<- sample(lev1,size = 200,replace = T,prob= c(0.45,0.1,0.45))


df<- data.frame(var1=var1,var2=var2)

order<-data.frame(var=c(lev1,lev1),factor=c(c(1:3),c(1:3)))

order


data_graphs_2D(df,order= order,k1 = 7,u1 = lev1**2 ,u2= lev1**2,runtime = T)

data_graphs_2D(df,order= order,k1 = 7,u1 = lev1**2 ,u2= sqrt(lev1),runtime = T)


#ex 3

set.seed(457)


var1<- sample(lev1,size = 200,replace = T,prob = c(0.15,0.7,0.15))

var2<- sample(lev1,size = 200,replace = T,prob= c(0.15,0.7,0.15))


df<- data.frame(var1=var1,var2=var2)

order<-data.frame(var=c(lev1,lev1),factor=c(c(1:3),c(1:3)))

order

data_graphs_2D(df,order= order,k1 = 7,u1 = lev1**2 ,u2= lev1**2,runtime = T)

data_graphs_2D(df,order= order,k1 = 7,u1 = lev1**2 ,u2= sqrt(lev1),runtime = T)


#ex4




############################################################################################3
#ex 4

# change to other attributes

lev1<-c(1:5)
lev2<- c(1:2)


set.seed(74)


var1<- sample(lev1,size = 200,replace = T)

var2<- sample(lev2,size = 200,replace = T)


df<- data.frame(var1=var1,var2=var2)

df

order<-data.frame(var=c(lev1,lev2),factor=c(lev1,lev2))

order

data_graphs_2D(df,order= order,k1 = 4,k2=3,u1 = lev1**2 ,u2= lev2**2 ,runtime = T)










#############################################################################################

#Appendix

#ex 5

lev1<-c(1:3)
lev2<- c(1:3)


set.seed(674)


var1<- sample(lev1,size = 200,replace = T)

var2<- sample(lev2,size = 200,replace = T)


df<- data.frame(var1=var1,var2=var2)

df

order<-data.frame(var=c(lev1,lev2),factor=c(lev1,lev2))

order

data_graphs_2D(df,order= order,k1 = 7,u1 = c(5,12,9) ,u2= c(10,25,15) ,runtime = T)




#ex 6

#ex 5

lev1<-c(1:5)
lev2<- c(1:5)


set.seed(679)


var1<- sample(lev1,size = 500,replace = T)

var2<- sample(lev2,size = 500,replace = T)


df<- data.frame(var1=var1,var2=var2)

df

order<-data.frame(var=c(lev1,lev2),factor=c(lev1,lev2))

order

data_graphs_2D(df,order= order,k1 = 0,k2=10,u1 = lev1**2 ,u2= lev2**2 ,runtime = T)

##############################################################################################


# ice cream example


flav<- c("vanilla","chocolate","strawberry","stracciatella")
sz<- c("small","medium","big")

set.seed(674)


var1<- sample(flav,size = 500,prob=c(0.3,0.3,0.2,0.2),replace = T)

var2<- sample(sz,size = 500,prob=c(0.3,0.4,0.3),replace = T)


df<- data.frame(flavour=var1,size=var2)

df

order<-data.frame(var=c(flav,sz),factor=c(c(1:length(flav)),c(1:length(sz))))

order

data_graphs_2D(df,order= order,k1=0,k2 = 9,u1 = c(1,5,7.5,9) ,u2= c(2,6,8) ,runtime = T)


df$flavour<- factor(df$flavour,levels = flav)
df$health<- factor(df$size,levels = sz)


n_flavour<- (table(df$flavour))


table_size<- sort(table(df$size))[sz]
 


bar_flav<- barplot(table(df$flavour),col=c("red","green","blue","pink"),ylim = c(0,1.15*max(n_flavour)),main="Flavour",cex.names = 0.8)
text(bar_flav,n_flavour,n_flavour,pos=3)

bar_size<- barplot(table_size,col=c("red","blue","green"),main="Size",ylim = c(0,1.15*max(n_size)))

text(bar_size,table_size,table_size,pos=3)


table_both<- table(df$flavour,df$size)
table_both<- table_both[,c(3,2,1)]




bar_both<- barplot(table_both,beside = T,col=c("red","blue","green","pink"),xlab = "size",main="Flavour and size",ylim = c(0,1.15*max(table(df$flavour,df$size))),cex.axis = 0.6 )
text(bar_both,table_both,table_both,pos=3)
legend("topright",legend = flav,fill=c("red","blue","green","pink"),title = "Flavour",cex=0.6)

####################################################################################################


#State simulation 



#population simulation

social<-c("low","medium","high")
saude<- c("low","medium","high")
prob_social<-c(0.40,0.5,0.1)

set.seed(214)

var_social<- sample(social,size = 1000,prob = prob_social,replace = T)

var_saude<-c()

for(i in var_social){
  
  if(i== "low"){
    
    var_saude<- append(var_saude,sample(saude,size=1,prob = c(0.6,0.3,0.1)))
    
  }
  
  if(i== "medium"){
    
    
    var_saude<- append(var_saude,sample(saude,size=1,prob = c(0.3,0.5,0.20)))
    
  }
  
  if(i=="high"){
    
    
    var_saude<- append(var_saude,sample(saude,size= 1,prob = c(0.0,0.4,0.60)))
    
  }
  
}


#dataframe building

df<- data.frame(social = var_social, health = var_saude)



df$social<- factor(df$social,levels = social)
df$health<- factor(df$health,levels = saude)

freq_social<- (table(df$social))/length(df$social)

n_social<- (table(df$social))

freq_saude<- (table(df$health))/length(df$health)

n_saude<- (table(df$health))

print("Frequência social")
print(n_social)
print("Frequência saude")
print(n_saude)
p_social<- c(0.3,0.2,0.1)
p_saude<- c(0.3,0.2,0.1)

custo_social<- c(400,770,1200)
custo_saude<- c(770,810,1030)

# baplots

total_social<- c()
total_saude<- c()


set.seed(519)

for(i in c(1:3)){
  
  
  
  total_social<- append(total_social,custo_social[i]*sum(rbinom(n=n_social[i],size = 1,prob = p_social[i])))
  
  total_saude<- append(total_saude,custo_saude[i]*sum(rbinom(n=n_saude[i],size = 1,prob = p_saude[i])))
}



print(head(df,5))

bar_social<- barplot(table(df$social),col=c("red","green","blue"),ylim = c(0,1.15*max(n_social)),main="Social class")
text(bar_social,n_social,n_social,pos=3)

bar_saude<- barplot(table(df$health),col=c("red","blue","green","pink"),main="Health quality",ylim = c(0,1.15*max(n_saude)))

text(bar_saude,n_saude,n_saude,pos=3)

bar_both<- barplot(table(df$social,df$health),beside = T,col=c("red","blue","green"),xlab = "health level",main="Health and social class",ylim = c(0,1.15*max(table(df$social,df$health))) )
text(bar_both,table(df$social,df$health),table(df$social,df$health),pos=3)
legend("topright",legend = c("low","medium","high"),fill=c("red","blue","green"),title = "Social class")


#cost functions

alfa_baixo<- function(m,custo,k){
  
  expr<- custo/(1+k*m)
  return(expr)
  
}


risk_baixo<- function(m,k){
  
  
  expr1<-  (2*k)
  expr2<- (1+m*k)
  
  return(expr1/expr2)
}



alfa_medio<- function(m,custo,k){
  
  expr<- custo - k*m
  return(expr)
  
}


# função constante
alfa_alto<- function(m,custo,k){
  
  expr<- custo*rep(1,length(m))
  return(expr)
}


#função que calcula as utilidades de cada nivel e atributo 

dist<- c(0.80,0.15,0.05)

utilidades<- function(m,custo,k,coef=0.5){
  
  dist<- c(0.80,0.15,0.05)
  
  if(sum(dist)!=1){
    
    print("Vector must have sum equal to 1")
  }
  
  # a utilidade de cada atributo corresponde á soma dos custos de cada nível multiplicado pela frequência nos dados
  
  util<- vector(mode = "list",length = 2)
  
  m_classe<- coef*m
  
  # apoios: 0.6 baixa
  # 0.3 média
  #0.1 alta
  
  util[[1]]<- c(alfa_baixo(dist[1]*m_classe,custo[1],k[1]),alfa_medio(dist[2]*m_classe,custo[2],k[2]),alfa_alto((dist[3])*m_classe,custo[3],k[3]))
  
  m_saude<- (1-coef)*m
  
  #apoios
  # 0.7 baixa
  #0.2 média
  #0.1 alta
  
  util[[2]]<- c(alfa_baixo(dist[1]*m_saude,custo[4],k[4]),alfa_medio(dist[2]*m_saude,custo[5],k[5]),alfa_alto((dist[3])*m_saude,custo[6],k[6]))
  
  return(util) 
  
  
}


total_custo<- function(m,custo,k,coef=0.5){
  
  util<- utilidades(m,custo,k,coef) 
  soma<- sum(unlist(util))+m
  
  
  return(soma)
  
}

#lapunov limit

lap_social<- c()
run_lap<- c(1:50)

n_lap<- n_social

for(i in run_lap){
  
  cent_mom<- n_lap*2*p_social*(1-p_social)*(p_social*(p_social-1)+0.5)
  b_n<- sum(cent_mom)**(1/3)
  
  var_bern<- n_lap*p_social*(1-p_social)
  c_n<- sum(var_bern)**(1/2)
  
  
  lap_social<- append(lap_social,b_n/c_n)
  
  n_lap<- n_lap*(i+1)
  
}

plot(run_lap,lap_social,main="Limit Lapunov social+health",xlab = "Multiplicative of number of elements",ylab = "Lapunov fraction")



#cost decreasing function and parameter definition




order<- data.frame(var=c("low","medium","high","low","medium","high"),
                   factor=c(c(1:3),c(1:3)),stringsAsFactors = F)

#data_freq_trans(df,order)



investimento<- seq(0,4e5,by = 3.5e4)

custo= c(total_social,total_saude)

k<- c(0.0001,0.35,0.001,0.00006,0.28,0.00015)

util0<-utilidades(0,custo = custo,k = k) 

print("Custo total sem investimento")
utilidades(0,custo,k,0.5)
total_custo(0,custo,k,0.5)




#gráficos classe

plot(0.5*investimento,alfa_baixo(0.5*investimento,custo[1],k[1]),main = "Utility social low",ylab = "cost",xlab="investment")

plot(0.5*investimento,risk_baixo(0.5*investimento,k[1]),main = "Aversion coeficient social low",ylab = "coef")

plot(0.5*investimento,alfa_medio(0.5*investimento,custo[2],k[2]),main = "Utility social medium",ylab = "cost",xlab = "investment" )


plot(0.5*investimento,alfa_alto(0.5*investimento,custo[3],k[3]),main = "Utility social high",ylab = "cost",xlab = "investment")


#gráficos saude

plot(0.5*investimento,alfa_baixo(0.5*investimento,custo[4],k[4]),main = "Utility health low",ylab = "cost",xlab = "investment")

plot(0.5*investimento,risk_baixo(investimento,k[4]),main = "Aversion risk  coefficient health low  ",ylab = "coef",xlab = "investment")

plot(0.5*investimento,alfa_medio(0.5*investimento,custo[5],k[5]),main = "Utility health medium",ylab = "cost",xlab = "investment")


plot(0.5*investimento,alfa_alto(0.5*investimento,custo[6],k[6]),main = "Utility health high",ylab = "cost",xlab = "investment")



# clustering zero investment 


print(utilidades(m = 0,custo = custo,k = k))

max_custo<- total_custo(0,custo=custo,k=k)

print(max_custo)


#define max investment
teto_custo<- 1.50*max_custo 


# process of investment. Sorry if it too dense, just run it and I believe it will speak for itself.

coef<-c(0,1)

min_gasto<-c()

min_coef<- c()

class_dev<- data.frame()
saude_dev<- data.frame()

value_risk<- c()


# valores esperados e variância
# fixar variância para valor inicial, sem decaímento

v_social<- sum(custo_social**2*n_social*p_social*(1-p_social))

v_saude<- sum(custo_saude**2*n_saude*p_saude*(1-p_saude))

v_total<- v_social + v_saude 

v_total<- 1000*v_total

print(sqrt(v_social))
print(sqrt(v_saude))
print(sqrt(v_total))

for( m in investimento ){
  
  
  # minimize coeficient
  
  
  
  total<- optimise(total_custo,interval = coef,custo=custo,k=k,m=m) #obter melhor coeficiente
  
  coef_min<- total$minimum  # coeficiente que minimiza custo total
  total_min<- total$objective # o valor total minimo 
  
  
  gasto<- total_min # gasto total
  
  min_gasto<- append(min_gasto,gasto) # valores do gasto mínimo
  
  min_coef<- append(min_coef,coef_min) # valor do coeficiente que minimiza
  
  util_min<- utilidades(m,custo,k,coef_min) 
  
  se_social<- sum(unlist(util_min[[1]]))
  
  se_saude<- sum(unlist(util_min[[2]]))
  
  me_total<- se_saude + se_social + m
  
  
  
  v_risk<-  1-pnorm(q = teto_custo, mean = me_total ,sd = sqrt(v_total))
  
  value_risk<- append(value_risk,v_risk)
  
  class_dev<- rbind(class_dev,100*(unlist(util_min[[1]]))/unlist(util0[[1]]))
  saude_dev<- rbind(saude_dev,100*(unlist(util_min[[2]]))/unlist(util0[[2]]))
  
  
}


colnames(class_dev)<- names(freq_social)
colnames(saude_dev)<- names(freq_saude)
# barplot

barplot(t(as.matrix(class_dev)),beside = T,names.arg = investimento,cex.names = 0.7,col=c("red","blue","green"),main = "Relative utility in social investment")
legend(-8,-2,legend = social ,fill=c("red","blue","green"),cex=0.6,xpd=T)


barplot(t(as.matrix(saude_dev)),beside = T,names.arg =  investimento,cex.names=0.7,col=c("red","blue","green"),main="Relative utility in health investment")

legend(-8,-2,legend = saude ,fill=c("red","blue","green"),cex = 0.6,xpd=T)


#barplot(investimento,class_dev)

#optimização

tol<- 0.01 # valor de tolerância

global_coef<- min_coef[order(min_gasto)[1]]

global<- optimise(total_custo,lower = investimento[1], upper = investimento[length(investimento)],custo=custo,k=k,coef= global_coef)

#parametros optimização

global_custo<- total_custo(m = global$minimum,custo = custo,k=k,coef = global_coef ) 
old_custo<- sort(min_gasto)[1]
old_m<- global$minimum

itr_count<- 0

while(abs(global_custo-old_custo)>tol){
  
  
  old_custo<- global_custo # velho custo
  
  new_global_coef<- optimise(total_custo,interval = coef,custo=custo,k=k,m=old_m) # novo coeficiente
  
  global_coef<- new_global_coef$minimum  # guardar novo coeficiente
  
  global<- optimise(total_custo,interval = c(investimento[1]+0.95*old_m,investimento[length(investimento)]-old_m),custo=custo,k=k,coef=global_coef) # obter novo minimo investimento com o novo coeficiente
  
  global_custo<- global$objective # novo custo minimo
  
  old_m<- global$minimum
  
  itr_count<- itr_count+1
  
  
}


reducao<- 100*abs(max_custo-global_custo)/max_custo  # percentagem de redução do custo total


av_risk<- sum(value_risk)/length(value_risk)
# gráfico investimento e custo total

plot(investimento,min_gasto,main=sprintf("Investment and cost %.0f-%.0f-%.0f: reduction of %.2f %% with %d cycles. Av. risk: %.4f",100*dist[1],100*dist[2],100*dist[3],reducao,itr_count,av_risk),xlab="investment",ylab = "total cost",ylim= c(0.85*global$objective,max(max(total_min),1.05*teto_custo)),xpd=T,cex.main=0.8)

text(investimento,min_gasto,sprintf("%.4f",value_risk),pos=1,cex=0.7) # value at risk

abline(h=teto_custo,lty=2,col="red")
text(x=investimento[0.3*length(investimento)],y=teto_custo,labels = sprintf("maximum cost: %.2f",teto_custo),cex=0.7,pos=1)



points(global$minimum,global_custo,col="red",cex=0.7,pch=19)

#text(global$minimum,global_custo,round(global_coef,3),pos=1,cex=0.5)


text(investimento,min_gasto,labels = sprintf(" %.3f",min_coef),pos=3,xpd=T,cex=0.7)


# encontrar valor de investimento onde o custo total é igual ao inicial

ord_equal<- order(abs(min_gasto[2:length(min_gasto)]-teto_custo))[1]
m_equal<- investimento[2:length(investimento)][ord_equal]



coef_equal<- optimise(total_custo,m=m_equal,interval = coef,k=k,custo=custo)$minimum



equal_custo<- function(m,custo,k,coef,teto_custo){
  
  expr<- total_custo(m,custo,k,coef)-teto_custo
  
  return(expr)
}

root<- uniroot(equal_custo, lower = investimento[2],upper = investimento[length(investimento)], k=k,custo=custo,coef=coef_equal,teto_custo=teto_custo)

m_equal<- as.numeric(root[1])

points(m_equal,teto_custo,col="blue",cex=0.7,pch=19)


legend("bottomright",legend=c(sprintf("Global min =(%.0f,%.0f) ",global$minimum,global_custo),sprintf("Max cost =(%.0f,%.0f) ",m_equal,teto_custo),"top: alpha","bottom: prob. value at risk"),pch = c(19,19,1,1),col=c("red","blue","black","black"),cex=0.6) 


global_util<- utilidades(global$minimum,custo = custo,k = k) 




class_dev<- 100*unlist(global_util[[1]])/unlist(util0[[1]])
saude_dev<- 100*unlist(global_util[[2]])/unlist(util0[[2]])

#diferenças utilidades


barplot(as.matrix(class_dev),beside = T,names.arg = social,cex.names = 0.7,col=c("red","blue","green"),main = sprintf("Relative utility in optimal social investment ",global$minimum),ylim=c(0,100))
legend(-8,-2,legend = social,fill=c("red","blue","green"),cex=0.6,xpd=T)

barplot(as.matrix(saude_dev),beside = T,names.arg = saude,cex.names=0.7,col=c("red","blue","green"),main=sprintf("Relative utility in optimal health investment",global$minimum),ylim=c(0,100))
legend(-8,-2,legend = saude,fill=c("red","blue","green"),cex = 0.6,xpd=T)


# cluster optimal invesment

data_graphs_2D(df,order = order,k1 = 5,k2=0,u1 =global_util[[1]],u2= global_util[[2]],runtime = T)


#cluster max investment

equal_util<- utilidades(m_equal,custo = custo,k = k) 

data_graphs_2D(df,order = order,k1 = 5,k2=0,u1 =equal_util[[1]],u2= equal_util[[2]],runtime = T)


# utility evolution plots

frame_social<- data.frame(zero=total_social,optimal=unlist(global_util[[1]]),max=unlist(equal_util[[1]]))

print(frame_social)

frame_saude<- data.frame(zero=total_saude,optimal=unlist(global_util[[2]]),max=unlist(equal_util[[2]]))

print(frame_saude)

plot(c(1:3),rep(NaN,3),ylim = c(min(frame_social),1.1*max(total_saude)),main="Utility attribute evolution",xlab = "Investment type", ylab = "Utility values",xaxt="n")
axis(1,at=1:3,labels = c("zero","optimal","maximum"))
for(i in c(1:3)){
  
  lines(c(1:3),frame_social[i,],col=i,lty=1)
  
  lines(c(1:3),frame_saude[i,],col=i,lty=2)
}


legend("topright", legend = c("low","medium","high","social","health"), col=c(1,2,3,"black","black"),lty=c(NA,NA,NA,1,2),cex = 0.6)


legend("topright",legend = c("low","medium","high"),
       fill = c(1,2,3),cex = 0.6,bty = "n")




