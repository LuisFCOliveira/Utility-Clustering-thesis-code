
partmat<- function(n,k){
  if(k>2){
    
    
    vet1<- c(1:(k-1)) #vetor de separadores
    n1<- length(vet1) #dimensão do vetor de separadores
    lim<-c() #vetor de limites dos separadores
    
    for(i in c(n1:1)){
      lim<- c(lim,n-i)
    }
    
    #cat("limites",lim,"\n")
    
    
    b<- 0 #indicador de limite
    mat<- matrix(vet1,ncol = n1)
    
    while(vet1[1]<lim[1]){
      
      #enquanto dentro do limite, preencher último
      
      if(vet1[n1]<lim[n1]){
        
        vet1[n1]<- vet1[n1]+1
        
        mat<-rbind(mat,vet1)
        
        
      }
      
      
      
      
      
      #contar indices para três
      for(i in c(n1:1)){
        if((vet1[i]+1)>lim[i]){
          
          b<- b+1
          
        }}
      
      #cat("b",b,"\n")
      
      
      if(!b==0){
        #1 indice para três
        if(b==1){
          vet1[(n1-b)]<- vet1[(n1-b)] +1
          vet1[n1]<- vet1[(n1-b)]+1
          b<- 0
          mat<-rbind(mat,vet1)
        }
        
        #2 ou mais indices para três  
        else{  
          vet1[(n1-b)]<- vet1[(n1-b)] +1
          for(i in c((n1-b+1):(n1))){
            vet1[i]<- vet1[i-1] + 1 #atualização anterior
            
          }
          mat<- rbind(mat,vet1)
          b<- 0
        }}
      
      
      
      
    }
    rownames(mat)<-c(1:length(mat[,1]))
    return(mat)
  }
  else{
    print("k deve ser maior do que 2")
    
  }
}




#funções auxiliares

#funcao de mediana
new_median<- function(dataset){
  
  n<- length(dataset)
  dataset<- sort(dataset)
  
  if(n %% 2 == 0){
    
    med<- dataset[floor(n/2)]
    
  } else{med<- dataset[(n+1)/2]}
  
  return(med)
  
}



#funcao de perda 1 
loss_function1<- function(dataset){
  
  med<- new_median(dataset)
  
  gain<- sum(abs(dataset-med))
  
  return(gain)
}

#função de perda infinit

loss_function2<- function(dataset){
  
  
  med<- new_median(dataset)
  
  gain<- max(abs(dataset-med))
  
  return(gain)
}


#obter partição do atributo

get_part<- function(dataset,order){
  
  #dataset: dados
  #order: linha matriz matpart
  
  n<- length(dataset)
  
  savepart<- vector(mode = "list") #lista para guardar particao
  
  start<- 1 #inicio do indice do conjunto
  for(j in order){
    
    
    savepart<- append(savepart,list(dataset[start:j]))
    start<- j+1
  }
  
  savepart<- append(savepart,list(dataset[start:n])) #??ltimo conjunto
  return(savepart)
  
}


#vizualizar partição

view_part<- function(part,k){
  
  #partição
  #k número de partição
  
  n<- length(part) #numero de conjuntos
  
  cj<-c()
  at<-c()
  
  #juntar dados e respetiva matricula 
  for(i in c(1:n)){
    
    c<-unlist(part[i]) #conjunto
    n1<- length(c) # dados do conjunto
    at<- c(at,rep(i,n1)) # numero do conjunto
    cj<-c(cj,c)
    
  }
  
  ylim<-max(table(cj)/length(cj))
  
  
  mytable<- table(at,cj)/length(at)  #gráfico
  
  table2<- table(cj,at)/length(at) #mediana
  
  margin1<-margin.table(table2,2)
  
  
  col<- rainbow(n)
  
  bar<-barplot(main=sprintf("Partition %s",as.character(k)),mytable,col=col,ylim = c(0,round(ylim,1)+0.05),xlab ="Values",space=0.3,xpd = T,cex.main=0.8,cex.names = 0.5,cex.axis = 0.5,las=2)
  
  
  
  #inserir linha de mediana
  
  oldindex<- 0 #comparação com index anterior
  atsum<- 0 #cumulação atributo
  
  for(i in c(1:n)){
    
    
    index<- 1 #atributo onde a mediana está
    
    data<-table2[,i]/margin1[i] #coluna conjunto i
    
    
    sum<- data[index] #cumulação conjunto
    
    #ciclo para verificar em que atributo a mediana está
    while (sum<0.5) {
      
      index<- index+1
      
      sum<- sum + data[index]
      
    }
    
    
    
    
    if(oldindex==index){
      
      atsum<- atsum + table2[index,][i-1]
      
      
      
      y<- (data[index]-abs(sum-0.5))*margin1[i] + atsum
      
      
      
      points(x=bar[index],y=y,pch=19,cex=0.5)
      
    }
    
    else{
      
      
      
      if(i>1){
        
        y<- (data[index]-abs(sum-0.5))*margin1[i] + table2[index,][i-1]
        
      }
      
      if(i==1){
        
        y<- (data[index]-abs(sum-0.5))*margin1[i]
        
      }
      
      
      points(x=bar[index],y=y,pch=19,cex=0.5)
      
      oldindex<- index
      
      atsum<- 0
      
      
    }
    
  }
  
  return(c(bar[ceiling(0.8*length(bar))],1.01*(round(ylim,1)+0.05)))
  
}



#devolve partição final a partir da partição por atributo partatr

part_def<- function(dataset,partatr){
  
  
  agreg<- c()
  
  newpart<- vector(mode = "list")
  
  #criar cada partição
  
  for(j in partatr ){
    
    for( a in j){
      
      agreg<-c(agreg,dataset[dataset==a])
    }
    
    newpart<- append(newpart,list(agreg))
    
    agreg<- c()
    
    
  }
  
  return(newpart)
}


#Brute Forte

BF<- function(dataset,k,func,plot=T,loss_method="l1"){
  
  library(tictoc)
  
  #l1  barras perda 1
  #linf perda infinita
  
  #função de utilidade incorporada
  
  utility<-function(x){
    utbody<- eval(parse(text= func))
    return(utbody)
  }
  
  tic(quiet = TRUE) #medir tempo
  
  atr<- sort(unique(dataset))  #lista de atributos
  
  gross1<- loss_function1(utility(dataset)) #perda 1 dados
  gross2<- loss_function2(utility(dataset)) #perda 2 dados
  
  loss1<-c()
  loss2<-c()
  
  #mínimo absoluto
  
  
  
  
  if(k==2){
    
    
    n1<- length(atr)-1
    
    min1<- 0
    min2<- 0
    
    #cat("Início","\n")
    
    #cat("Número de partições",n1,"\n")
    
    for(i in c(1:(length(atr)-1))){
      
      c1<-atr[1:i]
      c2<-atr[(i+1):length(atr)]
      
      partatr<-list(c1,c2) #partição de atributos
      
      part<-lapply(part_def(dataset,partatr),utility) #partição i obtida com utilidade 
      
      part_loss1<-sum(unlist(lapply(part,FUN = loss_function1))) #perda 1 da particao
      
      part_loss2<-sum(unlist(lapply(part,FUN = loss_function2))) #perda 2 da particao
      
      
      loss1<- c(loss1,part_loss1) #adicao ao vetor perda 1
      loss2<- c(loss2,part_loss2) #adicao ao vetor perda 2
      
      
      
      
      
    }
    
    #representação partições mínimos
    
    loss1<- loss1/gross1  #normalizar
    loss2<- (loss2/k)/gross2  #normalizar
    
    min1<-order(loss1)[1]
    min2<-order(loss2)[1]
    
    #perda 1
    
    c1<-atr[1:min1]
    c2<-atr[(min1+1):length(atr)]
    
    partatr1<-list(c1,c2) #partição de atributos
    
    part1<- part_def(dataset,partatr1)
    
    
    #perda infinit
    
    c1<-atr[1:min2]
    c2<-atr[(min2+1):length(atr)]
    
    partatr2<-list(c1,c2) #partição de atributos
    
    part2<-part_def(dataset,partatr2)
    
  }
  
  if(k>=3){
    
    
    
    atrmat<- partmat(length(atr),k) # matriz da partição dos atributos
    
    n1<- length(atrmat[,1]) # número de partições
    
    
    
    #correr todas as partições
    for(i in c(1:n1)){
      
      partatr<- get_part(atr,atrmat[i,]) # obter todas as partições
      
      
      part<-lapply(part_def(dataset,partatr),utility) #partição i obtida com utilidade
      
      
      part_loss1<-sum(unlist(lapply(part,FUN = loss_function1))) #perda 1 da particao
      
      part_loss2<-sum(unlist(lapply(part,FUN = loss_function2))) #perda 2 da particao
      
      loss1<- c(loss1,part_loss1) #adicao ao vetor perda 1
      loss2<- c(loss2,part_loss2) #adicao ao vetor perda 2
      
    }
    
    
    loss1<- loss1/gross1  #normalizar
    loss2<- (loss2/k)/gross2  #normalizar
    
    #mínimo absoluto
    
    min1<- order(loss1)[1] 
    min2<- order(loss2)[1] 
    
    #representação partições mínimos
    
    part1<- part_def(dataset,get_part(atr,atrmat[min1,]))  
    part2<- part_def(dataset,get_part(atr,atrmat[min2,]))
  }
  
  
  #gráfico
  
  if(plot==T){
    
    par(mfrow=c(1,1))
    
    par(bg="gray100")
    plot(main=sprintf("Loss 1 and infinit by partition, k=%d",k),y=loss1,x=c(1:n1),pch = 19,cex=0.7,col="blue",
         xlab="Nº de partição",ylab = "Perda",ylim = c(min(loss1,loss2),max(loss1,loss2)))
    abline(h=mean(loss1),col="red",lty=2)
    points(y=loss2,x=c(1:n1),pch = 19,col="red",cex=0.7)
    abline(h=mean(loss2),col="blue",lty=2)
    points(min1,loss1[min1],pch=0,col="green",lwd=2)
    points(min2,loss2[min2],pch=0,col="green",lwd=2)
    legend("bottomleft",legend = c("Perda 1","Perda média 1","Perda infinit","Perda média infinit","Mínimo"),col =c("blue","red","red","blue","green"),
           lty=c(NA,2,NA,2),pch =c(19,NA,19,NA,0),cex = 0.5,inset=c(-0.20,-0.30),xpd=T)
    
  }
  
  #representação partições mínimos
  
  
  if(loss_method=="l1"){
    
    bar1<-view_part(part1,sprintf("Brute Force, k= %d",k))
    text(x=bar1[1],y=bar1[2],adj=1,sprintf("Loss 1: %.4f",loss1[min1]),cex=0.7,xpd=T)
    
  }
  
  if(loss_method=="linf"){
    
    bar2<-view_part(part2,sprintf("Brute Force,k= %d",k))
    text(x=bar2[1],y=bar2[2],adj=1,sprintf("Loss infinit: %.4f",loss2[min2]),cex=0.7,xpd=T)
    
  }
  
  
  time<- toc(quiet=TRUE)
  
  tm<- as.numeric(round(time$toc - time$tic,5))
  
  #cat("Tempo de implementação Brute Force:",tm,"segundos","\n")
  
  #tm running time
  #n1 nº partitions
  
  return(c(loss1[min1],loss2[min2],tm,n1))
  
}

#Implementação CART

divide<- function(dataset,func,loss_method="l1"){
  
  #dataset: dados
  # func: funão de utilidade
  #loss_method: l1= perda 1 / linf= perda infinito
  
  utility<-function(x){
    utbody<- eval(parse(text= func))
    return(utbody)
  }
  
  n<- length(dataset)
  
  #função que minimiza a perda da divisão do conjunto
  atr<- sort(unique(dataset))
  
  
  #perda 1
  
  
  if(loss_method=="l1"){
    # devem existir no conjunto mais do que 1 atributo
    if(length(atr)>1){
      
      n<- length(atr)
      
      loss<- c()
      
      for(i in c(1:(n-1))){
        
        c1<-atr[1:i]
        c2<-atr[(i+1):n]
        
        part<-part_def(dataset,list(c1,c2)) #partição com os dados
        
        part<-lapply(part,utility)
        
        part_loss1<-sum(unlist(lapply(part,FUN = loss_function1))) #perda 1 da particao
        
        loss<-c(loss,part_loss1)
      }
      min<- order(loss)[1]  #minimizar a perda
      
      c1<-atr[1:min]
      c2<-atr[(min+1):n]
      
      part<- part_def(dataset,list(c1,c2))
    }
  }
  
  
  #perda infinito
  
  if(loss_method=="linf"){
    
    if(length(atr)>1){
      
      n<- length(atr)
      
      loss<- c()
      
      for(i in c(1:(n-1))){
        
        c1<-atr[1:i]
        c2<-atr[(i+1):n]
        
        part<-part_def(dataset,list(c1,c2)) #partição com os dados
        
        part<-lapply(part,utility)
        
        part_loss2<-sum(unlist(lapply(part,FUN = loss_function2))) #perda 1 da particao
        
        loss<-c(loss,part_loss2)
      }
      min<- order(loss)[1]  #minimizar a perda
      
      c1<-atr[1:min]
      c2<-atr[(min+1):n]
      
      part<- part_def(dataset,list(c1,c2))
      
    }}
  
  return(part)
  
}




CART_met<- function(dataset,k,func,loss_method ="l1"){
  
  library(tictoc)
  
  #dataset= dados
  #func= função de utilidade
  #loss: l1= perda 1 / linf= perda infinito
  
  #função de utilidade
  
  utility<-function(x){
    utbody<- eval(parse(text= func))
    return(utbody)
  }
  
  #método CART onde se inserem dados e se obtem a melhor partição em k conjuntos
  
  if(length(unique(dataset))<k){
    
    print("Impossível dividir dados")
  }
  
  else{
    
    tic(quiet = TRUE)
    
    if(loss_method=="l1"){
      
      gross<- loss_function1(utility(dataset)) #perda dados
      
      
      new_part<- divide(dataset,func)  #dar início à divisão com k=2
      
      min<-1
      
      
      loss<- c(sum(unlist(lapply(lapply(new_part,utility),FUN = loss_function1)))) #perda divisão em 2
      
      count<- 2 #contar conjuntos
      
      
      while(count<k){
        
        n<- length(new_part) #número de conjuntos da partição
        
        loss<- c() #perda
        
        #correr os conjuntos da partição
        
        for(i in c(1:n)){
          
          div<- unlist(new_part[i]) #retirar de lista o cojunto i
          
          
          #condição de haver mais do que 1 atributo no conjunto
          if(length(unique(div))>1){
            
            
            cj<- divide(div,func) #dividir o conjunto i
            
            part<- append(new_part[-i],cj,after=i-1) #criar a partição do conjunto i com os restantes
            
            part_loss1<-sum(unlist(lapply(lapply(part,utility),FUN = loss_function1))) #calcular a perda da partição final
            
            loss<- c(loss,part_loss1) #adicionar valor de perda
            
          }
          
          else{
            
            loss<-c(loss,gross)  #adicionar no lugar de um conjunto com único atributo a perda dos dados para não ser selecionado
            
            next
          }
        }
        
        min<- order(loss)[1]
        
        cj<-divide(unlist(new_part[min]),func) #dividir o conjunto i
        
        new_part<- append(new_part[-min],cj,after = min-1) #construir a nova partição
        
        count<- count+1
        
      }}
    
    if(loss_method=="linf"){
      
      gross<- loss_function2(utility(dataset)) #perda dados
      
      
      new_part<- divide(dataset,func,loss_method="linf")  #dar início à divisão com k=2
      
      min<-1
      
      
      loss<- c(sum(unlist(lapply(lapply(new_part,utility),FUN = loss_function2))))/2 #perda divisão em 2
      
      count<- 2 #contar conjuntos
      
      
      while(count<k){
        
        n<- length(new_part) #número de conjuntos da partição
        
        loss<- c() #perda
        
        #correr os conjuntos da partição
        
        for(i in c(1:n)){
          
          #condição de o conjunto ter mais do que um elemento
          
          div<- unlist(new_part[i]) #retirar de lista o cojunto i
          
          
          #condição de haver mais do que 1 atributo no conjunto
          if(length(unique(div))>1){
            
            
            cj<- divide(div,func,loss_method="linf") #dividir o conjunto i
            
            part<- append(new_part[-i],cj,after=i-1) #criar a partição do conjunto i com os restantes
            
            part_loss2<-sum(unlist(lapply(lapply(part,utility),FUN = loss_function2)))/k #calcular a perda da partição final
            
            loss<- c(loss,part_loss2) #adicionar valor de perda
            
          }
          
          else{
            
            loss<-c(loss,gross)  #adicionar no lugar de um conjunto com único atributo a perda dos dados para não ser selecionado
            
            next
          }
        }
        
        min<- order(loss)[1]
        
        cj<-divide(unlist(new_part[min]),func,loss_method="linf") #dividir o conjunto i
        
        new_part<- append(new_part[-min],cj,after = min-1) #construir a nova partição
        
        count<- count+1
        
      }}
    
    
    lossmin<-loss[min]/gross
    
    bar1<-view_part(new_part,sprintf("CART, k= %d",k)) #ver o gráfico de barras
    # perda 1
    if(loss_method =="l1"){
      text(x=bar1[1],y=bar1[2],adj=1,sprintf("Loss 1: %.4f",lossmin),cex=0.7,xpd=T)
    }
    #perda infinit
    if(loss_method =="linf"){
      text(x=bar1[1],y=bar1[2],adj=1,sprintf("Loss infinit: %.4f",lossmin),cex=0.7,xpd=T)
    }
    
    
    time<- toc(quiet=TRUE)
    
    tm<-as.numeric(round(time$toc-time$tic,5))
    
    #cat("Tempo de implementação CART:", tm,"segundos","\n")
    
    
    return(c(lossmin,tm))
    
  }
  
}


#Gráficos importantes para análise de dados: k1= k máximo
data_graphs<- function(dataset,func,k1,k2=0){
  
  #k1 k brute force
  # k2 cart para ir mais longe
  
  k<- c(2:(k1+k2+1))
  
  
  loss_bf<- c()
  loss_cart<- c()
  
  time_bf<- c()
  time_cart<- c()
  npart<- c()
  
  par(mfrow=c(2,2))
  
  for(i in k){
    
    
    if(i<=(k1+1)){
    bf<- BF(dataset,k = i,func = func,plot = F)
    cart<- CART_met(dataset = dataset,k = i,func = func)
    
    loss_bf<- append(loss_bf,bf[1])
    loss_cart<- append(loss_cart,cart[1])
    
    time_bf<- append(time_bf,bf[3])
    time_cart<- append(time_cart,cart[2])
    
    npart<- append(npart,bf[4])
    }
    
    else{
      
      
      cart<- CART_met(dataset = dataset,k = i,func = func)
      loss_cart<- append(loss_cart,cart[1])
      loss_bf<- append(loss_bf,NaN)
      
      time_bf<- append(time_bf,NaN)
      time_cart<- append(time_cart,cart[2])
      npart<- append(npart,NaN)

    }
    
    
  }
  
  par(mfrow=c(1,1))
  
  plot(k,loss_bf,col="blue",pch=19,main="Loss 1 in k",ylim = c(0,1),ylab = "Relative loss",axes=F,xlab="k")
  points(k,loss_cart,col="red",pch=17)
  axis(side= 1,at = k)
  axis(side = 2,at=seq(0,1,0.1))
  abline(h=0.2,col="blue",lty=2)
  text(x = 0.8*max(k),y=1.05*0.2,labels = "0.2",cex = 0.5)
  abline(h=0.1,col="red",lty=2)
  text(x = 0.8*max(k),y=1.05*0.1,labels = "0.1",cex = 0.5)
  abline(h=0.05,col="green",lty=2)
  text(x = 0.8*max(k),y=1.05*0.05,labels = "0.05",cex = 0.5)
  

  count<- 2
  while(count<=(k1+1)){
    
    text(xpd = T,x = count, pos = 3, cex = 0.7, y= loss_cart[count-1] ,label = sprintf("%.1f %% ", 100*(1-abs(loss_bf[count-1]-loss_cart[count-1])/(loss_bf[count-1]+loss_cart[count-1]))))
    count<- count + 1
    
  }

  legend("topleft",legend = c("Brute Force","CART"),col=c("blue","red"),pch=c(19,17),cex=0.7)
  
  plot(main="Runtime",x=k,y=(time_bf),type="l",col="blue",xlab="k",ylab = "Time /s",ylim = c(0,max(time_bf[!is.na(time_bf)])))
  lines(x=k,y=(time_cart),col="red")
  legend("topleft",legend = c("Brute Force","CART","Total partitions","top: time ratio"),col=c("blue","red","black",NaN),lty=c(1,1,NaN,NaN),pch = c(NaN,NaN,19,NaN),cex=0.8)
  par(new=T)
  plot(x=k,y=npart,axes=F,pch=19,ylab = NA,xlab = "",ylim = c(0,1.05*max(npart[!is.na(npart)])))
  axis(side = 4)
  mtext(side=4,text="Partition number",line=0)
  text(k,npart,labels = round(time_bf/time_cart,3),pos = 3,cex=0.7)
  
}





