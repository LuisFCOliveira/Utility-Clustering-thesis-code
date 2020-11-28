



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
      
      
      
      
      
      #contar indices para trás
      for(i in c(n1:1)){
        if((vet1[i]+1)>lim[i]){
          
          b<- b+1
          
        }}
      
      #cat("b",b,"\n")
      
      
      if(!b==0){
        #1 indice para trás
        if(b==1){
          vet1[(n1-b)]<- vet1[(n1-b)] +1
          vet1[n1]<- vet1[(n1-b)]+1
          b<- 0
          mat<-rbind(mat,vet1)
        }
        
        #2 ou mais indices para trás  
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


# visualizar partição e medianas 

view_part<- function(part,k){
  
  n<- length(part) #numero de conjuntos
  
  cj<-c()
  at<-c()  # guarda nº do conjunto
  
  #juntar dados e respetiva matricula 
  for(i in c(1:n)){
    
    c<-unlist(part[i]) #conjunto
    n1<- length(c) # dados do conjunto
    at<- c(at,rep(i,n1)) # numero do conjunto
    cj<-c(cj,c)
    
  }
  
  ylim<- max(table(cj)/length(cj))
  
  mytable<- table(at,cj)/length(at)  #gráfico
  
  table2<- table(cj,at)/length(at) #mediana
  
  margin1<-margin.table(table2,2)
  
  
  col<- c("orangered","lightblue","lightyellow","lightgreen","lightpink")
  
  bar<-barplot(main=sprintf("Partition nº %d",k),mytable,col=col,ylim = c(0,round(ylim,1)+0.05),xlab ="Level",space=0.3,xpd = T,cex.main=0.8,cex.axis = 0.5)
  
  
  
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
  return(c(bar[ceiling(0.8*length(bar))],1.05*(round(ylim,1)+0.05)))
  
}







#função que devolve o ganho por partição e 4 mínimos

BF_min<- function(dataset,k,func){
  
  
  #funcao de mediana
  new_median<- function(dataset){
    
    n<- length(dataset)
    dataset<- sort(dataset)
    
    if(n %% 2 == 0){
      
      med<- dataset[floor(n/2)]
      
    } else{med<- dataset[(n+1)/2]}
    
    return(med)
  }
  
  #funcao de utilidade
  utility<-function(x){
    utbody<- eval(parse(text= func))
    return(utbody)
  }
  
  #funcao de perda 1 
  loss_function1<- function(dataset){
    
    med<- new_median(dataset)
    
    gain<- sum(abs(dataset-med))
    
    return(gain)
  }
  
  loss_function2<- function(dataset){
    
    
    med<- new_median(dataset)
    
    gain<- max(abs(dataset-med))
    
    return(gain)
  }
  
  
  #funcao que extrai a particao da matriz matpart
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
  
  #funcao que extraia n minimos e seus indices
  
  get_min<- function(dataset,n){
    
    len<- length(dataset)
    
    data<-c()
    start<- 1
    end<- floor(len/n)
    n1<- 0
    for(i in c(1:(n-1))){
      
      
      data<-c(data,order(dataset[start:end])[1]+n1) #somar ao indice o comprimento dos dados passados
      
      n1<- n1 + length(dataset[start:end])
      
      start<- end+1
      end<-(end+1) + floor(len/n)
      
    }
    
    data<-c(data,(order(dataset[(n1+1):len])[1]+n1)) #ultima parte
    
    
    return(data)
    
  }
  
  
  
  #inicio de calculos
  
  n<- length(dataset)
  
  dataset<- sort(dataset) #ordena os dados
  
  savepart<- vector(mode = "list")
  
  gross1<- loss_function1(utility(dataset)) #perda 1 dados
  gross2<- loss_function2(utility(dataset)) #perda 2 dados
  
  
  #caso k=1
  
  if(k==1){
    
    print("total data")
    
  }
  
  
  #caso k=2
  
  if(k==2){
    
    
    
    
    loss1<-c() # perda 1
    loss2<-c() #perda 2
    
    for(i in c(1:(n-1))){
      c1<-utility(dataset[1:i])
      c2<-utility(dataset[(i+1):n])
      
      
      k1<-loss_function1(c1)
      k2<-loss_function1(c2)
      
      k3<-loss_function2(c1)
      k4<-loss_function2(c2)
      
      
      
      loss1<-c(loss1,k1+k2)
      loss2<-c(loss2,k3+k4)
      
      
    }
    
    loss1<- loss1/gross1
    loss2<- (loss2/k)/gross2
    
    
    #Detetar 4 minimos e respetivas particoes
    
    nmin<- 4
    
    min1<- get_min(loss1,nmin) #parti????es perdas m??nimas 1
    min2<- get_min(loss2,nmin)  #parti????es perdas m??nimas 2
    
    
    #grafico
    
    
    par(bg="gray100")
    plot(main=sprintf("Loss 1 and infinit by partition, k=%d",k),y=loss1,x=c(1:(n-1)),pch = 19,col="blue",
         xlab = "Partition number",ylab = "Relative loss",ylim = c(min(loss1,loss2),max(loss1,loss2)))
    abline(h = mean(loss1),col="red",lty=2) #media da perda
    points(y=loss2,x=c(1:(n-1)),pch = 19,col="red")
    abline(h = mean(loss2),col="blue",lty=2) #media da perda
    points(min1,loss1[min1],pch=0,col="green",lwd=2)
    points(min2,loss2[min2],pch=0,col="green",lwd=2)
    for(i in c(1:(nmin-1))){
      abline(v=i*((n-1)/nmin +1),col="black") #divisoes do grafico para minimos
    }
    legend("bottomleft",legend = c("Loss 1","Mean loss 1","Loss infinit","Mean loss infinit","Minimum"),col =c("blue","red","red","blue","green"),
           lty=c(NA,2,NA,2),pch =c(19,NA,19,NA,0),cex = 0.5,inset=c(-0.15,-0.25),xpd=T)
    
    
    
    
    #particoes 
    
    par(mfrow=c(2,2))
    for(i in c(1:nmin)){
      
      #perda 1
      c1<-dataset[1:min1[i]]
      c2<-dataset[(min1[i]+1):n]
      
      bar<-view_part(list(c1,c2),min1[i]) #grafico de barras
      text(x=bar[1],y=bar[2],sprintf("Loss 1: %.4f",loss1[min1[i]]),cex = 0.7,xpd=NA)
      
      
    }
    
    
    #perda 2
    for(i in c(1:nmin)){
      
      c1<-dataset[1:min2[i]]
      c2<-dataset[(min2[i]+1):n]
      
      bar<-view_part(list(c1,c2),min2[i])
      text(x=bar[1],y=bar[2],sprintf("Loss infinit: %.4f",loss2[min2[i]]),cex = 0.7,xpd=NA)
      
    }
    
    par(mfrow=c(1,1))  # plot  window restored
    
    return(c(min(loss1),min(loss2)))
    
  }
  
  
  #caso k>2
  
  else{
    
    
    mat<- partmat(n,k) #matriz de separadores
    
    
    loss1<-c() #vetor para guardar o ganho 1
    loss2<-c() #vetor para guardar o ganho 2
    
    
    n1<- length(mat[,1]) #numero de particoes
    
    for(i in c(1:n1)){
      
      savepart<- get_part(dataset,mat[i,]) #funcao que extrai particao
      savepart<- lapply(savepart, utility) #aplicar utilidade 
      
      #calcular ganho segundo funcao gain_function 1 e 2
      
      
      
      part_loss1<-sum(unlist(lapply(savepart,FUN = loss_function1))) #perda 1 da particao
      
      part_loss2<-sum(unlist(lapply(savepart,FUN = loss_function2))) #perda 2 da particao
      
      loss1<- c(loss1,part_loss1) #adicao ao vetor perda 1
      loss2<- c(loss2,part_loss2) #adicao ao vetor perda 2
      
      savepart<-vector(mode = "list") #limpar particao
      
    }
    
    loss1<- loss1/gross1  #normalizar
    loss2<- (loss2/k)/gross2  #normalizar
    
    
    
    #4 minimos de particoes
    
    nmin<- 4 #numero de minimos
    
    min1<- get_min(loss1,nmin)
    min2<- get_min(loss2,nmin)
    
    
    #grafico
    
    par(bg="gray100")
    plot(main=sprintf("Loss 1 and infinit by partition, k=%d",k),y=loss1,x=c(1:n1),pch = 19,col="blue",
         xlab="Partition number",ylab = "Relative loss",ylim = c(min(loss1,loss2),max(loss1,loss2)))
    abline(h=mean(loss1),col="red",lty=2)
    points(y=loss2,x=c(1:n1),pch = 19,col="red")
    abline(h=mean(loss2),col="blue",lty=2)
    points(min1,loss1[min1],pch=0,col="green",lwd=2)
    points(min2,loss2[min2],pch=0,col="green",lwd=2)
    for(i in c(1:(nmin-1))){
      abline(v=i*((n1/nmin)+1),col="black")
    }
    legend("bottomleft",legend = c("Loss 1","Mean loss 1","Loss infinit","Mean loss infinit","Minimum"),col =c("blue","red","red","blue","green"),
           lty=c(NA,2,NA,2),pch =c(19,NA,19,NA,0),cex = 0.5,inset=c(-0.15,-0.25),xpd=T)
    
    #perda 1
    
    #partições
    
    par(mfrow=c(2,2))
    
    for(i in c(1:nmin)){
      
      p<-get_part(dataset,mat[min1[i],])
      bar<-view_part(p,min1[i])
      text(x=bar[1],y=bar[2],sprintf("Loss 1: %.4f",loss1[min1[i]]),cex = 0.7,xpd=NA)
      
    }
    
    
    #perda 2
    
    #particoes e tabela
    
    for(i in c(1:nmin)){
      
      p<-get_part(dataset,mat[min2[i],])
      bar<-view_part(p,min2[i])
      text(x=bar[1],y=bar[2],sprintf("Loss infinit: %.4f",loss2[min2[i]]),cex = 0.7,xpd=NA)
    }
    
    par(mfrow=c(1,1))  # plot  window restored
    
    return(c(min(loss1),min(loss2)))
    
  }
  
  
}

  