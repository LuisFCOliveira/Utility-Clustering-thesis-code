#combinações sem repetição
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}


#numero de sterling:  partições de k conjuntos de n dados

ster<- function(n,k){
  
  if(k<=n){
    sum<-0
    for(i in seq(0,k)){
      
      sum<- sum+(-1)**i*comb(k,i)*(k-i)**n
    }
    return(sum/factorial(k))
  }
  else
    print("k é maior do que n")
}




#numero bell: total de partições com n elementos

bell<- function(n){
  sum<-0
  for(i in seq(1,n)){
    sum<- sum+ster(n,i)
  }
  
  return(sum)
}



#função que encontre todas as partições de um conjunto em k subconjuntos


part1<- function(dataset,k){
  
  if(length(unique(dataset))<= k | k<=0 ){
    
    stop("Número de conjuntos impossivel")
    
  }
  
  else{
    
    n<- length(dataset)
    seq<- rep(1,n)
    df<- vector(mode = "numeric",length = n) #vetor que guarda as partições por ciclo
    ls<- data.frame() #matriz para guardar todas as partições
    sp<-bell(n) #total de partições
    count<- 1 #contador para terminar ciclo
    while(count<sp){
      
      for(i in c(1:n)){df[i]=seq[i]}
      ls<- rbind(ls,df)
      startIndex<- n-1
      count<- count+1
      while(startIndex>=1){
        
        
        maxValue<- max(seq[1:startIndex])
        if(seq[startIndex+1] > maxValue | maxValue > k | seq[startIndex+1] >= k){
          seq[startIndex+1]<- 1
          startIndex <- startIndex - 1
        }
        
        else{seq[startIndex+1] <- seq[startIndex+1]+1
        break
        }}}
    ls<- unique(ls) #remove linhas repetidas
    
    #filtro para eliminar linhas onde k nao aparece
    del<-c()
    for(i in c(1:length(ls[,1]))){
      
      if(!all(c(1:k)%in%ls[i,]) ){
        del<- c(del,i)}
    }
    
    ls<- ls[-del,]
    row.names(ls)<- c(1:length(ls[,1])) #numero da linha:partição
    col<-c() #numero da coluna: nivel do atributo
    for(i in c(1:n)){
      
      col<- c(col,sub(1,x="Lev1",replacement = i))}
    
    colnames(ls)<- col
    return(ls)
  }}



#função de mediana com o novo critério
new_median<- function(dataset){
  
  n<- length(dataset)
  dataset<- sort(dataset)
  
  if(n %% 2 == 0){
    
    med<- dataset[floor(n/2)]
    
  } else{med<- dataset[(n+1)/2]}
  
  return(med)
  
}





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



#representar partições // atualização



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
  
  bar<-barplot(main=sprintf("Partition nº %d",k),mytable,col=col,ylim = c(0,ylim+0.1),xlab ="Level",space=0.3,xpd = T,cex.main=0.8,cex.axis = 0.5)
  
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
  return(c(bar[ceiling(0.8*length(bar))],max(mytable)+0.05))
  
}




#função que encontra as partições de um conjunto de dados em k subconjuntos e calcula as suas utilidades

part_data<- function(dataset,k,func="(x+2)**2",all=T){
  
  #dado um conjunto de eventos com um atributo manifestado, organiza segundo as partições
  
  if(class(dataset)=="numeric"){    
    lev<-sort(unique(dataset)) #níveis do atributo ordenados
  } else{ print("Tipo de dados não comportado")}
  
  
  part_matrix<- part1(lev,k) #matriz das partições
  
  
  #definir relação de utilidade
  
  
  
  utility<-function(x){
    
    utbody<- eval(parse(text= func))
    return(utbody)
  }
  
  
  #correr matriz de partições
  
  
  partk<- vector("list",k)
  
  #matriz de utilidades minimas e máximas de uma partição
  
  
  matut<- matrix(nrow = length(part_matrix[,1]),ncol = 2)
  rownames(matut)<- c(1:length(matut[,1])) #numerar partições na matriz
  
  
  
  
  
  #corre o numero de partições
  for(i in c(1:length(part_matrix[,1]))){
    
    #corre os niveis
    for(j in c(1:length(lev))){
      
      conj<-dataset[dataset==lev[j]] #dados de acordo com atributo
      
      pindex<-as.numeric(part_matrix[i,][j]) #indice do conjunto j da partição i
      
      partk[[pindex]]<- c(partk[[pindex]],conj)
      
      
    }
    
    #cat("Partição",i,"\n")
    #print(lapply(X = partk,FUN = unique)) #devolve a partição de acordo com atributos
    
    #vizualizar partição /atualização
    
    medpart<-lapply(X = partk,FUN = new_median) #mediana aplicada aos conjuntos da partição
    utmedpart<- lapply(X = medpart,FUN = utility) #aplicada a função de utilidade às medianas de cada conjunto
    
    
    
    utmin<-min(as.numeric(utmedpart)) #utilidade minima
    utmax<- max(as.numeric(utmedpart))#utilidade maxima
    
    matut[i,]<- c(utmin,utmax) #matriz com Um e UM das partições 
    
    
    if(utmin==min(utility(dataset)) & utmax==max(utility(dataset))|all==T){
    
    bar<-view_part(partk,i)
    text(x=0.5*bar[1],y=bar[2],adj=1,sprintf("Um: %.2f",utmin),cex=0.7,xpd=T)
    text(x=bar[1],y=bar[2],adj=1,sprintf("UM: %.2f",utmax),cex=0.7,xpd=T)
    }
    
    partk<-vector("list",k)
    
    
  }
  
  
  #definir utilidade dos dados
  
  datmed<- new_median(dataset)
  Ugross<- utility(datmed) #utilidade do conjunto de dados
  
  #definir utilidade da partição fina
  
  Umfin<- utility(min(dataset)) #utilidade minima da partição fina
  UMfin<- utility(max(dataset)) #utilidade maxima da partição fina
  
  matut<-data.frame(matut)
  colnames(matut)<- c("Um","UM")
  
  
  #representação gráfica
  
  par(mfrow=c(1,1))
  
  plot(matut[,1],type = 'b',main="Minimum utility by partition",
       ylim = c(Umfin-5,max(matut[,1],Ugross+5)),xlim = c(0,length(matut[,1])),xlab = "Partition",ylab = "Um(P)",axes=F)
  axis(side=1,at=c(1:length(part_matrix[,1])))
  axis(side=2,seq(Umfin-5,Ugross+5,1))
  abline(h=Umfin,col="red",lty=2)
  abline(h=Ugross,col="green",lty=2)
  legend("topright",legend = c("Singleton partition","Trivial partition"),
         col=c("red","green"),pch = 19,cex = 0.8)
  
  plot(matut[,2],type='b',main="Maximum utility by partition",
       ylim=c(min(matut[,2],Ugross)-5,UMfin+5),xlim = c(0,length(matut[,2])),ylab = "UM(P)",xlab = "Partition",axes=F)
  axis(side=1,at=c(1:length(part_matrix[,1])))
  axis(side=2,seq(0,UMfin+5,1))
  abline(h = UMfin,col="red",lty=2)
  abline(h = Ugross,col="green",lty=2)
  legend("bottomright",legend = c("Singleton partition","Trivial partition"),
         col=c("red","green"),pch = 19,cex=0.8)
  
  
}






