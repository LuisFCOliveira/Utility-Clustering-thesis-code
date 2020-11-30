#combinações sem repetição
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}


#numero de stirling:  partições de k conjuntos de n dados

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
#part1(c(1,2,3,4),4)


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
      
      col<- c(col,sub(1,x="P1",replacement = i))}
    
    colnames(ls)<- col
    
    return(ls)
  }
  }

# gives median through frequency
freq_median<- function(data,freq){
  
  i<- 1
  sum<- 0
  while(sum<0.5){
    
    median<- data[i]
    sum<- sum + freq[i]
    i<- i+1
  }
  return(median)
}




#get partition from a dataset with attributes and frequencies

# only works for 2  D

#function to get median of data

get_median<- function(dataset,freq,mat){
  
  medians<- vector(mode = "list",length = 2)
  
  for(i in c(1:2)){
    
    pt<- make_part(dataset[,i],mat = unlist(mat),freq=freq)
    
    partition<- pt[[1]]
    
    freq_part<- pt[[2]]
    
    ord<-lapply(partition,order) # orders elements
    
    partition<- mapply(function(x,y) x[y],x=partition,y=ord,SIMPLIFY = F)
    
    freq_part<- mapply(function(x,y) x[y],x=freq_part,y=ord,SIMPLIFY = F)
    
    freq_set<-lapply(freq_part,sum)  # partial sum of frequencies
    
    
    freq_new<-vector(mode = "list",length = length(partition)) #save new partial frequencies inside partition
    
    #small cycle to adjuste partial frequencies
    count<- 1
    for(j in freq_set){
      freq_new[[count]]<-freq_part[[count]]/j
      count<- count+1
    }
    
    
    med<- mapply(freq_median,partition,freq_new,SIMPLIFY = F)
    
    
    medians[[i]]<- unlist(med)
    
  }
  
  
  return(medians)
  
  
  
  
}


get_part_BF<- function(dataset,freq,mat,loss,configuration=F){
  
  medians<- get_median(dataset,freq,mat)
  
  
  palette(c(
    "dodgerblue2", "#E31A1C", # red
    "green4",
    "#6A3D9A", # purple
    "#FF7F00", # orange
    "gold1",
    "skyblue2", "#FB9A99", # lt pink
    "palegreen2",
    "#CAB2D6", # lt purple
    "#FDBF6F", # lt orange
    "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
    "darkturquoise", "green1", "yellow4", "yellow3",
    "darkorange4", "brown"
  ))
  
  k<- max(mat) # number of sets
  pch<- c(15,16,17,18,if(k>4){sample(c(15,16,17,18),k-4,replace = T)})
  
  colnam<- colnames(dataset)
  
  plot(dataset[,1],dataset[,2],col=mat,xlab = colnam[1] ,ylab=colnam[2],main=sprintf("BF k=%.0f, Loss= %.4f",k,loss),pch=pch[mat],
       axes=F,xpd=T,cex.main=0.7)
  points(unlist(medians[[1]]),unlist(medians[[2]]),pch=0 ,cex=2,xpd=T)
  legend("bottomleft", inset=c(-0.2,-0.3), legend=c("Median"), pch=0,cex=0.45,xpd=T)
  
  xlab<- c(1:(max(dataset[,1])+1))
  ylab<- c(1:(max(dataset[,2])+1))
  axis(side= 1,at = xlab,labels = T,cex.axis=0.6)
  axis(side = 2,at= ylab,labels = T,cex.axis=0.6,font = 3)
  text(x = dataset[,1],y=dataset[,2],labels = round(freq,3),cex = 0.7,xpd=NA,pos=4)
  
  if(configuration== T){
  return(mat)
  }
}



# function that makes all partiitons
BF_2D<- function(dataset,k,func1,func2){
  
  library(tictoc)
  
  tic(quiet = TRUE)
  
  
  nline<- length(dataset[,1]) # number of attributes
  ncoll<- length(dataset[1,])-1
  
  
  #utility functions
  ut1<-function(x){
    utbody<- eval(parse(text= func1))
    return(utbody)
  }
  
  ut2<-function(x){
    utbody<- eval(parse(text= func2))
    return(utbody)
  }
  
  mat<- part1(c(1:nline),k) # matrix of partition number
  
  n_part<- dim(mat)[1] # number of partitions
  
  #print(mat)
  
  freq<- dataset[,ncoll+1] # list of frequencies
  dataset<- dataset[,-(ncoll+1)]
  
  itr<- 1 # number of attribute
  
  total_ut<- matrix(nrow = length(mat[,1]),ncol = ncoll)  # store utility values
  
  
  
  
  #iterate through all attribute
  
  gross<-0  # sum of utility of dataset
  
  
  while(itr<= ncoll){
    
    
    
    partition<-vector(mode = "list",length = k) # save partition
    freq_part<-vector(mode = "list",length = k) # save frequencies
    
    util<- eval(parse(text=sub(1,replacement = itr,x="ut1"))) # alternate utility function with iteraction
    
    #calculate utility of data
    
    
    a<-dataset[,itr] # dataset
    
    gross_ord<- order(a) # order of elements 
    a<- a[gross_ord] # order the dataset
    
    b<-freq[gross_ord] # order the frequencies
    
    c<- util(freq_median(a,b)) #median
    
    a<- util(a) # aplly utility
    
    
    gross<- gross+sum(unlist(mapply(function(x,y,z){z*abs(x-y)},x=a,y=c,z=b,SIMPLIFY = F)))
    
    
    
    
    for(nbt in c(1:length(mat[,1]))){
      
      #cat("Number of partition",nbt,"\n")
      
      #nbt number of partition
      
      i<- 1 # number of point in dataset
      
      for(j in mat[nbt,]){
        # set number of partition 
        
        
        partition[[j]]<- append(partition[[j]],dataset[i,itr])
        freq_part[[j]]<- append(freq_part[[j]],freq[i])
        i<- i+1
      }
      
      ord<-lapply(partition,order) # orders elements
      
      partition<- mapply(function(x,y) x[y],x=partition,y=ord,SIMPLIFY = F)
      
      #print("partition")
      #print(partition)
      
      freq_part<- mapply(function(x,y) x[y],x=freq_part,y=ord,SIMPLIFY = F)
      
      freq_set<-lapply(freq_part,sum)  # partial sum of frequencies
      
      
      freq_new<-vector(mode = "list",length = k) #save new partial frequencies inside partition
      
      #small cycle to adjuste partial frequencies
      count<- 1
      for(i in freq_set){
        freq_new[[count]]<-freq_part[[count]]/i
        count<- count+1
      }
      
      
      med<- mapply(freq_median,partition,freq_new,SIMPLIFY = F)
      
      
      
      #we have the partition and the medians 
      # apply utility
      
      
      utpart<-lapply(partition,util);#print("utility partition");#print(utpart)
      utmed<-lapply(med,util);#print("utility median");#print(utmed)
      
      
      part_ut<-sum(unlist(mapply(function(x,y,z){z*abs(x-y)},x=utpart,y=utmed,z=freq_part,SIMPLIFY = F)))
      #print("partition utility")
      #print(mapply(function(x,y,z){z*abs(x-y)},x=utpart,y=utmed,z=freq_part,SIMPLIFY = F))
      
      
      total_ut[nbt,itr]<- part_ut
      
      #part_ut is the utility of the two sets for the itr attribute
      
      
      partition<-vector(mode = "list",length = k) #clean 
      freq_part<-vector(mode = "list",length = k) #clean 
      freq_new<-vector(mode = "list",length = k) #clean 
    }
    
    #cat("Attribute",itr,"finished","\n")
    itr<- itr+1
    
  }
  
  
  total_ut<- unlist(rowSums(total_ut))
  
  minimum<- order(total_ut)[1]  #get minimum utility
  
  get_part_BF(dataset,freq,unlist(mat[minimum,]),total_ut[minimum]/gross)
  
  time<- toc(quiet=TRUE)
  
  tm<-as.numeric(round(time$toc-time$tic,5))
  
  return(c(total_ut[minimum]/gross,tm,n_part))
  
}

#makes plot of cart

get_part_cart<- function(dataset,freq,mat,loss,configuration=F){
  
  k<- max(mat) # number of sets
  
  #calculate medians
  
  medians<- get_median(dataset,freq,mat)
  
  palette(c(
    "dodgerblue2", "#E31A1C", # red
    "green4",
    "#6A3D9A", # purple
    "#FF7F00", # orange
    "gold1",
    "skyblue2", "#FB9A99", # lt pink
    "palegreen2",
    "#CAB2D6", # lt purple
    "#FDBF6F", # lt orange
    "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
    "darkturquoise", "green1", "yellow4", "yellow3",
    "darkorange4", "brown"
  ))
  
  pch<- c(15,16,17,18,if(k>4){sample(c(15,16,17,18),k-4,replace = T)}) 
  
  colnam<- colnames(dataset)
   
  plot(dataset[,1],dataset[,2],col=mat,xlab = colnam[1],ylab=colnam[2],main=sprintf("CART k=%.0f, Loss= %.4f",k,loss),pch = pch[mat],
       axes = F,xpd=T,cex.main=0.7)
  points(unlist(medians[[1]]),unlist(medians[[2]]),pch= 0,cex=2,xpd=T)
  
  legend("bottomleft", inset=c(-0.2,-0.3), legend=c("Median"), pch=0 ,cex=0.45,xpd=T)
  
  xlab<- c(1:(max(dataset[,1])+1))
  ylab<- c(1:(max(dataset[,2])+1))
  axis(side= 1,at = xlab,labels = T,cex.axis=0.6)
  axis(side = 2,at= ylab,labels = T,cex.axis=0.6,font=3)
  text(x = dataset[,1],y=dataset[,2],labels = round(freq,3),cex = 0.7,xpd=NA,pos=4)
  
   if(configuration== T){
  return(mat)
  } 
  
  
}

# function that gives the partition and the frequency with the mat vector, mat gives the set 
make_part<- function(dataset,mat,freq){
  
  
  partition<- vector(mode = "list",length = max((unique(mat))))
  freq_part<-  vector(mode = "list",length= max((unique(mat))))
  
  for(i in c(1:length(mat))){
    
    
    partition[[mat[i]]]<- append(partition[[mat[i]]],dataset[i])
    freq_part[[mat[i]]]<- append(freq_part[[mat[i]]],freq[i])
  }
  
  return(list(partition,freq_part)) 
  
}

#function that gives the utility of a partition

ut_part<- function(partition,freq_part,func){
  
  #calculates utility of partition given a partition, it's frequencies and a utility function
  
  ord<-lapply(partition,order) # orders elements
  
  partition<- mapply(function(x,y) x[y],x=partition,y=ord,SIMPLIFY = F)
  
  freq_part<- mapply(function(x,y) x[y],x=freq_part,y=ord,SIMPLIFY = F)
  
  freq_set<-lapply(freq_part,sum)  # partial sum of frequencies
  
  
  freq_new<-vector(mode = "list",length = length(partition)) #save new partial frequencies inside partition
  
  #small cycle to adjuste partial frequencies
  count<- 1
  for(i in freq_set){
    freq_new[[count]]<-freq_part[[count]]/i
    count<- count+1
  }
  
  
  med<- mapply(freq_median,partition,freq_new,SIMPLIFY = F)
  
  #we have the partition and the medians 
  # apply utility
  
  
  utpart<-lapply(partition,func)
  utmed<-lapply(med,func)
  
  
  part_ut<-sum(unlist(mapply(function(x,y,z){z*abs(x-y)},x=utpart,y=utmed,z=freq_part,SIMPLIFY = F)))
  
  
  return(part_ut) 
}


# function to split data in two

break2<- function(dataset,freq,func){
  
  # dataset is the set of elements of an attribute
  
  #fixes a line of cut, fixes all other attribute loss and measures the variation of utility of the attribute utility
  
  min<- max(func(dataset))**2  # allows all types of functions, not only increasing or decreasing ones
  
  n<- length(dataset) # number of elements
  
  partition<-vector(mode = "list",length = 2) # save partition
  freq_part<-vector(mode = "list",length = 2) # save frequencies
  
  
  ord<-order(dataset) # orders elements
  dataset<- dataset[ord]
  freq<- freq[ord]
  
  for(i in c(1:(n-1))){
    
    partition[[1]]<- append(partition[[1]],dataset[1:i])
    partition[[2]]<- append(partition[[2]],dataset[(i+1):(n)])
    
    part_id<- c(rep(1,length(dataset[1:i])),rep(2,length(dataset[(i+1):n]))) # says which elements belong to first or second set
    part_id[ord]<- part_id  # order points
    
    freq_part[[1]]<- append(freq_part[[1]],freq[1:i])
    freq_part[[2]]<- append(freq_part[[2]],freq[(i+1):(n)])
    
    
    part_ut<- ut_part(partition,freq_part,func)
    
    
    if(part_ut<min){
      partition_out<- part_id
      min<- part_ut
    }
    
    partition<-vector(mode = "list",length = 2) # clear partition
    freq_part<-vector(mode = "list",length = 2) # clear frequencies
    
    
  }
  
  return(partition_out)
  
}


CART_2D<- function(dataset,k,func1,func2){
  
  library(tictoc)
  
  tic(quiet = T)
  
  ncoll<- length(dataset[1,])-1  # number of attribute space
  nline<- length(dataset[,1]) # number of events
  
  if(k>nline | k<=1){
    
    print("Wrong k, please try again")
  }
  
  freq<- dataset[,ncoll+1] #frequencies
  
  dataset<- dataset[,-(ncoll+1)] # dataset without frequencies
  
  #utility functions
  ut1<-function(x){
    utbody<- eval(parse(text= func1))
    return(utbody)
  }
  
  ut2<-function(x){
    utbody<- eval(parse(text= func2))
    return(utbody)
  }
  
  gross<- 0  # save utility of each attribute of data, sum is utillity of data
  
  itr<-1
  while(itr<= ncoll){
    
    
    util<- eval(parse(text=sub(1,replacement = itr,x="ut1"))) # alternate utility function with iteraction
    
    #calculate utility of data/ gross
    
    
    a<-dataset[,itr] # dataset
    
    gross_ord<- order(a) # order of elements 
    a<- a[gross_ord] # order the dataset
    
    b<-freq[gross_ord] # order the frequencies
    
    c<- util(freq_median(a,b)) #median
    
    a<- util(a) # aply utility
    
    gross<- gross+ sum(unlist(mapply(function(x,y,z){z*abs(x-y)},x=a,y=c,z=b,SIMPLIFY = F)))
    itr<- itr + 1
  }
  
  #print("utility gross")
  #print(gross)
  
  #start CART here
  
  
  #k=2 situation
  
  loss<- 1 #start checking min
  
  #cycle through attribute space
  
  for(i in c(1:ncoll)){
    
    #cat("start cycle attribute: ",i,"\n")
    
    util<- eval(parse(text=sub(1,replacement = i,x="ut1"))) # alternate utility function with iteraction
    
    obj<- break2(dataset[,i],freq,util)
    
    
    #compute loss of partition with cut
    loss_tot<- 0
    
    for(inside in c(1:ncoll)){
      
      util<- eval(parse(text=sub(1,replacement = inside,x="ut1"))) # alternate utility function with iteraction
      
      part<-make_part(dataset[,inside],mat = obj,freq = freq)
      
      #print(final_part)
      
      loss_tot <- loss_tot + ut_part(part[[1]],part[[2]],func = util)
      
      #print(final_loss)
      
    }
    
    
    #relative loss comparation
    if(loss_tot/gross<loss){
      
      loss<- loss_tot/gross
      
      partition_id<- obj  # gives the distribution of the partition
      
      
    }
  }
  #print("end k=2")
  
  
  # for k= 3 or bigger
  
  div<- 2 # start division by 3 clusters
  
  
  
  while(div<k){
    
    
    # for each attribute
    
    loss<- 1 #start checking min
    for(i in c(1:ncoll)){
      
      #cat("start cycle attribute: ",i,"\n")
      
      partition_obj<- make_part(dataset[,i],unlist(partition_id),freq) # make partition and frequency partition
      
      util<- eval(parse(text=sub(1,replacement = i,x="ut1"))) # alternate utility function with iteraction
      
      
      
      # for each j inside the partition, which is the best set to divide
      for(j in c(1:div)){
        
        if(length(unlist(partition_obj[[1]][j]))>1){
          obj<-break2(unlist(partition_obj[[1]][j]),unlist(partition_obj[[2]][j]),util) # apply break2 to a set of the partition
          
          
          
          #create partition with division to evaluate
          
          
          count2<- 1  #runs the id vector
          new_id<- c() #save new partition id
          #cat("break set", set_div,"\n")
          for(i in unlist(partition_id)){
            
            if(i< j){
              
              new_id<- append(new_id,i)
            }
            
            if(i> j){
              
              new_id<- append(new_id,i+1)
            }
            
            if(i == j){
              new_id<- append(new_id,j+(obj[count2]-1))
              count2<- count2 + 1
            }
          }
          
          #calculate utility of partitiion with cut 
          loss_tot<- 0
          for(inside in c(1:ncoll)){
            
            util_in<- eval(parse(text=sub(1,replacement = inside,x="ut1"))) # alternate utility function with iteraction
            
            part<-make_part(dataset[,inside],mat = new_id,freq = freq)
            
            loss_tot <- loss_tot + ut_part(part[[1]],part[[2]],func = util_in)
            
            
          }
          
          #relative loss comparation, take best partition
          if(loss_tot/gross < loss){
            
            loss<- loss_tot/gross 
            
            best_id<- new_id  # gives the new distribution of the partition
            
            
            
          }
        }
        else{
          
          next
        }
      }
    }
    
    
    
    partition_id<- best_id
    
    div<- div+1
    
  }
  
  #print("final partition")
  
  final_loss<- 0  # sum of loss of the partition
  for(i in c(1:ncoll)){
    
    util<- eval(parse(text=sub(1,replacement = i,x="ut1"))) # alternate utility function with iteraction
    
    final_part<-make_part(dataset[,i],mat = unlist(partition_id),freq = freq)
    
    #print(final_part)
    
    final_loss<- final_loss + ut_part(final_part[[1]],final_part[[2]],func = util)
    
    #print(final_loss)
    
  }
  
  get_part_cart(dataset,freq,mat = unlist(partition_id),loss = final_loss/gross)
  
  time<- toc(quiet=TRUE)
  
  tm<-as.numeric(round(time$toc-time$tic,5))
  
  return(c(final_loss/gross,tm))
  
  
}









# function that transforms dataset in frequency cases and if order is used, replaces factors


data_freq_trans<- function(dataset,order,replace=F){
  
  #dataset<- data.frame(dataset,stringsAsFactors = F)
  
  n<- dim(dataset)[1]
  
  df1<- as.data.frame(table(dataset)/n,stringsAsFactors = F) 
  
  colnames(df1)<- c(colnames(dataset),"freq")
  
  if(length(which(df1[,3]== 0))!=0){
  
  df1<- df1[-which(df1[,3]== 0),]  # removes 0 freq values
  
  }
  
  if(replace == T){
  
  start_order<- 0 
  for(i in c(1:2)){
    
    n_order<- length(unique(df1[,i])) # number of elements of col i
    
    frac_order<- order[c((start_order+1):(n_order+start_order)),] 
    
    #print(frac_order)
    
    for(j in c(1:dim(frac_order)[1])){
    
    df1[,i][df1[,i]== frac_order[j,1]]<-  as.numeric(frac_order[j,2])
    
    }
    
    start_order<- n_order 
    
  }  
  
  df1[,1]<- as.numeric(df1[,1])
  df1[,2]<- as.numeric(df1[,2])
  
  
  }
  
  return(df1)
  
  
}





# u1 and u 2 utility values for factor values

return_func<- function(k,u){
  
  a<- u[k]
  return(a)
}



data_graphs_2D<- function(dataset,order,k1,k2=0,u1,u2,runtime=F){
  
#utility matrix that gives the numeric values according to the names
  
  
 
   
 df<- data_freq_trans(dataset,order,replace = T)
 
 return_func<- function(k,u){
  
  a<- u[k]
  return(a)
}

 u1<<- u1
 
 u2<<- u2
 
 func1<- "return_func(x,u=u1)"  # values of utility for atr1
  
 func2<- "return_func(x,u=u2)"  # values of utility for atr2
 
 #print(u1)
 #print(u2)
 
  
  k<- c(2:(k1+k2+1))
  
  
  loss_bf<- c()
  loss_cart<- c()
  
  time_bf<- c()
  time_cart<- c()
  
  npart<- c()
  
  
  
  par(mfrow=c(2,2))
  
  for(i in k){
    
    
    if(i<= (k1+1)){
      
      bf<- BF_2D(df,k = i,func1 = func1,func2 = func2)
      cart<- CART_2D(df,k = i,func1 = func1,func2 = func2)
      
      loss_bf<- append(loss_bf,bf[1])
      loss_cart<- append(loss_cart,cart[1])
      
      time_bf<- append(time_bf,bf[2])
      time_cart<- append(time_cart,cart[2])
      
      npart<- append(npart,bf[3])
      
      
    }
    
    else{
      
      
      cart<- CART_2D(dataset = df,k = i,func1 = func1,func2 = func2)
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
  
  if(runtime==T){
  
  time<- c(time_bf,time_cart)
  ylim_time<- max(time[!is.na(time)])
  
  plot(main="Runtime",x=k,y=(time_bf),type="l",col="blue",xlab="k",ylab = "Time /s",ylim = c(0,1.5*ylim_time))
  lines(x=k,y=(time_cart),col="red")
  legend("topleft",legend = c("Brute Force","CART","Total partitions","top: ratio time"),col=c("blue","red","black",NaN),lty=c(1,1,NaN,NaN),pch=c(NaN,NaN,19,NaN),cex=0.7)
  
    if(length(npart[!is.na(npart)])>0){
    par(new=T)
    plot(x=k,y=npart,axes=F,pch=19,ylab = NA,xlab = "",ylim = c(0,1.05*max(npart[!is.na(npart)])),xpd=T)
    axis(side = 4)
    mtext(side=4,text="Partition number",line=0)
    text(k,npart,labels = round(time_bf/time_cart,3),pos = 3,cex=0.7,xpd=T)
  }
  }
}



  
#############################################################################################

# use factores to rank from 1 to the number of factors

#test

  
#lev1<- c("small","medium","big")
#lev2<- c("C","B","A")

#val_mat<- data.frame(var=c(lev1,lev1),fact=c(1,2,1,2));val_mat

#val_mat

#set.seed(321)

#dataset<- data.frame(size= sample(lev1,size = 50,replace = T),grade=sample(lev1,size = 50,replace = T),stringsAsFactors = F)

#dataset


#u1<- c(15,17,20)
#u2<- c(10,15,19)

#data_graphs_2D(dataset = dataset,order = val_mat,k1 = 4,u1=u1,u2=u2)






################################################################################

