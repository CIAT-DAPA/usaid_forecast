
predict=read.table("C:/Users/dagudelo/Desktop/PLS_2/x.csv",header=T,dec=".",sep=",")
response=read.table("C:/Users/dagudelo/Desktop/PLS_2/y.csv",header=T,dec=".",sep=",")


pls_2<-function(X,Y,H){
  
  n<-nrow(X)
  p<-ncol(X)
  q<-ncol(Y)
 
  X0<-scale(X)#*(sqrt(n)/sqrt(n-1))
  Y0<-scale(Y)#*(sqrt(n)/sqrt(n-1))

  
  W<-matrix(NA,p,H)
  T<-matrix(NA,n,H)
  C<-matrix(NA,q,H)
  U<-matrix(NA,n,H)
  P<-matrix(NA,p,H)
  for(h in 1:H) 
  {
    uh<-as.matrix(Y0[,1])
    
    for(i in 1:50)
    {  
      
      wh<-(t(X0)%*%uh)/as.numeric(t(uh)%*%uh) #sqrt(sum(uh^2))
      nwh<-wh/sqrt(sum(wh^2))
      th<-(X0%*%nwh)/as.numeric(t(nwh)%*%nwh)
      ch<-(t(Y0)%*%th)/as.numeric(t(th)%*%th)
      uh<-(Y0%*%ch)/as.numeric(t(ch)%*%ch)
      
    }
    ph<-(t(X0)%*%th)/as.numeric(t(th)%*%th)
    
    W[,h]<-nwh
    T[,h]<-th
    C[,h]<-ch
    U[,h]<-uh
    P[,h]<-ph
    
    
    X1<-X0-(th%*%t(ph))
    Y1<-Y0-(th%*%t(ch))
    
    X0<-X1
    Y0<-Y1
  } 
 
  Resul<-list(W,T,C,U,P)
  return(Resul) 
  
}

X=pca_x
Y=pca_y

VC=function(X,Y,H){

output=list()
est=matrix(NA,dim(Y)[1],dim(Y)[2])
n <- nrow(X)
p<-ncol(X)

  for(h in 1:H){
  
  
    for(i in 1:n)
    {
      Y_i <-Y[-i,]; X_i <-X[-i,]
      mYi <-colMeans(Y_i); sYi <-sqrt(diag(var(Y_i)))
      mXi <-colMeans(X_i); sXi <-sqrt(diag(var(X_i)))
      Xi <- as.matrix(X[i,,drop=FALSE]); Xiz <- as.matrix((Xi-mXi)/sXi)
      Yi <- Y[i,]
      rgPLS2_i <-pls_2(X_i,Y_i,h)
      Wh_i <- rgPLS2_i[[1]]; Th_i <- rgPLS2_i[[2]]
      Ch_i <- rgPLS2_i[[3]]; Ph_i <- rgPLS2_i[[5]]
      W.h_i <- Wh_i%*%solve(t(Ph_i)%*%Wh_i)
      thi <- Xiz%*%W.h_i
      Yvc <- thi%*%t(Ch_i); Yivc <- mYi+sYi*Yvc
      est[i,]<-Yivc
      
    } 
  
    output[[h]]=est
    
  }
return(output)
}



validation=VC(datos_x,datos_y,5)


for(k in 2:15){

   for(l in 2:15){
     
      pca_x=nipals(datos_x,3)[[1]]
      pca_y=nipals(datos_y,2)[[1]]
      
      validation=VC(pca_x,pca_y,3)
      
      
      lod_y=nipals(datos_y,2)[[2]]
      
      pls_stimated=lapply(validation,function(x) x%*%t(as.matrix(lod_y)))
      
      means=colMeans(datos_y)
      sd=apply(datos_y,2,sd)
      
      mean_matrix=matrix(means,dim(datos_y)[1],dim(datos_y)[2],byrow = T)
      
      pls_final=lapply(pls_stimated,function(x) (x%*%diag(sd))+mean_matrix)
      
      
      out=unlist(lapply(pls_final,function(x) mean(diag(cor(x,datos_y)))))

      print(c(k,l,which(out==max(out)),max(out)))
      
   }

}
lapply(validation,function(x) mean(diag(cor(x,datos_y))))

e=12
plot(validation[[2]][,e],type="o",col="red",ylim=c(min(validation[[16]][,e])-50,max(validation[[16]][,e])+80),pch=8,lwd=2)
lines(datos_y[,e],type="o",pch=8,lwd=2)
cor(validation[[2]][,e],datos_y[,e])
