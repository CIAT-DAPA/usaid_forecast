y=read.table("clipboard",dec=".",header=T)
x1=read.table("C:/Users/dagudelo/Desktop/verificacion_PCA/data_x1.csv",dec=".",header=T,sep = ",")


y_pca=nipals(y,5)


x_pca=nipals(x1,5)[[1]]


u=read.table("clipboard",dec=".")

plot(x_pca[,5],type="o",col="red")
lines(-u[,1],type="o",col="blue")


##########
data_x=read.table("clipboard",dec=".")

compx=read.table("C:/Users/dagudelo/Desktop/ejemplo_final/comp_x.txt",dec=".",header=T,skip=2)
compy=read.table("C:/Users/dagudelo/Desktop/ejemplo_final/comp_y.txt",dec=".",header=T,skip=2)

canonico=cancor(compx[,1:3],compy[,1:5])
x_center=scale(compx[,1:3],scale=F)
y_center=scale(compy[,1:5],scale=F)  
com_x=x_center%*%canonico$xcoef
com_y=y_center%*%canonico$ycoef
mode1=cbind(com_x[,1],com_y[,1])

pred=read.table("clipboard",dec=".")

xiz=as.matrix(pred-canonico$xcenter)
vec_x=canonico$xcoef
xiz_com=as.matrix(xiz)%*%vec_x[,1,drop=FALSE]
y_est=(R[1]*xiz_com)+coef[1]
y_last[i,]=(y_est%*%solve(canonico$ycoef)[,1])+ canonico$ycenter[1]

plot(mode1[,2],type="o",col="red",lwd=2)
lines(mode1[,1],type="o",col="blue",lwd=2)










cor(mode1)
mod=lm(mode1[,2]~mode1[,1])


X=compx[,1:3,drop=FALSE]
Y=compy[,1:5,drop=FALSE]
n=dim(X)[1]
y_last=matrix(,n,dim(Y)[2])

for(i in 1:n){

  Y_i<-Y[-i,]; X_i<-X[-i,]
  canonico=cancor(X_i,Y_i)
  x_center=scale(X_i,scale=F)
  y_center=scale(Y_i,scale=F)  
  com_x=x_center%*%canonico$xcoef
  com_y=y_center%*%canonico$ycoef
  R=canonico$cor
  xiz=as.matrix(X[i,]-canonico$xcenter)
  vec_x=canonico$xcoef
  xiz_com=as.matrix(xiz)%*%vec_x[,1,drop=FALSE]
  
  coef=coefficients(lm(com_y[,1]~com_x[,1]))
  y_est=(R[1]*xiz_com)+coef[1]
  y_last[i,]=(y_est%*%solve(canonico$ycoef)[,1])+ canonico$ycenter[1]
  
  
}




plot(Y[,1],type="o")
lines(y_last[,1],type="o",col="red")



loadin=read.table("clipboard",dec=".")

total=y_last%*%t(as.matrix(loadin))



data_y=read.table("clipboard",dec=".",header=TRUE)

means=colMeans(data_x)
sd=apply(data_x,2,sd)


mean_matrix=matrix(means,32,27,byrow = T)

f=total%*%diag(sd)
final=f+mean_matrix


k=21

plot(data_x[,k],type="o",col="red",lwd=2)
lines(final[,k],type="o",col="green",lwd=2)

cor(data_x[,k],final[,k])


cross=read.table("clipboard",dec=".",header=TRUE)

k=1

plot(cross[,k],type="o",col="red",lwd=2)
lines(final[,k],type="o",col="green",lwd=2)

cor(cross[,k],final[,k])

plot(cross[,k],final[,k])



sd(cross[,1]-data_x[-32,1])


plot(data_x[,1],type="o",col="red",lwd=2)
lines(cross[,1],type="o",col="green",lwd=2)

