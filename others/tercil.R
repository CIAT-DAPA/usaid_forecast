
#########################
#Cuantiles climatológicos
#########################
data_all = read.table("clipboard",header = T)

t1=aggregate(data_all[,-2:-1],list(Mes=data_all$month),quantile,probs=0.3333333,na.rm=T)
t2=aggregate(data_all[,-2:-1],list(Mes=data_all$month),quantile,probs=0.666667,na.rm=T)

write.csv(rbind(t1,t2),"tercil_prec.csv")

