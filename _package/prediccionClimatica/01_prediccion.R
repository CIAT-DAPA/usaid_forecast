####### Functions #########
###########################


#####Input download.cpt#####
##(dir_save) Ruta para guardar los archivos.
##(month) Mes antes del inicio de las predicciones.
##(year) A�o en el cual se generan las predicciones.
#####Outoput download.cpt#####
## Archivos de la TSM de los siguientes 6 meses
## guardados en la ruta suministrada en el input.

download.cpt=function(dir_save,month,year){ 
  
  w=(month)+(0:7)
  if(sum(w>12)>0)w[which(w>12)]=w[which(w>12)]-12
  if(sum(w<1)>0)w[which(w<1)]=w[which(w<1)]+12
  
  for(i in 1:6){
    
    l=month-1
    if(l<=0)l=l+12
    ensemble="M/1/24/RANGE"
    if(l==9 & (i==1|i==2)){
      ensemble="M/(1%202%203%204%205%206%207%208%209%2012%2013%2014%2015%2016%2017%2018%2019%2020%2021%2022%2023%2024)/VALUES/"
    }
    if(l==8 & i==1){
      ensemble="M/(1%202%203%204%205%206%207%208%209%2010%2011%2012%2014%2015%2016%2017%2018%2019%2020%2021%2022%2023%2024)/VALUES/"
    }
    if(l==1 & (i==4|i==5|i==6)){
      ensemble="M/(1%202%203%204%205%206%207%208%209%2010%2011%2012%2013%2014%2016%2017%2018%2019%2020%2021%2022%2023%2024)/VALUES/"
    } 
    
    route=paste("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/S/%280000%201%20",month.abb[l],"%201982-",year,"%29VALUES/L/",i,".5/",i+2,".5/RANGE%5BL%5D//keepgrids/average/",ensemble,"%5BM%5Daverage/-999/setmissing_value/Y/(30N)/(30S)/RANGEEDGES/%5BX/Y%5D%5BS/L/add%5Dcptv10.tsv.gz",sep="")### Ruta de descarga de datos 
    
    filePath <- paste(dir_save,"/",i,"_",paste(month.abb[w[i:(i+2)]], collapse = '_'),"-",Sys.Date(),".tsv.gz",sep="")

    # ----------------------------------------------------------------------------------------------------------------------
    #  > prueba que el archivo gzip no est� da�ado, sino intenta descargarlo de nuevo
    # ----------------------------------------------------------------------------------------------------------------------
    gunzipTestVal = 1
    
    while (gunzipTestVal == 1) {
    
      download.file(route, filePath) ### Realiza la descarga 
      
      gunzipTestStr <- try(system(paste("gunzip -t ", filePath, sep = "", collapse = NULL), intern = TRUE, ignore.stderr = FALSE))
      cat (gunzipTestStr)

      gunzipTestVal = length(grep('invalid compressed data--crc', gunzipTestStr))

      if (gunzipTestVal == 1){
        cat ('... archivo gzip corrupto, se reitentara descargar\n\n\n\n')
      }else {
        cat ('... archivo descargado correctamente\n\n\n\n')
      }
    }
    # ----------------------------------------------------------------------------------------------------------------------    
    
    
  }
  
}

#####Input data_table#####
##### (dates) Datos cargados en R en formato data frame con 
##### read.table(x,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999))
#####Output Data_table#####
## (all_output) Una lista [1]los datos de la tsm donde en las filas 
## estan los a�os y en las columnas los pixeles 
## [2] los a�os disponibles de la informacion 

data_table=function(dates){
  
  year_month=dates[1,][!is.na(dates[1,])]
  if(substr(year_month[2],6,7)=="12"){year=as.numeric(substr(year_month[-1],1,4))+1
  }else{year=as.numeric(substr(year_month[-1],1,4))}
  data_cpt1=na.omit(dates)
  pos=which(data_cpt1[,1]=="")
  pos=sort(rep(year,pos[2]-pos[1]))
  list_dates=split(data_cpt1,pos)
  tables=lapply(list_dates,"[",-1,-1)
  y=lapply(tables,function(x)as.numeric(as.vector(t(x))))
  data_fin=do.call(rbind,y) 
  colnames(data_fin)=1:dim(data_fin)[2]
  data_fin=data_fin[,colSums(data_fin==-999)==0]
  all_output=list(data_fin,year)
  return(all_output)
  
}

#####Input quarterly_data#####
## (data) Datos de las estaciones en el formato definido.
## (y) Mes antes del inicio de las predicciones.
#####Output quartely_data#####
## (all_output) Una lista [[1]] los datos de las estaciones 
## en forma trimestral [[2]] A�os disponibles de la informaci�n.

quarterly_data=function(data,sy_month){
  
  names_months=0
  data_out=list()
  year_out=list()
  l=(sy_month)+(0:7)
  if(sum(l>12)>0)l[which(l>12)]=l[which(l>12)]-12
  if(sum(l<1)>0)l[which(l<1)]=l[which(l<1)]+12
  for(i in 1:6){
    pos_ini=which(data$month==l[i])
    pos_data=sort(c(pos_ini,pos_ini+1,pos_ini+2))
    data_out[[i]]=data.frame(na.omit(aggregate(data[pos_data,-1:-2],by=list(sort(rep(1:(length(pos_ini)),3))),sum))[,-1])
    names(data_out[[i]])=colnames(data)[-1:-2]
    year_out[[i]]=data[pos_ini+1,1][1:dim(data_out[[i]])[1]]
    names_months[i]=paste(month.abb[l[(0:2)+i]],collapse="_")
    rownames(data_out[[i]])=year_out[[i]]
  }
  
  names(data_out)=names_months
  names(year_out)=names_months
  
  all_output=list(data_out,year_out)
  names(all_output)=c("data_stations","year_response")
  
  return(all_output)
} 

#####Input nipals#####
## (X) Matrix de datos para calcular el PCA.
## (modos) Numero de componentes a calcular.
#####Output nipals#####
## (resul) Una lista donde el [[1]] Componentes principales, [[2]] Vectores propios 
## [[3]] Valores propios

nipals<-function(X,modos){
  
  n<-nrow(X)
  p<-ncol(X)
  X0<-scale(X)*(sqrt(n)/sqrt(n-1))
  T<-matrix(NA,n,modos)#Componentes principales
  P<-matrix(NA,p,modos)#Vectores Propios
  valor<-0
  for(h in 1:modos) 
  {
    th<-as.matrix(X0[,1])
    
    for(i in 1:40)
    {  
      ph<-(t(X0)%*%th)/as.numeric(t(th)%*%th)
      nph<-ph/sqrt(sum(ph^2))#Vectores Normalizados
      ph<-nph
      th<-X0%*%ph
    } 
    valor[h]<-t(th)%*%th/(dim(X0)[1])
    T[,h]<-th
    P[,h]<-ph
    
    X1<-X0-th%*%t(ph)
    X0<-X1
  } 
  rownames(T)<-rownames(X) 
  Resul<-list(T,P,valor)
  return(Resul) 
  
}

#####Input selection_area#####
## (x) datos de la tsm en filas a�os y columnas pixeles.
## (y) datos de las estaciones en el formato predefinido.
#####Output selection_area#####
## (data_x_selec) datos de la tsm seleccionados para el 
## modelo CCA, en las filas a�os y en las columnas pixeles.

selection_area=function(x,y){
  
  if(dim(y)[2]<10){
    k=dim(y)[2]
    y_pca=nipals(y,modos=k)[[1]]
  }else{
    k=10
    y_pca=nipals(y,modos=10)[[1]]
  }
  
  x_pca=nipals(x,modos=10)[[1]]
  all_cor=matrix(NA,k*10,dim(x)[2])
  count=0
  
  for(i in 1:10){
    
    for(j in 1:k){
      
      canonico=cancor(x_pca[,1:i],y_pca[,1:j,drop=F])
      x_center=scale(x_pca[,1:i],scale = F)
      y_center=scale(y_pca[,1:j],scale = F)  
      com_x=x_center%*%canonico$xcoef
      com_y=y_center%*%canonico$ycoef
      mode1=cbind(com_x[,1],com_y[,1])
      cor_tsm=cor(x,mode1[,1])
      count=count+1
      all_cor[count,]=cor_tsm[,1]
     print(c(i,j))
    }
    
  }
  
  cor_mean=apply(abs(all_cor),2,mean)
  percen=quantile(cor_mean,0.70)
  data_x_selec=x[,cor_mean>percen]
  
  return(data_x_selec)
}

#####Input cross_val#####
## (x) datos de la tsm seleccionado para correr en el modelo
## (Y) datos de las estaciones en el formato predefinido.
#####Otuput cross_val#####
## (all_out) Una lista donde el [[1]] es el resultado de la validacion.
## cruzada con el modelo que arrojo los mejores resultados [[2]] Modos de x.
## ,modos de y, goodness index del mejor modelo.

cross_val=function(x,y){
  
  if(dim(y)[2]<15){
    k=dim(y)[2]
    y_pca=nipals(y,modos=k)
  }else{
    k=15
    y_pca=nipals(y,modos=15)
  }
  
  x_pca=nipals(x,modos=15)
  output=matrix(NA,k*15,3)
  all_stimation=list()
  count=0
  
  for(i in 1:15){
    
    for(j in 1:k){
      
      X=x_pca[[1]][,1:i,drop=FALSE]
      Y=y_pca[[1]][,1:j,drop=FALSE]
      n=dim(Y)[1]
      y_last=matrix(NA,n,dim(Y)[2])
      y_est=0
      
      for(p in 1:n){
        
        Y_i<-Y[-p,]; X_i<-X[-p,]
        canonico=cancor(X_i,Y_i)
        x_center=scale(X_i,scale=F)
        y_center=scale(Y_i,scale=F)  
        com_x=x_center%*%canonico$xcoef
        com_y=y_center%*%canonico$ycoef
        R=canonico$cor
        xiz=as.matrix(X[p,]-canonico$xcenter)
        vec_x=canonico$xcoef
        xiz_com=t(as.matrix(xiz))%*%vec_x[,1,drop=FALSE]
        
        
        y_est[p]=(R[1]*xiz_com)
        y_last[p,]=(y_est[p]*solve(canonico$ycoef)[1,,drop=FALSE]) + canonico$ycenter
        
        
      }
      
      lod_y=y_pca[[2]][,1:j,drop=FALSE]
      data_y_scaled=y_last%*%t(as.matrix(lod_y))
      means=colMeans(y)
      means_matrix=matrix(means,dim(y)[1],dim(y)[2],byrow = T)
      sds=apply(y,2,sd)
      data_y_stimated=(data_y_scaled%*%diag(sds))+means_matrix
      data_y_stimated[data_y_stimated<0]=0
      
      count=count+1
      
      all_stimation[[count]]=data_y_stimated
      colnames(all_stimation[[count]])=colnames(y)
      output[count,1]=i
      output[count,2]=j
      output[count,3]=mean(diag(cor(data_y_stimated,y,method = "kendall")))
      
      
    }
    
    
  }  
  
  pos=which(output[,3]==max(output[,3]))
  all_out=list(all_stimation[[pos]],output[pos,])
  return(all_out)
  
}


forecast=function(x,y,x_fores,set){
  
  mean_x=colMeans(x)
  sd_x=apply(x,2,sd)
  mean_y=colMeans(y)
  sd_y=apply(y,2,sd)
  x_fores_scale=(x_fores-mean_x)/sd_x
  pca_x=nipals(x,set[1])
  pca_y=nipals(y,set[2])
  x_fores_comp= x_fores_scale%*%pca_x[[2]]
  
  
  canonico=cancor(pca_x[[1]],pca_y[[1]])
  x_center=scale(pca_x[[1]],scale=F)
  y_center=scale(pca_y[[1]],scale=F)  
  com_x=x_center%*%canonico$xcoef
  com_y=y_center%*%canonico$ycoef
  R=canonico$cor
  xi=as.matrix(x_fores_comp-canonico$xcenter)
  vec_x=canonico$xcoef
  xi_com=as.matrix(xi)%*%vec_x[,1,drop=FALSE]
  y_fores=(R[1]*xi_com)
  y_fores_last=(y_fores%*%solve(canonico$ycoef)[1,])+canonico$ycenter
  
  
  y_fores_comp=y_fores_last%*%t(as.matrix(pca_y[[2]]))
  y_fores_final=(y_fores_comp*sd_y)+mean_y
  
  all_out=list(y_fores_final,xi_com)
  
  return(all_out)
  
}


probabilities=function(fores,Y,sd_s){
  
  terciles=apply(Y,2,function(x)quantile(x,c(1/3,2/3),type=6))
  below=(pnorm(terciles[1,],fores,sd_s))*100
  normal=(pnorm(terciles[2,],fores,sd_s)-pnorm(terciles[1,],fores,sd_s))*100
  above=(1-pnorm(terciles[2,],fores,sd_s))*100
  id=substr(names(Y),2,nchar(names(Y)))
  prob_output=cbind.data.frame(id,below,normal,above)
  
  return(prob_output)
}


prob_output=function(p,m,y){
  
  w=(m)+(1:6)
  years=rep(as.numeric(y),6)
  if(sum(w>12)>0)years[which(w>12)]=years[which(w>12)]+1
  if(sum(w>12)>0)w[which(w>12)]=w[which(w>12)]-12
  if(sum(w<1)>0)w[which(w<1)]=w[which(w<1)]+12
  table=data.frame(year=rep(years,p),month=rep(w,p))
  table_order=table[order(table$year,table$month),]
  return(table_order)
  
}

#########RUN#########
#####################

# start.time <- Sys.time()
# dir_save="C:/Users/dagudelo/Desktop/Ejemplo_descarga"
month=as.numeric(format(Sys.Date(),"%m"))
year=format(Sys.Date(),"%Y")
y=download.cpt(dir_save,month,year)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken


files_gz=list.files(dir_save,pattern=as.character(Sys.Date()))
dir_files=paste(dir_save,files_gz,sep="/")
p=lapply(dir_files,function(x)gzfile(x,'rt'))
y_k=lapply(dir_files,function(x)read.table(x,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999))


data_x=lapply(y_k,data_table)
data_tsm=unlist(lapply(data_x,"[", 1),recursive=FALSE)
names(data_tsm)=substr(files_gz,3,13)
year_predictor=unlist(lapply(data_x,"[", 2),recursive=FALSE)
names(year_predictor)=names(data_tsm)
data_tsm_fore=lapply(data_tsm,function(x) x[dim(x)[1],,drop=F])
names(data_tsm_fore)=names(data_tsm)


# dir_response="C:/Users/dagudelo/Desktop/Estaciones_1"
dir_res=paste(dir_response,list.files(dir_response),sep="/")
data_y=lapply(dir_res,function(x)read.table(x,dec=".",sep = ",",header = T))
names(data_y)=basename(dir_res)


# dir_stations="Y:/USAID_Project/Product_1_web_interface/test/clima/daily_data"
stations_selec=substr(list.files(dir_stations),1,nchar(list.files(dir_stations))-4)


data_quar=lapply(data_y,quarterly_data,month)
data_quartely=unlist(lapply(data_quar,"[", 1),recursive=FALSE)
year_response=unlist(lapply(data_quar,"[", 2),recursive=FALSE)

########## seleccion area ###########
####################################

year_model=lapply(year_response,function(x) Map(function(x1,y1) years_model=intersect(x1,y1),year_predictor, x))
years_final_res=Map(function(x,y) Map(function(x1,y1) pos_x=x1%in%y1 ,x,y),year_response,year_model)
years_final_prec=lapply(year_model,function(x) Map(function(x1,y1) pos_x=x1%in%y1 ,year_predictor, x))

data_tsm_final=lapply(years_final_prec,function(x) Map(function(x1,y1) x1[y1,] , data_tsm , x))
data_res_final=Map(function(x,y) Map(function(x1,y1) x1[y1,] ,x,y),data_quartely,years_final_res)

data_tsm_selec=Map(function(x,y) Map(selection_area,x,y),data_tsm_final,data_res_final)

########## Cross Validation ##########
######################################

cross_obj=Map(function(x,y) Map(cross_val,x,y),data_tsm_selec,data_res_final)

######### deterministic forecast ###########
############################################

fores_tsm_selec=lapply(data_tsm_selec,function(x) Map(function(x1,y1) y1[,match(colnames(x1),colnames(y1)),drop=F],x,data_tsm_fore ))
settings=lapply(cross_obj,function(x) lapply(x,"[[",2))
cross_all=lapply(cross_obj,function(x) lapply(x,"[[",1))
forescast_obj=Map(function(x,y,x_fores,set)Map(forecast,x,y,x_fores,set),data_tsm_selec,data_res_final,fores_tsm_selec,settings)
forescast_all=lapply(forescast_obj,function(x) lapply(x,"[[",1))
Value_modo_x=lapply(forescast_obj,function(x) lapply(x,"[[",2))

######### Probabilistic forecast ###########
############################################

sd_cross=Map(function(x,y) Map(function(x1, y1) sqrt(colSums(((x1-y1)^2)/dim(x1)[1]-1-1)),x,y),cross_all,data_res_final)
n_all=lapply(data_res_final,function(x)lapply(x,function(x)dim(x)[1]))
sd_final=Map(function(sd,value_modo,n)Map(function(sd1,value_modo1,n1) sd1*sqrt(1+(1/n1)+(value_modo1)^2) ,sd,value_modo,n),sd_cross,Value_modo_x,n_all)
probabilities_final=Map(function(fores,Y,sd_s) Map(probabilities,fores,Y,sd_s),forescast_all,data_res_final,sd_final)
probabilities_join=lapply(probabilities_final,function(x) do.call(rbind,x))

######### Probability table ################
############################################

p_all=lapply(data_y,function(x)dim(x)[2]-2)
table_year_month=lapply(p_all,prob_output,month,year)
prob_output_list=Map(function(x,y)cbind(x,y),table_year_month,probabilities_join)
prob_output_final=do.call(rbind,prob_output_list)

#path_prob="Y:/USAID_Project/Product_1_web_interface/test/clima/prob_forecast"
#path_prob="C:/Users/dagudelo/Desktop"

write.csv(prob_output_final,paste0(path_prob,"/",format(Sys.Date(),"%Y%m%d"),"_prob.csv"),row.names = F)

######### Metrics table ###############
#######################################

#pearson_cor=Map(function(x,y) Map(function(x1, y1){pearson=diag(cor(x1,y1));id=substr(names(x1),2,nchar(names(x1)));data.frame(id,pearson)},x,y),data_res_final,cross_all)
#kendall_cor=Map(function(x,y) Map(function(x1, y1){kendall=diag(cor(x1,y1,method = "kendall"));data.frame(kendall)},x,y),data_res_final,cross_all)
#pearson_join=lapply(pearson_cor,function(x) do.call(rbind,x))
#kendall_join=lapply(kendall_cor,function(x) do.call(rbind,x))


#lapply(settings,function(x) lapply(x,function(x1)x1[3]) )

#u=Map(function(x,y,z) cbind(x,y,z),table_year_month,pearson_join,kendall_join)



