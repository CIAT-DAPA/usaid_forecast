
library(lubridate)
library(reshape)
#require(ggplot2)
library(stringr)



#---------------------------------------------------------------------------------#
#-------------------Funci�n para generar barra de progreso\-----------------------#
#---------------------------------------------------------------------------------#
sapply_pb <- function(X, FUN, ...) {
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir=env)
    setTxtProgressBar(get("pb", envir=env), curVal +1)
    FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}


#---------------------------------------------------------------------------------#
#----------------Funci�n para generar a�os m�s frecuentes-------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# data: Datos acumulados mensuales para la precipitaci�n del mes a pronosticar
# organizados de forma ascendente
# a�oshistorico: Tabla de a�os ordenados de forma ascendente de acuerdo a la precipitaci�n
# acumulada del mes de inter�s

# OUTPUT
# Remuestreo de los a�os mas frecuentes del mes de inter�s de acuerdo a las probabilidades ingresadas

resampling <- function(data,prob,a�oshistorico){
  matrizcombinaciones=0
  vectorprobabilidades=prob
  datas=0
  for(i in 1:100){
    r=sample(prob,1,prob=prob)
    if(r==vectorprobabilidades[1]){
      datas=which(data<quantile(data[which(data>0)],0.3333))
      
      matrizcombinaciones[i]=a�oshistorico[sample(datas,1)]
    }
    
    if(r==vectorprobabilidades[2]){
      datas=which(data>=quantile(data,0.3333) & data<quantile(data,0.6666))
      matrizcombinaciones[i]=a�oshistorico[sample(datas,1)]
      
    }
    
    if(r==vectorprobabilidades[3]){
      datas=which(data>=quantile(data,0.6666))
      matrizcombinaciones[i]=a�oshistorico[sample(datas,1)]
      
    }
    
    
  }
  return(matrizcombinaciones)
  
  
}



#---------------------------------------------------------------------------------#
#-----------------Funci�n para generar escenarios diarios-------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# prob: Tabla de probabilidades de la estaci�n de inter�s para los siguientes 6 meses
# data_d: Tabla con datos diarios de la estaci�n de inter�s
# path_output: Ruta donde se guardar�n las salidas
# station: Nombre de la estaci�n de inter�s

#OUTPUT
# Al correr esta funci�n se generar�n los 100 escenarios (en formato .csv) de datos diarios 
# para la estaci�nde inter�s

gen_esc_daily <- function(prob,data_d,path_output,station){
  
   
  #---------------------------------------------------------------------------------#
  #-------------------------------Lectura de datos----------------------------------#
  #---------------------------------------------------------------------------------#
  
  data_d=read.csv(data_d,header=T,dec=".")
   
  
  #attach(data_d,warn.conflicts =F)
  
  data=aggregate(as.numeric(data_d$prec),list(data_d$year,data_d$month),sum)
  names(data)=c("year","month","value")
  
  probabilidades=as.data.frame(data_prob[1:6,])
  month.prob = month.name[probabilidades$month] 
  
  #---------------------------------------------------------------------------------#
  #-----------------Ordenar de menor a Mayr datos mensuales hist�ricos-------------#
  #---------------------------------------------------------------------------------#
  prec_sort = matrix(NA,nrow(data),6)
  year_sort = matrix(NA,nrow(data),6)
  
  for (i in probabilidades$month){
    prec_month = data[data$month==i,3]
    year_month = data[data$month==i,1]
    
    prec_sort[,i] = prec_month[order(prec_month)]
    year_sort[,i] = year_month[order(prec_month)]
    
    
  }
  
  colnames(prec_sort)=month.prob
  colnames(year_sort)=month.prob
  
  
  #   A�os_org2=A�os_org[,match(names(probabilidades),colnames(A�os_org))]
  #   var_org2=var_org[,match(names(probabilidades),colnames(var_org))]
  #   
  #---------------------------------------------------------------------------------#
  #---------------------------C�lculo de percentiles--------------------------------#
  #---------------------------------------------------------------------------------#
  
#   percentiles=rbind(apply(prec_sort,2,FUN=quantile,0.3333),apply(prec_sort,2,FUN=quantile,0.6666),apply(prec_sort,2,FUN=quantile,0.9999))
#   rownames(percentiles)=c("T1","T2","T3")
#   colnames(percentiles)=month.prob
#   
  
  
 
  
  
  #---------------------------------------------------------------------------------#
  #--------------Generaci�n de los 12 a�os an�logos mas probables-------------------#
  #---------------------------------------------------------------------------------#
  
  masprobable=matrix(0,nrow=100,ncol=dim(probabilidades)[2])
  
  print("Generando a�os m�s frecuentes...")
  masprobable=sapply_pb(1:100,
                        function(j){
                          esc1=sapply(1:dim(probabilidades)[2], function(i) insumo(prec_sort[,i],probabilidades[i,4:6],year_sort[,i]))
                          masprobable[j,]=sapply(1:dim(probabilidades)[2],function(j) as.numeric(rownames(cbind(which(table(esc1[,j])==max(table(esc1[,j]))))))[1])
                        })    
  
  masprobable2=apply(t(masprobable),2,function(x) as.numeric(names(sort(table(x),T))[1:10]))
  
  #---------------------------------------------------------------------------------#
  #-------------Generaci�n de datos y resumen de los a�os mas probables-------------#
  #---------------------------------------------------------------------------------#
  #   
  #   valores=function(mes,var,A�os){
  #     datos=0
  #     for(i in 1:length(mes))
  #       datos[i]=var[which(mes[i]==A�os)]
  #     return(datos)
  #   }
  #   
  #   todo=sapply(1:dim(probabilidades)[2], function(i) valores(masprobable2[,i],var_org2[,i],A�os_org2[,i]))
  #   todo2=as.data.frame(rbind(masprobable2,c("Datos an�logos",rep("",dim(probabilidades)[2]-1)),todo)) ###A�os y datos analogos
  #   colnames(todo2)=names(probabilidades)
  #   
  #   resumen=function(x) rbind(min(x),max(x))
  #   resumen2=apply(todo,2,resumen)
  #   
  #   if(svalue(val_p)=="precip") {medias=apply(todo,2,median)}else{medias=apply(todo,2,mean)}
  #   
  #   dif=t(t(todo)-medias)
  #   
  #   valores2=function(masprobable2,todo,resumen2,dif){
  #     datos=0
  #     for(i in 1:2){
  #       pos<-which(todo==resumen2[i])
  #       n=length(pos)
  #       datos[i]=masprobable2[pos[sample(n,1)]]
  #     }
  #     
  #     pos2=which(abs(dif)==min(abs(dif)))
  #     n2=length(pos2)
  #     datos2=masprobable2[pos2[sample(n2,1)]]
  #     
  #     datost=c(datos[1],datos2,datos[2])
  #     
  #     return(datost)
  #   }
  #   
  #   todo3=sapply(1:dim(probabilidades)[2], function(i) valores2(masprobable2[,i],todo[,i],resumen2[,i],dif[,i]))
  #   
  #   resumen3=rbind(resumen2[1,],round(medias,2),resumen2[2,])
  #   row.names(resumen3)=c("M�n","Promedio","M�x")
  #   
  #   resumenf=rbind(resumen3,c("A�os",rep("",dim(probabilidades)[2]-1)),todo3) ###Resumen con min max y prom de los escenarios analogos
  #   colnames(resumenf)=names(probabilidades)
  #   
  #   ###Salida de a�os analogos y resumen
  #   write.csv(todo2,file="Pronosticos/a�osanalogos.csv")
  #   write.csv(resumenf,file="Pronosticos/resumen_a�osanalogos.csv")
  #   
  #---------------------------------------------------------------------------------#
  #----------Generaci�n de todos los escenarios definidos por el usuario------------#
  #---------------------------------------------------------------------------------#
  num_esc1=100
  
  a=masprobable2
  a=as.data.frame(a)
  names(a)=month.prob
  
  
  if(any(names(a)=="January")){ escenario_Ene=list()}
  if(any(names(a)=="February")){ escenario_Feb=list()}
  if(any(names(a)=="March")){ escenario_Mar=list()}
  if(any(names(a)=="April")){ escenario_Abr=list()}
  if(any(names(a)=="May")){ escenario_May=list()}
  if(any(names(a)=="June")){ escenario_Jun=list()}
  if(any(names(a)=="July")){ escenario_Jul=list()}
  if(any(names(a)=="August")){ escenario_Ago=list()}
  if(any(names(a)=="September")){ escenario_Sep=list()}
  if(any(names(a)=="October")){ escenario_Oct=list()}
  if(any(names(a)=="November")){ escenario_Nov=list()}
  if(any(names(a)=="December")){ escenario_Dic=list()}
  
  esc_consolidado=list()
  
  
  for(w in 1:num_esc1){
    esc_consolidado[[w]]=cbind(
      if(any(names(a)=="January")){ escenario_Ene[[w]]=sample(a$January,1)},
      if(any(names(a)=="February")){ escenario_Feb[[w]]=sample(a$February,1)},
      if(any(names(a)=="March")){ escenario_Mar[[w]]=sample(a$March,1)},
      
      if(any(names(a)=="April")){ escenario_Abr[[w]]=sample(a$April,1)},
      if(any(names(a)=="May")){escenario_May[[w]]=sample(a$May,1)},
      if(any(names(a)=="June")){escenario_Jun[[w]]=sample(a$June,1)},
      if(any(names(a)=="July")){escenario_Jul[[w]]=sample(a$July,1)},
      if(any(names(a)=="August")){ escenario_Ago[[w]]=sample(a$August,1)},
      if(any(names(a)=="September")){escenario_Sep[[w]]=sample(a$September,1)},
      if(any(names(a)=="October")){escenario_Oct[[w]]=sample(a$October,1)},
      if(any(names(a)=="November")){escenario_Nov[[w]]=sample(a$November,1)},
      if(any(names(a)=="December")){escenario_Dic[[w]]=sample(a$December,1)})
    
  }
  
  escenarios_final1=do.call("rbind",esc_consolidado)
  
  orden=match(month.prob,colnames(year_sort))
  ord_col=order(match(sort(orden),orden))
  
  #     escenarios_final=rbind(escenarios_final1[,ord_col],todo3)
  #     nom=c(seq(1,num_esc1),"min","prom","max")
  
  escenarios_final=escenarios_final1[,ord_col]
  nom=seq(1,num_esc1)
  
  
  
  escenarios_final=as.data.frame(escenarios_final)
  names(escenarios_final)=month.prob
  
  #---------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------#
  #----------------------Creaci�n de escenarios a nivel diario----------------------#
  #---------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------#
  print("Generando escenarios...")
  
    esc_final_diarios=list()
    
    if(any(names(a)=="January")){ esc_diario_Ene=list()}
    if(any(names(a)=="February")){ esc_diario_Feb=list()}
    if(any(names(a)=="March")){ esc_diario_Mar=list()}
    if(any(names(a)=="April")){ esc_diario_Abr=list()}
    if(any(names(a)=="May")){ esc_diario_May=list()}
    if(any(names(a)=="June")){ esc_diario_Jun=list()}
    if(any(names(a)=="July")){ esc_diario_Jul=list()}
    if(any(names(a)=="August")){ esc_diario_Ago=list()}
    if(any(names(a)=="September")){ esc_diario_Sep=list()}
    if(any(names(a)=="October")){ esc_diario_Oct=list()}
    if(any(names(a)=="November")){ esc_diario_Nov=list()}
    if(any(names(a)=="December")){ esc_diario_Dic=list()}
    
    for (n in 1:nrow(escenarios_final)){
      
      esc_final_diarios[[n]]=rbind(
        if(any(names(a)=="January")){esc_diario_Ene[[n]]=data_d[data_d$month=="1"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="January")],]},
        if(any(names(a)=="February")){esc_diario_Feb[[n]]=data_d[data_d$month=="2"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="February")],]},
        if(any(names(a)=="March")){esc_diario_Mar[[n]]=data_d[data_d$month=="3"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="March")],]},
        if(any(names(a)=="April")){esc_diario_Abr[[n]]=data_d[data_d$month=="4"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="April")],]},
        if(any(names(a)=="May")){esc_diario_May[[n]]=data_d[data_d$month=="5"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="May")],]},
        if(any(names(a)=="June")){esc_diario_Jun[[n]]=data_d[data_d$month=="6"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="June")],]},
        if(any(names(a)=="July")){esc_diario_Jul[[n]]=data_d[data_d$month=="7"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="July")],]},
        if(any(names(a)=="August")){esc_diario_Ago[[n]]=data_d[data_d$month=="8"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="August")],]},
        if(any(names(a)=="September")){esc_diario_Sep[[n]]=data_d[data_d$month=="9"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="September")],]},
        if(any(names(a)=="October")){esc_diario_Oct[[n]]=data_d[data_d$month=="10"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="October")],]},
        if(any(names(a)=="November")){esc_diario_Nov[[n]]=data_d[data_d$month=="11"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="November")],]},
        if(any(names(a)=="December")){ esc_diario_Dic[[n]]=data_d[data_d$month=="12"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="December")],]})
      
    }
    
    for (n in 1:nrow(escenarios_final)){
      ord=order(match(as.numeric(esc_final_diarios[[n]]$month),orden))
      
      esc_final_diarios[[n]]=esc_final_diarios[[n]][ord,]
    }
    
    
 
    #---------------------------------------------------------------------------------#
    #-----------------Exporta los escenarios a nivel diario a .csv--------------------#
    #---------------------------------------------------------------------------------#
    
    dir.create(paste(path_output,format.Date(Sys.Date(),"%Y%m%d"),sep="/"),showWarnings=F)
    dir.create(paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,sep=""),showWarnings=F)
    
    for(k in 1:nrow(escenarios_final)){
      write.csv(esc_final_diarios[[k]],paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,"/escenario_",nom[k],".csv",sep=""),row.names=F)
    }
    
  print("Proceso finalizado exitosamente")

 
}


#---------------------------------------------------------------------------------#
#---------------------------RUN para todas las estaciones-------------------------#
#---------------------------------------------------------------------------------#

path_output = "Y:/USAID_Project/Product_1_web_interface/test/clima/resampling/" 
path_prob = "Y:/USAID_Project/Product_1_web_interface/test/clima/prob_forecast/20170120_prob.csv"
path_data_d = "Y:/USAID_Project/Product_1_web_interface/test/clima/daily_data/"

data_d_all = list.files(path_data_d,full.names = T)

data_prob_all=read.csv(path_prob,header=T,dec=".")
station_names = unique(data_prob_all$id)

start.time <- Sys.time()
sapply(1:length(data_d_all),function(x) {
  data_prob = data_prob_all[which(data_prob_all$id==station_names[x]),]
  gen_esc_daily(prob = data_prob,data_d = data_d_all[x],path_output,station = station_names[x]
                )})

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken