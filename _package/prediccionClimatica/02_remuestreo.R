
# library(lubridate)
# library(reshape)
# #require(ggplot2)
# library(stringr)



#---------------------------------------------------------------------------------#
#-------------------Función para generar barra de progreso\-----------------------#
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
#----------------Función para generar años más frecuentes-------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# data: Datos acumulados mensuales para la precipitación del mes a pronosticar
# organizados de forma ascendente
# añoshistorico: Tabla de años ordenados de forma ascendente de acuerdo a la precipitación
# acumulada del mes de interés

# OUTPUT
# Remuestreo de los años mas frecuentes del mes de interés de acuerdo a las probabilidades ingresadas

resampling <- function(data,prob,añoshistorico){
  matrizcombinaciones=0
  vectorprobabilidades=prob
  datas=0
  for(i in 1:100){
    r=sample(prob,1,prob=prob)
    if(r==vectorprobabilidades[1]){
      datas=which(data<quantile(data[which(data>0)],0.3333))
      
      matrizcombinaciones[i]=añoshistorico[sample(datas,1)]
    }
    
    if(r==vectorprobabilidades[2]){
      datas=which(data>=quantile(data,0.3333) & data<quantile(data,0.6666))
      matrizcombinaciones[i]=añoshistorico[sample(datas,1)]
      
    }
    
    if(r==vectorprobabilidades[3]){
      datas=which(data>=quantile(data,0.6666))
      matrizcombinaciones[i]=añoshistorico[sample(datas,1)]
      
    }
    
    
  }
  return(matrizcombinaciones)
  
  
}



#---------------------------------------------------------------------------------#
#-----------------Función para generar escenarios diarios-------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# prob: Tabla de probabilidades de la estación de interés para los siguientes 6 meses
# data_d: Tabla con datos diarios de la estación de interés
# path_output: Ruta donde se guardarán las salidas
# station: Nombre de la estación de interés

#OUTPUT
# Al correr esta función se generarán los 100 escenarios (en formato .csv) de datos diarios 
# para la estaciónde interés

gen_esc_daily <- function(prob,data_d,path_output,station){
  
   
  #---------------------------------------------------------------------------------#
  #-------------------------------Lectura de datos----------------------------------#
  #---------------------------------------------------------------------------------#
  
  data_d=read.csv(data_d,header=T,dec=".")
   
  
  #attach(data_d,warn.conflicts =F)
  
  data=aggregate(as.numeric(data_d$prec),list(data_d$year,data_d$month),sum)
  data_temp=aggregate(data_d[,4:5],list(data_d$year,data_d$month),mean)
  
  names(data)=c("year","month","value")
  names(data_temp)=c("year","month","t_max","t_min")
  
  probabilidades=as.data.frame(data_prob[1:6,])
  month.prob = month.name[probabilidades$month] 
  
  #---------------------------------------------------------------------------------#
  #-----------------Ordenar de menor a Mayr datos mensuales históricos-------------#
  #---------------------------------------------------------------------------------#
  
  cat("\n Calculando terciles de la precipitación... \n")
  
  prec_sort = matrix(NA,length(unique(data$year)),6)
  year_sort = matrix(NA,length(unique(data$year)),6)
  
  t_max_trend = matrix(NA,length(unique(data$year)),6)
  t_min_trend = matrix(NA,length(unique(data$year)),6)
  
  for (i in 1:6){
    prec_month = data[data$month==i,3]
    year_month = data[data$month==i,1]
    
    t_max_trend[,i] = data_temp[data_temp$month==probabilidades$month[i],3]
    t_min_trend[,i] = data_temp[data_temp$month==probabilidades$month[i],4]
    
    prec_sort[,i] = prec_month[order(prec_month)]
    year_sort[,i] = year_month[order(prec_month)]
  }
  
  colnames(prec_sort)=month.prob
  colnames(year_sort)=month.prob
  
  colnames(t_max_trend)=month.prob
  colnames(t_min_trend)=month.prob
  #---------------------------------------------------------------------------------#
  #------------------------Cálculo tendencias para temp-----------------------------#
  #---------------------------------------------------------------------------------#
  
 
  s.pred_new = matrix(NA,6,2)
  for(v in 1:6){
    
    by_month_tmax = ts(start = min(data_temp$year),end = max(data_temp$year),t_max_trend[,v])
    sen.res_tmax = sens.slope(by_month_tmax)
    
    by_month_tmin = ts(start = min(data_temp$year),end = max(data_temp$year),t_min_trend[,v])
    sen.res_tmin = sens.slope(by_month_tmin)
    
    # t <- (1:length(by_month))
    # s.pred <- sen.res$intercept + sen.res$b.sen * t
    if(data.table::between(0, sen.res_tmax$b.sen.lo, sen.res_tmax$b.sen.up)==F){
      s.pred_new[v,1] <- sen.res_tmax$b.sen
    } 
    if(data.table::between(0, sen.res_tmin$b.sen.lo, sen.res_tmin$b.sen.up)==F){
      s.pred_new[v,2] <- sen.res_tmin$b.sen
    }  
    
  }
  
  s.pred_new[which(is.na(s.pred_new))] = 0 
  colnames(s.pred_new) = c("t_max","t_min")
  row.names(s.pred_new) = month.prob
  
  trend_by_year_max = matrix(NA,length(by_month_tmax),6)
  trend_by_year_min = matrix(NA,length(by_month_tmax),6)
  
  for(y in 1:6){
    trend_by_year_max[,y] = s.pred_new[y,1]*1:length(by_month_tmax)
    trend_by_year_min[,y] = s.pred_new[y,2]*1:length(by_month_tmax)
    
  }
  
  
  colnames(trend_by_year_max) = month.prob
  row.names(trend_by_year_max) = max(data_temp$year):min(data_temp$year) 
  
  colnames(trend_by_year_min) = month.prob
  row.names(trend_by_year_min) = max(data_temp$year):min(data_temp$year) 
  
  data_d_trend = data_d[data_d$month %in% probabilidades$month,]
  
for(y in min(data_temp$year):max(data_temp$year)){
  for(m in probabilidades$month){
    trend_max = trend_by_year_max[which(row.names(trend_by_year_max)==y),month.name[m]]
    trend_min = trend_by_year_min[which(row.names(trend_by_year_min)==y),month.name[m]]
    
    pos_max = which(data_d_trend$year==y & data_d_trend$month==m)
    pos_min = which(data_d_trend$year==y & data_d_trend$month==m)
    data_d_trend$t_max[pos_max] = data_d_trend$t_max[pos_max]+trend_max
    data_d_trend$t_min[pos_min] = data_d_trend$t_min[pos_min]+trend_min
    
  }
  
}  
  
  #---------------------------------------------------------------------------------#
  #--------------Generación de los 10 años análogos mas probables-------------------#
  #---------------------------------------------------------------------------------#
  
  masprobable=matrix(0,nrow=100,ncol=dim(probabilidades)[2])
  
  cat("\n Generando años más frecuentes... \n")
  masprobable=sapply_pb(1:100,
                        function(j){
                          esc1=sapply(1:dim(probabilidades)[2], function(i) resampling(prec_sort[,i],probabilidades[i,4:6],year_sort[,i]))
                          masprobable[j,]=sapply(1:dim(probabilidades)[2],function(j) as.numeric(rownames(cbind(which(table(esc1[,j])==max(table(esc1[,j]))))))[1])
                        })    
  
  masprobable2=apply(t(masprobable),2,function(x) as.numeric(names(sort(table(x),T))[1:10]))
  
    
  #---------------------------------------------------------------------------------#
  #----------Generación de todos los escenarios definidos por el usuario------------#
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
  #----------------------Creación de escenarios a nivel diario----------------------#
  #---------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------#
  cat("\n Generando escenarios diarios... \n")
  
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
        if(any(names(a)=="January")){esc_diario_Ene[[n]]=data_d_trend[data_d_trend$month=="1"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="January")],]},
        if(any(names(a)=="February")){esc_diario_Feb[[n]]=data_d_trend[data_d_trend$month=="2"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="February")],]},
        if(any(names(a)=="March")){esc_diario_Mar[[n]]=data_d_trend[data_d_trend$month=="3"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="March")],]},
        if(any(names(a)=="April")){esc_diario_Abr[[n]]=data_d_trend[data_d_trend$month=="4"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="April")],]},
        if(any(names(a)=="May")){esc_diario_May[[n]]=data_d_trend[data_d_trend$month=="5"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="May")],]},
        if(any(names(a)=="June")){esc_diario_Jun[[n]]=data_d_trend[data_d_trend$month=="6"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="June")],]},
        if(any(names(a)=="July")){esc_diario_Jul[[n]]=data_d_trend[data_d_trend$month=="7"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="July")],]},
        if(any(names(a)=="August")){esc_diario_Ago[[n]]=data_d_trend[data_d_trend$month=="8"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="August")],]},
        if(any(names(a)=="September")){esc_diario_Sep[[n]]=data_d_trend[data_d_trend$month=="9"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="September")],]},
        if(any(names(a)=="October")){esc_diario_Oct[[n]]=data_d_trend[data_d_trend$month=="10"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="October")],]},
        if(any(names(a)=="November")){esc_diario_Nov[[n]]=data_d_trend[data_d_trend$month=="11"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="November")],]},
        if(any(names(a)=="December")){ esc_diario_Dic[[n]]=data_d_trend[data_d_trend$month=="12"&data_d_trend$year==escenarios_final[n,which(names(escenarios_final)=="December")],]})
      
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
    
  cat("\n Proceso finalizado exitosamente \n")

 
}


#---------------------------------------------------------------------------------#
#---------------------------RUN para todas las estaciones-------------------------#
#---------------------------------------------------------------------------------#

# path_output = "Y:/USAID_Project/Product_1_web_interface/test/clima/resampling/" 
# path_prob = "Y:/USAID_Project/Product_1_web_interface/test/clima/prob_forecast/20170120_prob.csv"
# path_data_d = "Y:/USAID_Project/Product_1_web_interface/test/clima/daily_data/"
path_data_d <- dir_stations

data_d_all = list.files(path_data_d,full.names = T)

data_prob_all=read.csv(path_prob,header=T,dec=".")
station_names = gsub('.csv','',list.files(path_data_d))

start.time <- Sys.time()

for(x in 1:length(data_d_all)){
    data_prob = data_prob_all[which(data_prob_all$id==station_names[x]),]
    gen_esc_daily(prob = data_prob,data_d = data_d_all[x],path_output,station = station_names[x])
      }

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
