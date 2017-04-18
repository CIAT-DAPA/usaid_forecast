
library(lubridate)
library(reshape)
require(ggplot2)
library(stringr)

library(raster)
library(rgdal)
library(R.utils)
library(stringr)
library(parallel)
library(trend)

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
  for(i in 1:length(data)){
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
#----------------Función sacar número de días del mes-----------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# date: fecha de interés
# OUTPUT
# Número de días del mes de interés

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

#---------------------------------------------------------------------------------#
#----------------Función sacar datos diarios NASA POWER---------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# lat: latitud de la estación/sitio de interés
# lon: longitud de la estación/sitio de interés
# year_to: año actual
# month_to: mes actual
# OUTPUT
# Datos diarios de temperatura máxima, mínima y radiación solar de NASA POWER

download_data_nasa = function(lat,lon,year_to,month_to,data_d){
  
  url_all = paste0("https://power.larc.nasa.gov/cgi-bin/agro.cgi?email=&area=area&latmin=",lat,"&lonmin=",lon,"&latmax=",lat,"&lonmax=",lon,"&ms=1&ds=1&ys=1983&me=12&de=31&ye=",year_to,"&p=swv_dwn&p=T2MN&p=T2MX&submit=Submit")
  data_nasa = read.table(url_all,skip = 15,header = F, na.strings = "-")
  names(data_nasa) = c("year","julian","sol_rad","t_min","t_max")
  dates = seq(as.Date("1983/1/1"), as.Date(paste0(year_to,"/12/31")), "days")
  month = as.numeric(format(dates,"%m"))
  #data_d = read.csv("D:/_Scripts/usaid_forecast/_package/prediccionClimatica/dailyData/58504f1a006cb93ed40eebe3.csv",header=T,dec=".")
  
  sel_obs = data_d[data_d$year %in% unique(data_nasa$year),]
  sel_nasa = data_nasa[data_nasa$year %in% unique(data_d$year),c(-1,-2)]
  
  ses_tmax = mean(sel_obs$t_max-sel_nasa$t_max,na.rm=T)
  ses_tmin = mean(sel_obs$t_min-sel_nasa$t_min,na.rm=T)
  ses_srad = mean(sel_obs$sol_rad-sel_nasa$sol_rad,na.rm=T)
  
  data_sel_m = data_nasa[data_nasa$year %in% year_to & month %in% month_to,3:5]
  data_sel_m$sol_rad = data_sel_m$sol_rad+ses_srad
  data_sel_m$t_min = data_sel_m$t_min+ses_tmin
  data_sel_m$t_max = data_sel_m$t_max+ses_tmax
  
  data_sel_m$sol_rad[is.na(data_sel_m$sol_rad)] = mean(data_sel_m$sol_rad,na.rm=T)
  data_sel_m$t_max[is.na(data_sel_m$t_max)] = mean(data_sel_m$t_max,na.rm=T)
  data_sel_m$t_min[is.na(data_sel_m$t_min)] = mean(data_sel_m$t_min,na.rm=T)
  
  return(data_sel_m)
}

#---------------------------------------------------------------------------------#
#----------------Función sacar datos diarios CHIRP--------------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# lat: latitud de la estación/sitio de interés
# lon: longitud de la estación/sitio de interés
# ini.date: fecha inicio de descarga
# end.date: fecha final de descarga
# outDir: Directorio donde se guardarán las imagenes de chirps
# OUTPUT
# Datos diarios de precipitación de CHIRP

download_data_chirp = function(ini.date,end.date,outDir,cl){
  
  z=seq(as.Date(ini.date), as.Date(end.date), "days")
  fechas=str_replace_all(z, "-", ".")  
  
  urls <- paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/daily/2017/chirp.",fechas,".tif",sep="")
  file <- basename(urls)
  outDir_all = paste0(outDir,"/",file)
  
  
  clusterMap(cl, download.file, url = urls, destfile = outDir_all, mode = "wb", 
             .scheduling = 'dynamic')
  
  
  
  return("Datos de CHIRPS descargados!")
}


#---------------------------------------------------------------------------------#
#-----------------Función para generar escenarios diarios-------------------------#
#---------------------------------------------------------------------------------#
# INPUT
# prob: Tabla de probabilidades de la estación de interés para los siguientes 6 meses
# data_d: Tabla con datos diarios de la estación de interés
# path_output: Ruta donde se guardarán las salidas
# station: Nombre de la estación de interés

# OUTPUT
# Al correr esta función se generarán los 100 escenarios (en formato .csv) de datos diarios 
# para la estaciónde interés

gen_esc_daily <- function(prob,data_d,path_output,station,lat,lon){
  
  cat("\n Inicio del remuestreo... \n")
  
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

#  month.num = unique(data_prob$month)
  for (i in 1:6){
    prec_month = data[data$month==probabilidades$month[i],3]
    year_month = data[data$month==probabilidades$month[i],1]
    
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
  
  cat("\n Calculando tendencias de temperaturas... \n")
  
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
  #-------------Generación de datos y resumen de los años mas probables-------------#
  #---------------------------------------------------------------------------------#

    valores=function(mes,var,Años){
      datos=0
      for(i in 1:length(mes))
        datos[i]=var[which(mes[i]==Años)]
      return(datos)
    }

    todo=sapply(1:dim(probabilidades)[2], function(i) valores(masprobable2[,i],prec_sort[,i],year_sort[,i]))
    todo2=as.data.frame(rbind(masprobable2,c("Datos análogos",rep("",dim(probabilidades)[2]-1)),todo)) ###Años y datos analogos
    colnames(todo2)=names(probabilidades)

    resumen=function(x) rbind(min(x),max(x))
    resumen2=apply(todo,2,resumen)

    medias=apply(todo,2,median)

    dif=t(t(todo)-medias)
    valores2=function(masprobable2,todo,resumen2,dif){

      datos=0
      for(i in 1:2){
        pos<-which(todo==resumen2[i])
        n=length(pos)
        datos[i]=masprobable2[pos[sample(n,1)]]
      }

      pos2=which(abs(dif)==min(abs(dif)))
      n2=length(pos2)
      datos2=masprobable2[pos2[sample(n2,1)]]

      datost=c(datos[1],datos2,datos[2])

      return(datost)
    }

    todo3=sapply(1:dim(probabilidades)[2], function(i) valores2(masprobable2[,i],todo[,i],resumen2[,i],dif[,i]))

    resumen3=rbind(resumen2[1,],round(medias,2),resumen2[2,])
    row.names(resumen3)=c("min","avg","max")

    resumenf=rbind(resumen3,c("Años",rep("",dim(probabilidades)[2]-1)),todo3) ###Resumen con min max y prom de los escenarios analogos
    colnames(resumenf)=names(probabilidades)

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
  
  
  
      escenarios_final=rbind(escenarios_final1[,ord_col],todo3)
      nom=c(seq(1,num_esc1),"min","prom","max")

  # escenarios_final=escenarios_final1[,ord_col]
  # nom=seq(1,num_esc1)
  # 
  
  
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
    
    all_min = aggregate(esc_final_diarios[[101]][,4:7],list(esc_final_diarios[[101]]$year,esc_final_diarios[[101]]$month),mean)
    all_min = all_min[orden,]

    all_avg = aggregate(esc_final_diarios[[102]][,4:7],list(esc_final_diarios[[101]]$year,esc_final_diarios[[101]]$month),mean)
    all_avg = all_avg[orden,]
    
    all_max = aggregate(esc_final_diarios[[103]][,4:7],list(esc_final_diarios[[101]]$year,esc_final_diarios[[101]]$month),mean)
    all_max = all_max[orden,]
    
    
    prec_limit = cbind(data_prob[,1:2],t(resumen3))
    tmax_limit = cbind(data_prob[,1:2],all_min$t_max,all_avg$t_max,all_max$t_max)
    tmin_limit = cbind(data_prob[,1:2],all_min$t_min,all_avg$t_min,all_max$t_min)
    srad_limit = cbind(data_prob[,1:2],all_min$sol_rad,all_avg$sol_rad,all_max$sol_rad)
    
    names(tmax_limit) = names(prec_limit)
    names(tmin_limit) = names(prec_limit)
    names(srad_limit) = names(prec_limit)
    
    tmax_limit_ord = t(apply(tmax_limit[,3:5],1,order,decreasing =F))
    for(ord in 1:6){
      tmax_limit[ord,3:5] = tmax_limit[ord,3:5][tmax_limit_ord[ord,]]
      
    }
    
    rm(ord)
    tmin_limit_ord = t(apply(tmin_limit[,3:5],1,order,decreasing =F))
    for(ord in 1:6){
      tmin_limit[ord,3:5] = tmin_limit[ord,3:5][tmin_limit_ord[ord,]]
      
    }
    
    rm(ord)
    srad_limit_ord = t(apply(srad_limit[,3:5],1,order,decreasing =F))
    for(ord in 1:6){
      srad_limit[ord,3:5] = srad_limit[ord,3:5][srad_limit_ord[ord,]]
      
    }
    
    dir.create(paste(path_output,format.Date(Sys.Date(),"%Y%m%d"),sep="/"),showWarnings=F)
    dir.create(paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,sep=""),showWarnings=F)
    
    limit.name = c("min","avg", "max")
    for(j in 1:3){
      write.csv(prec_limit[,c(1,2,(j+2))],paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,"/prec_",limit.name[j], ".csv",sep=""),row.names=F)
      write.csv(tmax_limit[,c(1,2,(j+2))],paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,"/t_max_",limit.name[j], ".csv",sep=""),row.names=F)
      write.csv(tmin_limit[,c(1,2,(j+2))],paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,"/t_min_",limit.name[j], ".csv",sep=""),row.names=F)
      write.csv(srad_limit[,c(1,2,(j+2))],paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,"/sol_rad_",limit.name[j], ".csv",sep=""),row.names=F)
      
    }
     
 
    #---------------------------------------------------------------------------------#
    #-----------------Exporta los escenarios a nivel diario a .csv--------------------#
    #---------------------------------------------------------------------------------#
    cat("\n Descargando datos observados de NASA POWER... \n")
    
    year_to = format(Sys.Date(),"%Y")
    month_to = as.numeric(format(Sys.Date(),"%m"))-1
    
    data_nasa = download_data_nasa(lat,lon,year_to,month_to,data_d)
    
    cat("\n Extrayendo datos estimados de CHIRP... \n")
    
    
    outDir_all = list.files(path_output,pattern = ".tif",full.names = T )
    trs = list()
    for(j in 1:length(outDir_all)){ trs[[j]] <- raster(outDir_all[j]) }
  
    trs_st = stack(trs)
 
    extr_vals <- raster::extract(trs_st, data.frame(x=lon,y=lat))
    data_chirp <- as.numeric(extr_vals)
    
    
    
    z=seq(as.Date(ini.date), as.Date(end.date), "days")
    data_obs_m =  cbind.data.frame("day" = as.numeric(format(z,"%d")),"month" = as.numeric(format(z,"%m")), "year" = as.numeric(format(z,"%Y")),"t_max"= data_nasa$t_max,"t_min"=data_nasa$t_min,"prec" = data_chirp,"sol_rad" = data_nasa$sol_rad)
    for(k in 1:nrow(escenarios_final)){
      to.write = rbind.data.frame( data_obs_m,esc_final_diarios[[k]])
      write.csv(to.write,paste(path_output,"/",format.Date(Sys.Date(),"%Y%m%d"),"/Escenarios_",station,"/escenario_",nom[k],".csv",sep=""),row.names=F)
    }
    
  cat("\n Proceso finalizado exitosamente \n \n")

 
}


#---------------------------------------------------------------------------------#
#---------------------------RUN para todas las estaciones-------------------------#
#---------------------------------------------------------------------------------#

# path_output = "Y:/USAID_Project/Product_1_web_interface/test/clima/resampling/" 
# path_prob = "Y:/USAID_Project/Product_1_web_interface/test/clima/prob_forecast/20170120_prob.csv"
# path_data_d = "Y:/USAID_Project/Product_1_web_interface/test/clima/daily_data/"
    
    lat = 5.320
    lon = -72.388

    path_data_d <- dir_stations
    
    data_d_all = list.files(path_data_d,full.names = T)
    
    data_prob_all=read.csv(paste0(path_prob,"/",format(Sys.Date(),"%Y%m%d"),"_prob.csv"),header=T,dec=".")
    station_names = gsub('.csv','',list.files(path_data_d))
    cl <- makeCluster(detectCores())
    
    ini.date = paste0(substring(Sys.Date(),1,4),"-",str_pad(as.numeric(substring(Sys.Date(),7,7))-1,2,pad = "0"),"-01")
    ini.date = as.Date(ini.date)
    
    end.date = paste0(substring(Sys.Date(),1,4),"-",str_pad(as.numeric(substring(Sys.Date(),7,7))-1,2,pad = "0"),"-",numberOfDays(ini.date))
    end.date = as.Date(end.date)
    
     
    download_data_chirp(ini.date,end.date,outDir = path_output,cl)
    


start.time <- Sys.time()

for(x in 1:length(data_d_all)){
    data_prob = data_prob_all[which(data_prob_all$id==station_names[x]),]
    gen_esc_daily(prob = data_prob,data_d = data_d_all[x],path_output,station = station_names[x],lat,lon)
      }

file.remove(list.files(path_output,pattern = ".tif",full.names=T))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken






#########################################################

#########################################################









# plot(sel_nasa[500:600,1]+ses_srad,type="l")
# lines(sel_obs[500:600,7],type="l",col="red")
# 
# plot(sel_nasa[500:600,2]+ses_tmin,type="l")
# lines(sel_obs[500:600,5],type="l",col="red")
# 
# plot(sel_nasa[500:600,3]+ses_tmax,type="l")
# lines(sel_obs[500:600,4],type="l",col="red")

# 
# 
# srad_cum_nasa = aggregate(sel_nasa$sol_rad,list(sel_obs$month,sel_obs$year),sum,na.rm=T)
# srad_cum_obs = aggregate(sel_obs$sol_rad,list(sel_obs$month,sel_obs$year),sum,na.rm=T)
# plot(srad_cum_nasa[,3],type="l",ylim=c(350,700))
# lines(srad_cum_obs[,3],type="l",col="red")



