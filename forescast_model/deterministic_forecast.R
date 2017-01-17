
require(lubridate)
require(reshape)
require(ggplot2)
require(stringr)



#Función para sacar barra de progreso
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

pronosticos=function(path_prob,path_data_d,path_output){
  
  path_prob =  "C:\\Users\\lllanos\\Desktop\\datos_prueba\\prob_prueba.csv"
  path_data_d =  "C:\\Users\\lllanos\\Desktop\\datos_prueba\\LaUnion.csv"
  
  #---------------------------------------------------------------------------------#
  #-------------------------------Lectura de datos----------------------------------#
  #---------------------------------------------------------------------------------#
  
  data_d=read.csv(path_data_d,header=T,dec=".")
  data_prob=read.csv(path_prob,header=T,dec=".")
  
  
  #attach(data_d,warn.conflicts =F)
  
  data=aggregate(as.numeric(data_d$prec),list(data_d$year,data_d$month),sum)
  names(data)=c("year","month","value")
  
  probabilidades=as.data.frame(data_prob[1:6,])
  month.prob = month.name[probabilidades$month] 
  
  #---------------------------------------------------------------------------------#
  #-----------------Ordenar de menor a mayor datos mensuales históricos-------------#
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
  
  
  #   Años_org2=Años_org[,match(names(probabilidades),colnames(Años_org))]
  #   var_org2=var_org[,match(names(probabilidades),colnames(var_org))]
  #   
  #---------------------------------------------------------------------------------#
  #---------------------------Cálculo de percentiles--------------------------------#
  #---------------------------------------------------------------------------------#
  
  percentiles=rbind(apply(prec_sort,2,FUN=quantile,0.3333),apply(prec_sort,2,FUN=quantile,0.6666),apply(prec_sort,2,FUN=quantile,0.9999))
  rownames(percentiles)=c("T1","T2","T3")
  colnames(percentiles)=month.prob
  
  
  
  #---------------------------------------------------------------------------------#
  #-------------------Función para generar años analogos----------------------------#
  #---------------------------------------------------------------------------------#
  
  insumo=function(data,prob,añoshistorico){
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
  #--------------Generación de los 12 años análogos mas probables-------------------#
  #---------------------------------------------------------------------------------#
  
  masprobable=matrix(0,nrow=100,ncol=dim(probabilidades)[2])
  
  
  masprobable=sapply_pb(1:100,
                        function(j){
                          esc1=sapply(1:dim(probabilidades)[2], function(i) insumo(prec_sort[,i],probabilidades[i,4:6],year_sort[,i]))
                          masprobable[j,]=sapply(1:dim(probabilidades)[2],function(j) as.numeric(rownames(cbind(which(table(esc1[,j])==max(table(esc1[,j]))))))[1])
                        })    
  
  #masprobable2=apply(t(masprobable),2,function(masprobable)sample(masprobable,12,rep=F))
  masprobable2=apply(t(masprobable),2,function(x) as.numeric(names(sort(table(x),T))[1:10]))
  
  #---------------------------------------------------------------------------------#
  #-------------Generación de datos y resumen de los años mas probables-------------#
  #---------------------------------------------------------------------------------#
  #   
  #   valores=function(mes,var,Años){
  #     datos=0
  #     for(i in 1:length(mes))
  #       datos[i]=var[which(mes[i]==Años)]
  #     return(datos)
  #   }
  #   
  #   todo=sapply(1:dim(probabilidades)[2], function(i) valores(masprobable2[,i],var_org2[,i],Años_org2[,i]))
  #   todo2=as.data.frame(rbind(masprobable2,c("Datos análogos",rep("",dim(probabilidades)[2]-1)),todo)) ###Años y datos analogos
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
  #   row.names(resumen3)=c("Mín","Promedio","Máx")
  #   
  #   resumenf=rbind(resumen3,c("Años",rep("",dim(probabilidades)[2]-1)),todo3) ###Resumen con min max y prom de los escenarios analogos
  #   colnames(resumenf)=names(probabilidades)
  #   
  #   ###Salida de años analogos y resumen
  #   write.csv(todo2,file="Pronosticos/añosanalogos.csv")
  #   write.csv(resumenf,file="Pronosticos/resumen_añosanalogos.csv")
  #   
  #---------------------------------------------------------------------------------#
  #----------Generación de todos los escenarios definidos por el usuario------------#
  #---------------------------------------------------------------------------------#
  num_esc1=100
  
  a=masprobable2
  a=as.data.frame(a)
  names(a)=month.prob
  
  
  if(any(names(a)=="Enero")){ escenario_Ene=list()}
  if(any(names(a)=="Febrero")){ escenario_Feb=list()}
  if(any(names(a)=="Marzo")){ escenario_Mar=list()}
  if(any(names(a)=="Abril")){ escenario_Abr=list()}
  if(any(names(a)=="Mayo")){ escenario_May=list()}
  if(any(names(a)=="Junio")){ escenario_Jun=list()}
  if(any(names(a)=="Julio")){ escenario_Jul=list()}
  if(any(names(a)=="Agosto")){ escenario_Ago=list()}
  if(any(names(a)=="Septiembre")){ escenario_Sep=list()}
  if(any(names(a)=="Octubre")){ escenario_Oct=list()}
  if(any(names(a)=="Noviembre")){ escenario_Nov=list()}
  if(any(names(a)=="Diciembre")){ escenario_Dic=list()}
  
  esc_consolidado=list()
  
  
  for(w in 1:num_esc1){
    esc_consolidado[[w]]=cbind(
      if(any(names(a)=="January")){ escenario_Ene[[w]]=sample(a$January,1)},
      if(any(names(a)=="February")){ escenario_Feb[[w]]=sample(a$February,1)},
      if(any(names(a)=="March")){ escenario_Mar[[w]]=sample(a$March,1)},
      
      if(any(names(a)=="April")){ escenario_Abr[[w]]=sample(a$April,1)},
      if(any(names(a)=="May")){escenario_May[[w]]=sample(a$Mayo,1)},
      if(any(names(a)=="June")){escenario_Jun[[w]]=sample(a$Junio,1)},
      if(any(names(a)=="July")){escenario_Jul[[w]]=sample(a$Julio,1)},
      if(any(names(a)=="August")){ escenario_Ago[[w]]=sample(a$Agosto,1)},
      if(any(names(a)=="September")){escenario_Sep[[w]]=sample(a$Septiembre,1)},
      if(any(names(a)=="October")){escenario_Oct[[w]]=sample(a$Octubre,1)},
      if(any(names(a)=="November")){escenario_Nov[[w]]=sample(a$Noviembre,1)},
      if(any(names(a)=="December")){escenario_Dic[[w]]=sample(a$Diciembre,1)})
    
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
  print("Generando escenarios")
  if(exists("data_d")){
    esc_final_diarios=list()
    
    if(any(names(a)=="Enero")){ esc_diario_Ene=list()}
    if(any(names(a)=="Febrero")){ esc_diario_Feb=list()}
    if(any(names(a)=="Marzo")){ esc_diario_Mar=list()}
    if(any(names(a)=="Abril")){ esc_diario_Abr=list()}
    if(any(names(a)=="Mayo")){ esc_diario_May=list()}
    if(any(names(a)=="Junio")){ esc_diario_Jun=list()}
    if(any(names(a)=="Julio")){ esc_diario_Jul=list()}
    if(any(names(a)=="Agosto")){ esc_diario_Ago=list()}
    if(any(names(a)=="Septiembre")){ esc_diario_Sep=list()}
    if(any(names(a)=="Octubre")){ esc_diario_Oct=list()}
    if(any(names(a)=="Noviembre")){ esc_diario_Nov=list()}
    if(any(names(a)=="Diciembre")){ esc_diario_Dic=list()}
    
    for (n in 1:nrow(escenarios_final)){
      
      esc_final_diarios[[n]]=rbind(
        if(any(names(a)=="Enero")){esc_diario_Ene[[n]]=data_d[data_d$month=="1"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Enero")],]},
        if(any(names(a)=="Febrero")){esc_diario_Feb[[n]]=data_d[data_d$month=="2"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Febrero")],]},
        if(any(names(a)=="Marzo")){esc_diario_Mar[[n]]=data_d[data_d$month=="3"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Marzo")],]},
        if(any(names(a)=="Abril")){esc_diario_Abr[[n]]=data_d[data_d$month=="4"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Abril")],]},
        if(any(names(a)=="Mayo")){esc_diario_May[[n]]=data_d[data_d$month=="5"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Mayo")],]},
        if(any(names(a)=="Junio")){esc_diario_Jun[[n]]=data_d[data_d$month=="6"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Junio")],]},
        if(any(names(a)=="Julio")){esc_diario_Jul[[n]]=data_d[data_d$month=="7"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Julio")],]},
        if(any(names(a)=="Agosto")){esc_diario_Ago[[n]]=data_d[data_d$month=="8"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Agosto")],]},
        if(any(names(a)=="Septiembre")){esc_diario_Sep[[n]]=data_d[data_d$month=="9"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Septiembre")],]},
        if(any(names(a)=="Octubre")){esc_diario_Oct[[n]]=data_d[data_d$month=="10"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Octubre")],]},
        if(any(names(a)=="Noviembre")){esc_diario_Nov[[n]]=data_d[data_d$month=="11"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Noviembre")],]},
        if(any(names(a)=="Diciembre")){ esc_diario_Dic[[n]]=data_d[data_d$month=="12"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Diciembre")],]})
      
    }
    
    for (n in 1:nrow(escenarios_final)){
      ord=order(match(as.numeric(esc_final_diarios[[n]]$month),orden))
      
      esc_final_diarios[[n]]=esc_final_diarios[[n]][ord,]
    }
    
    
    #---------------------------------------------------------------------------------#
    #-----------------Exporta los escenarios a nivel diario a oryza--------------------#
    #---------------------------------------------------------------------------------#
    
    #     julian.date = yday(seq(as.Date(paste0("2016-",orden[1],"-01")), as.Date(paste0("2017-",orden[6],"-31")), "days"))
    #     julian.date.b = yday(seq(as.Date(paste0("2016-",orden[1],"-01")), as.Date(paste0("2017-",orden[6],"-31")), "days"))
    #     
    #     ano.ini = c()
    #     if(any(diff(julian.date)<=-364)){
    #        pos = which.min(diff(julian.date))
    #        ano.ini[1:pos] = year(Sys.Date())
    #        ano.ini[(pos+1):length(julian.date)] = year(Sys.Date())+1
    #       }else{
    #        ano.ini[1:length(julian.date)] = year(Sys.Date())
    #       }
    #     
    #     
    #     ano.ini.b = c()
    #     if(any(diff(julian.date.b)<=-364)){
    #       pos = which.min(diff(julian.date.b))
    #       ano.ini.b[1:pos] = year(Sys.Date())
    #       ano.ini.b[(pos+1):length(julian.date.b)] = year(Sys.Date())+1
    #     }else{
    #       ano.ini.b[1:length(julian.date.b)] = year(Sys.Date())
    #     }
    #       
    #     #}
    #     dir.create("Pronosticos/esc_oryza",showWarnings = F)
    #     filas=unlist(lapply(esc_final_diarios,nrow))
    #     filas.1=which(filas==182)
    #       
    #     encab=paste(svalue(long),svalue(lat),svalue(alt),svalue(xx),svalue(yy),sep=",")
    #     for (H in 1:length(filas.1)){
    #       codigo=matrix(H,182)#crea vector con el codigo de la estacion
    #       
    #       
    #       if(filas[H] == min(unique(filas))){
    #         ano=as.matrix(ano.ini)
    #         juliano=matrix(julian.date,182) #Colocar dias julianos desde donde inicia hasta donde termina la serie
    #         
    #       }else{
    #         ano=as.matrix(ano.ini.b)
    #         juliano=matrix(julian.date.b,182)
    #       }
    #           
    #       SRAD=((esc_final_diarios[[filas.1[H]]]$srad)*1000)
    #       TMIN=esc_final_diarios[[filas.1[H]]]$tmin
    #       TMAX=esc_final_diarios[[filas.1[H]]]$tmax
    #       viento=matrix(-99,filas[filas.1[H]])
    #       RAIN=esc_final_diarios[[filas.1[H]]]$precip
    #       
    #       
    #       
    #       if(length(unique(ano))==1){
    #         clima=cbind(codigo,ano,juliano,SRAD,TMIN,TMAX,viento,viento,RAIN)
    #         
    #         b=paste("Pronosticos/esc_oryza/IBTO",nom[H],".",substring(unique(ano),2,4),sep="")  #Modificar nombre de archivos (4 caracteres)
    #         sink(b)
    #         cat(encab) #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
    #         cat("\n")
    #         write.table(clima,sep=",",row.names=F,col.names=F)
    #         sink()
    #         
    #       }else{
    #         clima=cbind(codigo,ano,juliano,SRAD,TMIN,TMAX,viento,viento,RAIN)
    #         clima.ini =  clima[clima[,2]==unique(ano)[1],]
    #         clima.fin =  clima[clima[,2]==unique(ano)[2],]
    #         
    #         b=paste("Pronosticos/esc_oryza/IBTO",nom[H],".",substring(clima.ini[1,2],2,4),sep="")  #Modificar nombre de archivos (4 caracteres)
    #         b.1=paste("Pronosticos/esc_oryza/IBTO",nom[H],".",substring(clima.fin[1,2],2,4),sep="")  #Modificar nombre de archivos (4 caracteres)
    #         
    #         sink(b)
    #         cat(encab) #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
    #         cat("\n")
    #         write.table(clima.ini,sep=",",row.names=F,col.names=F)
    #         sink()
    #         
    #         sink(b.1)
    #         cat(encab) #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
    #         cat("\n")
    #         write.table(clima.fin,sep=",",row.names=F,col.names=F)
    #         sink()
    #         
    #       }
    #       
    #        
    #     }
    #     
    
    
    sitio = svalue(sitio)
    #---------------------------------------------------------------------------------#
    #-----------------Exporta los escenarios a nivel diario a DSSAT--------------------#
    #---------------------------------------------------------------------------------#
    julian.date = yday(seq(as.Date(paste0("2016-",orden[1],"-01")), as.Date(paste0("2017-",orden[6],"-31")), "days"))
    julian.date.b = yday(seq(as.Date(paste0("2016-",orden[1],"-01")), as.Date(paste0("2017-",orden[6],"-31")), "days"))
    
    ano.ini = c()
    if(any(diff(julian.date)<=-364)){
      pos = which.min(diff(julian.date))
      ano.ini[1:pos] = year(Sys.Date())
      ano.ini[(pos+1):length(julian.date)] = year(Sys.Date())+1
    }else{
      ano.ini[1:length(julian.date)] = year(Sys.Date())
    }
    
    
    ano.ini.b = c()
    if(any(diff(julian.date.b)<=-364)){
      pos = which.min(diff(julian.date.b))
      ano.ini.b[1:pos] = year(Sys.Date())
      ano.ini.b[(pos+1):length(julian.date.b)] = year(Sys.Date())+1
    }else{
      ano.ini.b[1:length(julian.date.b)] = year(Sys.Date())
    }
    
    #}
    dir.create("esc_dssat",showWarnings = F)
    filas=unlist(lapply(esc_final_diarios,nrow))
    filas.1=which(filas==182)
    
    
    
    
    
    for (H in 1:99){
      codigo=matrix(H,182)#crea vector con el codigo de la estacion
      
      
      if(filas[H] == min(unique(filas))){
        ano=as.matrix(ano.ini)
        juliano=matrix(julian.date,182) #Colocar dias julianos desde donde inicia hasta donde termina la serie
        
      }else{
        ano=as.matrix(ano.ini.b)
        juliano=matrix(julian.date.b,182)
      }
      
      SRAD=(esc_final_diarios[[filas.1[H]]]$srad)
      TMAX=esc_final_diarios[[filas.1[H]]]$tmax
      TMIN=esc_final_diarios[[filas.1[H]]]$tmin
      RAIN=esc_final_diarios[[filas.1[H]]]$precip
      DEPW=matrix(NA,filas[filas.1[H]])
      viento=matrix(NA,filas[filas.1[H]])
      PAR=matrix(NA,filas[filas.1[H]])
      EVAP=matrix(NA,filas[filas.1[H]])
      RHUM=matrix(NA,filas[filas.1[H]])
      
      
      
      
      
      
      
      #      if(length(unique(ano))==1){
      #clima=cbind(codigo,ano,juliano,SRAD,TMIN,TMAX,viento,viento,RAIN)
      
      b=paste("esc_dssat/",sitio,substring(unique(ano)[1],3,4),str_pad(nom[H], 2, pad = "0"),".WTH",sep="")  #Modificar nombre de archivos (4 caracteres)
      sink(b)
      cat(paste("*WEATHER DATA :"), sitio)
      cat("\n")
      cat("\n")
      cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
      cat("\n")
      cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", sitio, as.numeric(svalue(lat)),as.numeric(svalue(long)), as.numeric(svalue(alt)),-99, -99.0, 0, 0))
      cat("\n")
      cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEPW  WIND  PAR  EVAP  RHUM'))
      cat("\n")
      cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f", paste0(substring(ano,3,4),str_pad(juliano, 3, pad = "0")), SRAD, TMAX, TMIN, RAIN, DEPW,viento,PAR,EVAP,RHUM)), sep = "\n")
      sink()    
      #       }else{
      #         clima=cbind(NA,ano,SRAD, TMAX, TMIN, RAIN, DEPW,viento,PAR,EVAP,RHUM)
      #         clima.ini =  clima[clima[,2]==unique(ano)[1],]
      #         clima.fin =  clima[clima[,2]==unique(ano)[2],]
      #         
      #         dy.ini = paste0(substring(ano,3,4),str_pad(juliano, 3, pad = "0"))[clima[,2]==unique(ano)[1]]
      #         dy.fin = paste0(substring(ano,3,4),str_pad(juliano, 3, pad = "0"))[clima[,2]==unique(ano)[2]]
      #         
      #         b=paste("Pronosticos/esc_dssat/",sitio,substring(clima.ini[1,2],3,4),str_pad(nom[H], 2, pad = "0"),".WTH",sep="")  #Modificar nombre de archivos (4 caracteres)
      #         b.1=paste("Pronosticos/esc_dssat/",sitio,substring(clima.fin[1,2],3,4),str_pad(nom[H], 2, pad = "0"),".WTH",sep="")  #Modificar nombre de archivos (4 caracteres)
      #         
      #         sink(b)
      #         cat(paste("*WEATHER DATA :"), sitio)
      #         cat("\n")
      #         cat("\n")
      #         cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
      #         cat("\n")
      #         cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", sitio, as.numeric(svalue(lat)),as.numeric(svalue(long)), as.numeric(svalue(alt)),-99, -99.0, 0, 0))
      #         cat("\n")
      #         cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEPW  WIND  PAR  EVAP  RHUM'))
      #         cat("\n")
      #         cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f",dy.ini,clima.ini[,3],clima.ini[,4],clima.ini[,5],clima.ini[,6],clima.ini[,7],clima.ini[,8],clima.ini[,9],clima.ini[,10],clima.ini[,11])), sep = "\n")
      #         sink()    
      #         
      #         sink(b.1)
      #         cat(paste("*WEATHER DATA :"), sitio)
      #         cat("\n")
      #         cat("\n")
      #         cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
      #         cat("\n")
      #         cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", sitio, as.numeric(svalue(lat)),as.numeric(svalue(long)), as.numeric(svalue(alt)),-99, -99.0, 0, 0))
      #         cat("\n")
      #         cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEPW  WIND  PAR  EVAP  RHUM'))
      #         cat("\n")
      #         cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f",dy.fin,as.numeric(clima.fin[,3]),as.numeric(clima.fin[,4]),as.numeric(clima.fin[,5]),as.numeric(clima.fin[,6]),as.numeric(clima.fin[,7]),as.numeric(clima.fin[,8]),as.numeric(clima.fin[,9]),as.numeric(clima.fin[,10]),as.numeric(clima.fin[,11]))), sep = "\n")
      #         sink()
      #         
      #       }
      
    }
    
    
    #---------------------------------------------------------------------------------#
    #-----------------Exporta los escenarios a nivel diario a .csv--------------------#
    #---------------------------------------------------------------------------------#
    
    dir.create("Pronosticos/Escenarios",showWarnings=F)
    
    for(k in 1:nrow(escenarios_final)){
      write.csv(esc_final_diarios[[k]],paste("Pronosticos/Escenarios/escenario_",nom[k],".csv",sep=""),row.names=F)
    }
    
    #---------------------------------------------------------------------------------#
    #----------------------------Gráfica para PRECIPITACIÓN---------------------------#
    #---------------------------------------------------------------------------------#
    
    esc_final_mensual_Lluvia=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    
    for(z in 1:nrow(escenarios_final)){
      prev=aggregate(as.numeric(esc_final_diarios[[z]]$precip),list(Mes=esc_final_diarios[[z]]$month),sum)
      ord=order(match(as.numeric(prev$Mes),orden))
      esc_final_mensual_Lluvia[z,]=t(prev)[-1,ord]
      
    }
    
    esc_final_mensual_Lluvia=as.data.frame(esc_final_mensual_Lluvia)
    colnames(esc_final_mensual_Lluvia)=names(probabilidades)
    
    Multianual_Libertad1=aggregate(data_d$precip,list(Mes=data_d$month,Año=data_d$year),sum) #Se carga la precipitación mensual multianual 
    Multianual_Libertad=aggregate(Multianual_Libertad1[,3],list(Mes=Multianual_Libertad1$Mes),mean)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    if(svalue(num_esc)==" "){promedio_escenarios_Lluvia=esc_final_mensual_Lluvia[2,] 
    }else{promedio_escenarios_Lluvia=apply(esc_final_mensual_Lluvia,2,mean)}
    
    
    tiff("Pronosticos/precip_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    a=barplot(as.numeric(Multianual_Libertad[,2]),names.arg=names(probabilidades),col="Slate Gray 2",ylab="Precipitacion (mm)",,cex.lab=1,main="Pronóstico precipitación",cex.main=0.8,ylim=c(0,max(Multianual_Libertad[,2])+200))
    
    for(m in 1:nrow(escenarios_final)){
      lines(a,as.numeric(esc_final_mensual_Lluvia[m,]),col="gray29",lwd=1.5)
    }
    
    
    lines(a,as.numeric(promedio_escenarios_Lluvia),ty="l",col="red3",lwd=2)
    legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray29","red3"),cex=0.8)
    box()
    dev.off()
    
    
    #---------------------------------------------------------------------------------#
    #----------------------------Gráfica para TMAX---------------------------#
    #---------------------------------------------------------------------------------#
    
    esc_final_mensual_Tmax=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    
    for(z in 1:nrow(escenarios_final)){
      prev=aggregate(esc_final_diarios[[z]]$tmax,list(Mes=esc_final_diarios[[z]]$month),mean)
      ord=order(match(as.numeric(prev$Mes),orden))
      esc_final_mensual_Tmax[z,]=t(prev)[-1,ord]
    }
    
    esc_final_mensual_Tmax=as.data.frame(esc_final_mensual_Tmax)
    colnames(esc_final_mensual_Tmax)=names(probabilidades)
    
    if(svalue(num_esc)==" "){promedio_escenarios_tmax=esc_final_mensual_Tmax[2,] 
    }else{promedio_escenarios_tmax=apply(esc_final_mensual_Tmax,2,mean)}
    
    Tmax_Multianual_Libertad1=aggregate(data_d$tmax,list(Mes=data_d$month,Año=data_d$year),mean) #Se carga la precipitación mensual multianual 
    Tmax_Multianual_Libertad=aggregate(Tmax_Multianual_Libertad1[,3],list(Mes=Tmax_Multianual_Libertad1$Mes),mean)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmax_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    r=barplot(as.numeric(Tmax_Multianual_Libertad[,2]),names.arg=names(probabilidades),xpd = FALSE,col="cornsilk1",ylab="Temperatura Máxima (°C)",cex.lab=1,,main="Pronóstico Temperatura Máxima ",cex.main=0.8,ylim=c(min(data_d$tmax)+2,max(data_d$tmax)-1))
    
    for(m in 1:nrow(escenarios_final)){
      lines(r,as.numeric(esc_final_mensual_Tmax[m,]),col="gray29")
    }
    
    lines(r,as.numeric(promedio_escenarios_tmax),ty="l",col="red3",lwd=2)
    legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("khaki","gray29","red3"),cex=0.8)
    box()
    dev.off()
    
    #---------------------------------------------------------------------------------#
    #-----------------------------Gráfica para TMIN  ---------------------------------#
    #---------------------------------------------------------------------------------#
    
    esc_final_mensual_tmin=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      prev=aggregate(esc_final_diarios[[z]]$tmin,list(Mes=esc_final_diarios[[z]]$month),mean)
      ord=order(match(as.numeric(prev$Mes),orden))
      esc_final_mensual_tmin[z,]=t(prev)[-1,ord]
      
    }
    
    esc_final_mensual_Tmin=as.data.frame(esc_final_mensual_tmin)
    colnames(esc_final_mensual_Tmin)=names(probabilidades)
    
    if(svalue(num_esc)==" "){promedio_escenarios_Tmin=esc_final_mensual_Tmin[2,] 
    }else{promedio_escenarios_Tmin=apply(esc_final_mensual_Tmin,2,mean)}
    
    Tmin_Multianual_Libertad1=aggregate(data_d$tmin,list(Mes=data_d$month,Año=data_d$year),mean) #Se carga la precipitación mensual multianual 
    Tmin_Multianual_Libertad=aggregate(Tmin_Multianual_Libertad1[,3],list(Mes=Tmin_Multianual_Libertad1$Mes),mean)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmin_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    
    r=barplot(as.numeric(Tmin_Multianual_Libertad[,2]),names.arg=names(probabilidades),ylim=c(min(data_d$tmin)+2,max(data_d$tmin)+3),xpd = FALSE,col="cornsilk1",ylab="Temperatura Mínima(°C)",cex.lab=1,main="Pronóstico Temperatura Mínima ",cex.main=0.8)
    
    for(m in 1:nrow(escenarios_final)){
      lines(r,as.numeric(esc_final_mensual_Tmin[m,]),ty="l",col="gray29")
    }
    
    lines(r,as.numeric(promedio_escenarios_Tmin),ty="l",col="red3",lwd=2)
    legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("khaki","gray29","red3"),cex=0.8)
    box()
    dev.off()
    
    #---------------------------------------------------------------------------------#
    #----------------------------Gráfica para RADIACION SOLAR-------------------------#
    #---------------------------------------------------------------------------------#
    
    if(exists("srad")){
      
      esc_final_mensual_srad=matrix(0,nrow(escenarios_final),ncol(probabilidades))
      
      for(z in 1:nrow(escenarios_final)){
        prev=aggregate(esc_final_diarios[[z]]$srad,list(Mes=esc_final_diarios[[z]]$month),mean)
        ord=order(match(as.numeric(prev$Mes),orden))
        esc_final_mensual_srad[z,]=t(prev)[-1,ord]
        
      }
      
      esc_final_mensual_srad=as.data.frame(esc_final_mensual_srad)
      colnames(esc_final_mensual_srad)=names(probabilidades)
      
      if(svalue(num_esc)==" "){promedio_escenarios_srad=esc_final_mensual_srad[2,] 
      }else{promedio_escenarios_srad=apply(esc_final_mensual_srad,2,mean)}
      
      srad_Multianual_Libertad1=aggregate(data_d$srad,list(Mes=data_d$month,Año=data_d$year),mean) #Se carga la precipitación mensual multianual 
      srad_Multianual_Libertad=aggregate(srad_Multianual_Libertad1[,3],list(Mes=srad_Multianual_Libertad1$Mes),mean)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
      
      tiff("Pronosticos/srad_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
      r=barplot(as.numeric(srad_Multianual_Libertad[,2]),names.arg=names(probabilidades),xpd = FALSE,col="cornsilk1",ylab="Radiación Solar (MJ*m2/mes)",,cex.lab=1,,main="Pronóstico Radiación Solar ",cex.main=0.8,ylim=c(min(srad_Multianual_Libertad[,2])-10,max(srad_Multianual_Libertad[,2])+10))
      
      for(m in 1:nrow(escenarios_final)){
        lines(r,as.numeric(esc_final_mensual_srad[m,]),ty="l",col="gray29")
      }
      
      lines(r,as.numeric(promedio_escenarios_srad),ty="l",col="red3",lwd=2)
      legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("khaki","gray29","red3"),cex=0.8)
      box()
      dev.off()
    }
    
    
  }
  
  
  print("Proceso finalizado exitosamente")
}
