#getwd() #Se obtiene el directorio de trabajo
#setwd("C:/PRONOSTICOS/IBAGUE/")# Se cambia el directorio de trabajo

require(lubridate)
require(reshape)
require(ggplot2)
require(gWidgets) #Paquete para generar interfaz grafica
require(gWidgetsRGtk2) #Paquete para generar interfaz grafica

##########################################
##########Interfaz gráfica
##########################################

win <- gwindow("RClimTool 2.0", visible=F ,width = 600) #Crea ventana inicial
nb = gnotebook(cont=win,expand=T,tab.pos = 2)



lyt6=glayout(homogeneous =T,cont=nb,spacing=2,label="9. Pronósticos Agroclimáticos",expand=T) 

lyt6[1,1]=(gg6=gframe("Parámetros de entrada",cont=lyt6,horizontal=F))
lty.33=glayout(homogeneous =F,cont=gg6,spacing=2,expand=T) 

lty.33[1,1]=glabel(" ",cont=lty.33)


# lty.33[2,1]=glabel("-Estación No. :",cont=lty.33)
# lty.33[2,2]=(est_p=gedit("",cont=lty.33,width =5))


#Fechas=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre") # Crea una tabla con los nombres de los meses
lty.33[4,1]=glabel("-Cargar datos (opcional):",cont=lty.33)
lty.33[4,2]=gbutton("Diarios",cont=lty.33,handler=function(h,...) cargar_diarios())
lty.33[4,3]=gbutton("Mensuales",cont=lty.33,handler=function(h,...) cargar_mensual())

lty.33[5,1]=glabel(" ",cont=lty.33)

lty.33[6,1]=glabel("-Seleccione la variable:",cont=lty.33)
lty.33[6,2]=(val_p=gdroplist(c("tmax","tmin","precip","srad"),cont=lty.33))
#lty.33[3,3]=gdroplist(c("Hasta",Fechas),cont=lty.33)
lty.33[7,1]=glabel("-Cargar tabla probabilidades:",cont=lty.33)
lty.33[7,2]=gbutton("Probabilidades",cont=lty.33,handler=function(h,...) cargar_prob())


lty.33[8,1]=glabel("-No. de escenarios a simular:",cont=lty.33)
lty.33[8,2]=(num_esc=gedit(" ",cont=lty.33,width=3))


lty.33[9,1]=glabel("-Generar pronósticos:",cont=lty.33)
lty.33[9,2]=gbutton("Pronosticar",cont=lty.33,handler=function(h,...) pronosticos())



lyt6[2,1]=(gg.6=gframe("Parámetros estacion",cont=lyt6,horizontal=F))
lty.3.3=glayout(homogeneous =F,cont=gg.6,spacing=2,expand=T) 
#lty.3.3[1,1]=glabel("-IDEAM: www.ideam.gov.co/",cont=lty.3.3)
lty.3.3[1,1]=glabel("-Longitud:",cont=lty.3.3)
lty.3.3[1,2]=(long=gedit("",cont=lty.3.3,width=5))
lty.3.3[2,1]=glabel("-Latitud:",cont=lty.3.3)
lty.3.3[2,2]=(lat=gedit("",cont=lty.3.3,width=5))
lty.3.3[3,1]=glabel("-Altura:",cont=lty.3.3)
lty.3.3[3,2]=(alt=gedit("",cont=lty.3.3,width=5))
lty.3.3[4,1]=glabel("-XX:",cont=lty.3.3)
lty.3.3[4,2]=(xx=gedit("0",cont=lty.3.3,width=5))
lty.3.3[5,1]=glabel("-YY:",cont=lty.3.3)
lty.3.3[5,2]=(yy=gedit("0",cont=lty.3.3,width=5))


visible(win) = T
focus(win)

##########################################
#######Funciones
##########################################
cargar_diarios=function(){
  data_d=read.csv(gfile("Seleccione un archivo"),header=T,dec=".")
  assign("data_d",data_d,.GlobalEnv)
}

cargar_mensual=function(){
  data_m=read.csv(gfile("Seleccione un archivo"),header=T,dec=".")
  assign("data_m",data_m,.GlobalEnv)
}

cargar_prob=function(){
  probabilidades=read.csv(gfile("Seleccione un archivo"),header=T,dec=".")
  assign("probabilidades",probabilidades,.GlobalEnv)
}

confirmDialog <- function(message,title, handler=NULL) { #Funcion para cuadro de diálogo
  
  window <- gwindow(title,width=100,height=100)
  group <- ggroup(container = window)
  gimage("info", dirname="stock", size="large_toolbar", container=group)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group, expand=TRUE)
  
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Ok", handler = function(h,...) dispose(window),
          container=button.group)
  
  return()
}

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

pronosticos=function(){

  ######################################################### DESDE AQUÍ #####################################
  ##########################################################################################################
  
  if(ncol(probabilidades)>6) stop("El número de meses a pronosticar es mayor a 6, por favor verifique el archivo con las probabilidades")
  
  confirmDialog(paste("Los resultados se guardarán en",getwd(),"/Pronosticos"),"Ubicación archivos")
  
  dir.create("Pronosticos",showWarnings=F)
  Fechas=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre") # Crea una tabla con los nombres de los meses
  
  #---------------------------------------------------------------------------------#
  #-------------------------------Lectura de datos----------------------------------#
  #---------------------------------------------------------------------------------#
  
  
  if(exists("data_d") ){
    
    attach(data_d,warn.conflicts =F)
    
    if(svalue(val_p)=="tmax"){data=aggregate(as.numeric(tmax),list(data_d$year,data_d$month),mean)}
    if(svalue(val_p)=="tmin"){data=aggregate(as.numeric(tmin),list(data_d$year,data_d$month),mean)}
    if(svalue(val_p)=="precip"){data=aggregate(as.numeric(precip),list(data_d$year,data_d$month),sum)}
    if(svalue(val_p)=="srad"){data=aggregate(as.numeric(srad),list(data_d$year,data_d$month),mean)}
    
    names(data)=c("year","month","value")
  }
  
  
  
  if(exists("data_m")){
    
    attach(data_m,warn.conflicts =F)
    
    data=data_m[,c(2,1,which(names(data_m)==svalue(val_p)))]
    
  }
  
  probabilidades=as.data.frame(probabilidades)
  
  
  #---------------------------------------------------------------------------------#
  #-----------------Ordenar de menor a mayor datos mensuales históricos-------------#
  #---------------------------------------------------------------------------------#
  
  Enero1=data[data$month==1,-2]
  Febrero1=data[data$month==2,-2]
  Marzo1=data[data$month==3,-2]
  Abril1=data[data$month==4,-2]
  Mayo1=data[data$month==5,-2]
  Junio1=data[data$month==6,-2]
  Julio1=data[data$month==7,-2]
  Agosto1=data[data$month==8,-2]
  Septiembre1=data[data$month==9,-2]
  Octubre1=data[data$month==10,-2]
  Noviembre1=data[data$month==11,-2]
  Diciembre1=data[data$month==12,-2]
  
  
  Enero1=Enero1[order(Enero1[,2]),]
  Febrero1=Febrero1[order(Febrero1[,2]),]
  Marzo1=Marzo1[order(Marzo1[,2]),]
  Abril1=Abril1[order(Abril1[,2]),]
  Mayo1=Mayo1[order(Mayo1[,2]),]
  Junio1=Junio1[order(Junio1[,2]),]
  Julio1=Julio1[order(Julio1[,2]),]
  Agosto1=Agosto1[order(Agosto1[,2]),]
  Septiembre1=Septiembre1[order(Septiembre1[,2]),]
  Octubre1=Octubre1[order(Octubre1[,2]),]
  Noviembre1=Noviembre1[order(Noviembre1[,2]),]
  Diciembre1=Diciembre1[order(Diciembre1[,2]),]
  
  
  var_org=cbind(Enero1[,2],Febrero1[,2],Marzo1[,2],Abril1[,2],Mayo1[,2],Junio1[,2],Julio1[,2],Agosto1[,2],Septiembre1[,2],Octubre1[,2],Noviembre1[,2],Diciembre1[,2])
  colnames(var_org)=Fechas
  
  Años_org=cbind(Enero1[,1],Febrero1[,1],Marzo1[,1],Abril1[,1],Mayo1[,1],Junio1[,1],Julio1[,1],Agosto1[,1],Septiembre1[,1],Octubre1[,1],Noviembre1[,1],Diciembre1[,1])
  colnames(Años_org)=Fechas
  
  Años_org2=Años_org[,match(names(probabilidades),colnames(Años_org))]
  var_org2=var_org[,match(names(probabilidades),colnames(var_org))]
  
  #---------------------------------------------------------------------------------#
  #---------------------------Cálculo de percentiles--------------------------------#
  #---------------------------------------------------------------------------------#
  
  percentiles=rbind(apply(var_org,2,FUN=quantile,0.3333),apply(var_org,2,FUN=quantile,0.6666),apply(var_org,2,FUN=quantile,0.9999))
  
  if(svalue(val_p)=="precip"){ nomb_prob=c("Déficit","Normal","Exceso")
  }else nomb_prob=c("Disminución","Normal","Aumento")
  
  rownames(percentiles)=nomb_prob
  colnames(percentiles)=Fechas
  
  #Salida terciles de los datos históricos
  write.csv(percentiles,file="Pronosticos/percentiles.csv")
  
  #---------------------------------------------------------------------------------#
  #---------------------------Gráfica de probabilidades-----------------------------#
  #---------------------------------------------------------------------------------#
  
  o=melt(probabilidades)
  o1=cbind(o,"cond"=rep(nomb_prob,ncol(probabilidades)))
  
  p=ggplot(o1,aes(x=variable,y=value))+geom_bar(stat="identity", position="dodge",aes(fill=cond,alpha=0.7))+
    scale_fill_manual(values=c( "firebrick3","dodgerblue2","chartreuse3"),name=paste(svalue(val_p)), breaks=nomb_prob,labels=nomb_prob)+
    ylab("Probabilidad de pronóstico (%)")+xlab("")+scale_alpha(guide = 'none')+ylim(0,100)
  
  ggsave("Pronosticos/probabilidades.tiff",p,width=7,height=3)
  
  
  #---------------------------------------------------------------------------------#
  #-------------------Función para generar años analogos----------------------------#
  #---------------------------------------------------------------------------------#
  
  insumo=function(data,prob,añoshistorico){
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
  #--------------Generación de los 12 años análogos mas probables-------------------#
  #---------------------------------------------------------------------------------#
  
  masprobable=matrix(0,nrow=500,ncol=dim(probabilidades)[2])
  
  
  masprobable=sapply_pb(1:500,function(j){esc1=sapply(1:dim(probabilidades)[2], function(i) insumo(var_org2[,i],probabilidades[,i],Años_org2[,i]))
                                          masprobable[j,]=sapply(1:dim(probabilidades)[2],function(j) as.numeric(rownames(cbind(which(table(esc1[,j])==max(table(esc1[,j]))))))[1])})    
  
  #masprobable2=apply(t(masprobable),2,function(masprobable)sample(masprobable,12,rep=F))
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
  
  todo=sapply(1:dim(probabilidades)[2], function(i) valores(masprobable2[,i],var_org2[,i],Años_org2[,i]))
  todo2=as.data.frame(rbind(masprobable2,c("Datos análogos",rep("",dim(probabilidades)[2]-1)),todo)) ###Años y datos analogos
  colnames(todo2)=names(probabilidades)
  
  resumen=function(x) rbind(min(x),max(x))
  resumen2=apply(todo,2,resumen)
  
  if(svalue(val_p)=="precip") {medias=apply(todo,2,median)}else{medias=apply(todo,2,mean)}
  
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
  row.names(resumen3)=c("Mín","Promedio","Máx")
  
  resumenf=rbind(resumen3,c("Años",rep("",dim(probabilidades)[2]-1)),todo3) ###Resumen con min max y prom de los escenarios analogos
  colnames(resumenf)=names(probabilidades)
  
  ###Salida de años analogos y resumen
  write.csv(todo2,file="Pronosticos/añosanalogos.csv")
  write.csv(resumenf,file="Pronosticos/resumen_añosanalogos.csv")
  
  #---------------------------------------------------------------------------------#
  #----------Generación de todos los escenarios definidos por el usuario------------#
  #---------------------------------------------------------------------------------#
  num_esc1=as.numeric(svalue(num_esc))
  
  a=masprobable2
  a=as.data.frame(a)
  names(a)=names(probabilidades)
  
  if(!is.na(num_esc1)){
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
        if(any(names(a)=="Enero")){ escenario_Ene[[w]]=sample(a$Enero,1)},
        if(any(names(a)=="Febrero")){ escenario_Feb[[w]]=sample(a$Febrero,1)},
        if(any(names(a)=="Marzo")){ escenario_Mar[[w]]=sample(a$Marzo,1)},
        
        if(any(names(a)=="Abril")){ escenario_Abr[[w]]=sample(a$Abril,1)},
        if(any(names(a)=="Mayo")){escenario_May[[w]]=sample(a$Mayo,1)},
        if(any(names(a)=="Junio")){escenario_Jun[[w]]=sample(a$Junio,1)},
        if(any(names(a)=="Julio")){escenario_Jul[[w]]=sample(a$Julio,1)},
        if(any(names(a)=="Agosto")){ escenario_Ago[[w]]=sample(a$Agosto,1)},
        if(any(names(a)=="Septiembre")){escenario_Sep[[w]]=sample(a$Septiembre,1)},
        if(any(names(a)=="Octubre")){escenario_Oct[[w]]=sample(a$Octubre,1)},
        if(any(names(a)=="Noviembre")){escenario_Nov[[w]]=sample(a$Noviembre,1)},
        if(any(names(a)=="Diciembre")){escenario_Dic[[w]]=sample(a$Diciembre,1)})
      
    }
    
    escenarios_final1=do.call("rbind",esc_consolidado)
    
    orden=match(names(probabilidades),colnames(Años_org))
    ord_col=order(match(sort(orden),orden))
    
    escenarios_final=rbind(escenarios_final1[,ord_col],todo3)
    nom=c(seq(1,num_esc1),"min","prom","max")
    
  }else{escenarios_final=todo3
        nom=c("min","prom","max")}
  
  orden=match(names(probabilidades),colnames(Años_org))
  
  
  escenarios_final=as.data.frame(escenarios_final)
  names(escenarios_final)=names(probabilidades)
  
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
    dir.create("Pronosticos/esc_oryza",showWarnings = F)
    filas=unlist(lapply(esc_final_diarios,nrow))
    filas.1=which(filas==182)
      
    encab=paste(svalue(long),svalue(lat),svalue(alt),svalue(xx),svalue(yy),sep=",")
    for (H in 1:length(filas.1)){
      codigo=matrix(H,182)#crea vector con el codigo de la estacion
      
      
      if(filas[H] == min(unique(filas))){
        ano=as.matrix(ano.ini)
        juliano=matrix(julian.date,182) #Colocar dias julianos desde donde inicia hasta donde termina la serie
        
      }else{
        ano=as.matrix(ano.ini.b)
        juliano=matrix(julian.date.b,182)
      }
          
      SRAD=((esc_final_diarios[[filas.1[H]]]$srad)*1000)
      TMIN=esc_final_diarios[[filas.1[H]]]$tmin
      TMAX=esc_final_diarios[[filas.1[H]]]$tmax
      viento=matrix(-99,filas[filas.1[H]])
      RAIN=esc_final_diarios[[filas.1[H]]]$precip
      
      
      
      if(length(unique(ano))==1){
        clima=cbind(codigo,ano,juliano,SRAD,TMIN,TMAX,viento,viento,RAIN)
        
        b=paste("Pronosticos/esc_oryza/IBTO",nom[H],".",substring(unique(ano),2,4),sep="")  #Modificar nombre de archivos (4 caracteres)
        sink(b)
        cat(encab) #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
        cat("\n")
        write.table(clima,sep=",",row.names=F,col.names=F)
        sink()
        
      }else{
        clima=cbind(codigo,ano,juliano,SRAD,TMIN,TMAX,viento,viento,RAIN)
        clima.ini =  clima[clima[,2]==unique(ano)[1],]
        clima.fin =  clima[clima[,2]==unique(ano)[2],]
        
        b=paste("Pronosticos/esc_oryza/IBTO",nom[H],".",substring(clima.ini[1,2],2,4),sep="")  #Modificar nombre de archivos (4 caracteres)
        b.1=paste("Pronosticos/esc_oryza/IBTO",nom[H],".",substring(clima.fin[1,2],2,4),sep="")  #Modificar nombre de archivos (4 caracteres)
        
        sink(b)
        cat(encab) #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
        cat("\n")
        write.table(clima.ini,sep=",",row.names=F,col.names=F)
        sink()
        
        sink(b.1)
        cat(encab) #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
        cat("\n")
        write.table(clima.fin,sep=",",row.names=F,col.names=F)
        sink()
        
      }
      
#       clima=cbind(codigo,ano,juliano,SRAD,TMIN,TMAX,viento,viento,RAIN)
#       
#       b=paste("Pronosticos/esc_oryza/IBTO",nom[H],".","016",sep="")  #Modificar nombre de archivos (4 caracteres)
#       sink(b)
#       cat("-75.087,4.42,750,0,0") #Modificar coordenadas SDTO -74.98,3.917,305,0,0--AIHU -74.767,3.250,380,0,0--MRCO -74.15,8.8,14,0,0-- YOCS -72.15,5.15,250,0,0---VVME -72.53,4.03,315,0,0-- IBTO -75.087, 4.42, 750,0,0
#       cat("\n")
#       write.table(clima,sep=",",row.names=F,col.names=F)
#       sink()
#       
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
  
  
  
  #---------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------#
  #----------------------Creación de escenarios a nivel mensual---------------------#
  #---------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------#
  
  
  if(exists("data_m")){
    esc_final_mensual=list()
    
    if(any(names(a)=="Enero")){ esc_mensual_Ene=list()}
    if(any(names(a)=="Febrero")){ esc_mensual_Feb=list()}
    if(any(names(a)=="Marzo")){ esc_mensual_Mar=list()}
    if(any(names(a)=="Abril")){ esc_mensual_Abr=list()}
    if(any(names(a)=="Mayo")){ esc_mensual_May=list()}
    if(any(names(a)=="Junio")){ esc_mensual_Jun=list()}
    if(any(names(a)=="Julio")){ esc_mensual_Jul=list()}
    if(any(names(a)=="Agosto")){ esc_mensual_Ago=list()}
    if(any(names(a)=="Septiembre")){ esc_mensual_Sep=list()}
    if(any(names(a)=="Octubre")){ esc_mensual_Oct=list()}
    if(any(names(a)=="Noviembre")){ esc_mensual_Nov=list()}
    if(any(names(a)=="Diciembre")){ esc_mensual_Dic=list()}
    
    for (n in 1:nrow(escenarios_final)){
      
      esc_final_mensual[[n]]=rbind(
        if(any(names(a)=="Enero")){esc_mensual_Ene[[n]]=data_m[data_m$month=="1"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Enero")],]},
        if(any(names(a)=="Febrero")){esc_mensual_Feb[[n]]=data_m[data_m$month=="2"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Febrero")],]},
        if(any(names(a)=="Marzo")){esc_mensual_Mar[[n]]=data_m[data_m$month=="3"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Marzo")],]},
        if(any(names(a)=="Abril")){esc_mensual_Abr[[n]]=data_m[data_m$month=="4"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Abril")],]},
        if(any(names(a)=="Mayo")){esc_mensual_May[[n]]=data_m[data_m$month=="5"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Mayo")],]},
        if(any(names(a)=="Junio")){esc_mensual_Jun[[n]]=data_m[data_m$month=="6"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Junio")],]},
        if(any(names(a)=="Julio")){esc_mensual_Jul[[n]]=data_m[data_m$month=="7"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Julio")],]},
        if(any(names(a)=="Agosto")){esc_mensual_Ago[[n]]=data_m[data_m$month=="8"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Agosto")],]},
        if(any(names(a)=="Septiembre")){esc_mensual_Sep[[n]]=data_m[data_m$month=="9"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Septiembre")],]},
        if(any(names(a)=="Octubre")){esc_mensual_Oct[[n]]=data_m[data_m$month=="10"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Octubre")],]},
        if(any(names(a)=="Noviembre")){esc_mensual_Nov[[n]]=data_m[data_m$month=="11"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Noviembre")],]},
        if(any(names(a)=="Diciembre")){ esc_mensual_Dic[[n]]=data_m[data_m$month=="12"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Diciembre")],]})
      
      
    }
    
    
    for (n in 1:nrow(escenarios_final)){
      ord=order(match(as.numeric(esc_final_mensual[[n]]$month),orden))
      
      esc_final_mensual[[n]]=esc_final_mensual[[n]][ord,]
    }
    
    
    #---------------------------------------------------------------------------------#
    #-----------------Exporta los escenarios a nivel mensual a .csv-------------------#
    #---------------------------------------------------------------------------------#
    
    dir.create("Pronosticos/Escenarios",showWarnings=F)
    for(k in 1:nrow(escenarios_final)){
      
      j=paste("Pronosticos/Escenarios/escenario_",nom[k],".csv",sep="") 
      
      write.csv(esc_final_mensual[[k]],j,row.names=F)
    }
    
    
    #---------------------------------------------------------------------------------#
    #---------------------------Gráficos Precipitación--------------------------------#
    #---------------------------------------------------------------------------------#
    
    esc_final_mensual_Lluvia=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_Lluvia[z,]=rbind(esc_final_mensual[[z]]$precip)
    }
    
    colnames(esc_final_mensual_Lluvia)=names(probabilidades)
    
    if(svalue(num_esc)==" "){promedio_escenarios_Lluvia=esc_final_mensual_Lluvia[2,] 
    }else{promedio_escenarios_Lluvia=apply(esc_final_mensual_Lluvia,2,mean)}
    
    Multianual_Libertad=aggregate(data_m$precip,list(Mes=data_m$month),mean)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/precip_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    a=barplot(as.numeric(Multianual_Libertad[,2]),names.arg=names(probabilidades),col="Slate Gray 2",ylab="Precipitacion (mm)",,cex.lab=1,main="Pronóstico precipitación",cex.main=0.8,ylim=c(0,max(Multianual_Libertad[,2])+200))
    
    for(m in 1:nrow(escenarios_final)){
      lines(a,as.numeric(esc_final_mensual_Lluvia[m,]),col="gray29",lty=1,lwd=1)
    }
    
    lines(a,as.numeric(promedio_escenarios_Lluvia),col="red3",lwd=2)
    legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray29","red3"),cex=0.8)
    box()
    dev.off()
    
    #---------------------------------------------------------------------------------#
    #---------------------------Gráficos TMAX-----------------------------------------#
    #---------------------------------------------------------------------------------#
    
    esc_final_mensual_tmax=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_tmax[z,]=rbind(esc_final_mensual[[z]]$tmax)  }
    
    esc_final_mensual_Tmax=as.data.frame(esc_final_mensual_tmax)
    colnames(esc_final_mensual_Tmax)=names(probabilidades)
    
    if(svalue(num_esc)==" "){promedio_escenarios_Tmax=esc_final_mensual_Tmax[2,] 
    }else{promedio_escenarios_Tmax=apply(esc_final_mensual_Tmax,2,mean)}
    
    Tmax_Multianual_Libertad=aggregate(data_m$tmax,list(Mes=data_m$month),mean)[match(names(probabilidades),colnames(Años_org)),] #Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmax_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    r=barplot(as.numeric(Tmax_Multianual_Libertad[,2]),names.arg=names(probabilidades),xpd = FALSE,col="cornsilk1",ylab="Temperatura Máxima (°C)",,cex.lab=1,,main="Pronóstico Temperatura Máxima ",cex.main=0.8,ylim=c(min(data_m$tmax)-2,max(data_m$tmax)+1))
    
    for(m in 1:nrow(escenarios_final)){
      lines(r,as.numeric(esc_final_mensual_Tmax[m,]),ty="l",col="gray29")
    }
    
    lines(a,as.numeric(promedio_escenarios_Tmax),ty="l",col="red3",lwd=2)
    legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("khaki","gray29","red3"),cex=0.8)
    box()
    dev.off()
    
    #---------------------------------------------------------------------------------#
    #----------------------------------Gráficos TMIN----------------------------------#
    #---------------------------------------------------------------------------------#
    
    esc_final_mensual_tmin=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_tmin[z,]=rbind(esc_final_mensual[[z]]$tmin)
    }
    esc_final_mensual_Tmin=as.data.frame(esc_final_mensual_tmin)
    colnames(esc_final_mensual_Tmin)=names(probabilidades)
    
    if(svalue(num_esc)==" "){promedio_escenarios_tmin=esc_final_mensual_tmin[2,] 
    }else{promedio_escenarios_tmin=apply(esc_final_mensual_tmin,2,mean)}
    
    Tmin_Multianual_Libertad=aggregate(data_m$tmin,list(Mes=data_m$month),mean)[match(names(probabilidades),colnames(Años_org)),] #Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmin_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    r=barplot(as.numeric(Tmin_Multianual_Libertad[,2]),names.arg=names(probabilidades),ylim=c(min(data_m$tmin)-2,max(data_m$tmin)+2),xpd = FALSE,col="cornsilk1",ylab="Temperatura Mínima (°C)",cex.lab=1,main="Pronóstico Temperatura Mínima ",cex.main=0.8)
    
    for(m in 1:nrow(escenarios_final)){
      lines(r,as.numeric(esc_final_mensual_Tmin[m,]),ty="l",col="gray29")
    }
    
    lines(a,as.numeric(promedio_escenarios_tmin),ty="l",col="red3",lwd=2)
    legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("khaki","gray29","red3"),cex=0.8)
    box()
    dev.off()
    
    
    #---------------------------------------------------------------------------------#
    #---------------------------Gráficos Radiacion Solar------------------------------#
    #---------------------------------------------------------------------------------#
    
    if(exists("srad")){
      
      esc_final_mensual_srad=matrix(0,nrow(escenarios_final),ncol(probabilidades))
      for(z in 1:nrow(escenarios_final)){
        esc_final_mensual_srad[z,]=rbind(esc_final_mensual[[z]]$srad)
      }
      esc_final_mensual_Tmin=as.data.frame(esc_final_mensual_srad)
      colnames(esc_final_mensual_srad)=names(probabilidades)
      
      if(svalue(num_esc)==" "){promedio_escenarios_srad=esc_final_mensual_srad[2,] 
      }else{promedio_escenarios_srad=apply(esc_final_mensual_srad,2,mean)}
      
      srad_Multianual_Libertad=aggregate(data_m$srad,list(Mes=data_m$month),mean)[match(names(probabilidades),colnames(Años_org)),] #Se carga la precipitación mensual multianual 
      
      tiff("Pronosticos/srad_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
      r=barplot(as.numeric(srad_Multianual_Libertad[,2]),names.arg=names(probabilidades),ylim=c(min(data_m$srad)-5,max(data_m$srad)+5),xpd = FALSE,col="cornsilk1",ylab="Radiación Solar  (MJ*m2/mes)",cex.lab=1,main="Pronóstico Radiación Solar",cex.main=0.8)
      
      for(m in 1:nrow(escenarios_final)){
        lines(r,as.numeric(esc_final_mensual_srad[m,]),ty="l",col="gray29")
      }
      
      lines(a,as.numeric(promedio_escenarios_srad),ty="l",col="red3",lwd=2)
      legend("topright",c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("khaki","gray29","red3"),cex=0.8)
      box()
      dev.off()
    }
  }
  ######################################################### HASTA AQUÍ #####################################
  ##########################################################################################################
print("Proceso finalizado exitosamente")
  }
