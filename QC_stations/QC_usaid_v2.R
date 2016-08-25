
###########################################
#Script QC USAID###########################
#Date: July 19th 2016######################
#Lizeth Llanos#############################
###########################################
###########################################


#Lectura de datos desde el cluster 4 

variable <-"prec" #Definir variable como "prec" "tmax" "tmin" "srad" "rhum" "sbright"
rutOrigen = paste0("S:/observed/weather_station/col-ideam/daily-raw/",variable,"-per-station") #Ruta donde se encuentran los archivos .txt


files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files <-substring(files,1,nchar(files)-13)
nom.files <-as.numeric(nom.files[-length(nom.files)])

idstation <-read.table("clipboard",header=T) #Cargar base con código y nombre de la estación
CODIGO <-idstation[,1]

where <- match(CODIGO,nom.files)
station_find <-nom.files[where[which(!is.na(where))]]

############################################
data_station <- lapply(paste(rutOrigen,"/",station_find,"_raw_",variable,".txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

for (i in 1:length(data_station)){
  data_station[[i]][,2] = as.numeric(sub(",", ".", data_station[[i]][,2], fixed = TRUE))
}

#Modificar graficos de control poner antes y despues
setwd("D:\\Tobackup\\CIAT\\USAID\\Data_clima\\data_qc")
vref <-read.csv("vref_QC.csv",header = T)
vref <-vref[vref$var==variable,]


#QC rango de referencia
QC_rango = function(data_station,vref,variable=variable){
  data = list()
  #23 20
#   i=20
#   
#   dates=seq(as.Date("1977/12/1"), as.Date("2014/12/31"), "days") #Definir periodo que se desea analizar
#   head(data_station[[i]])
#   
#   
#   #for(i in 1 :30){
#     plot(dates[1:nrow(data_station[[i]])],data_station[[i]][,2],type="b",main=paste(i),ylim=c(as.numeric(vref[2])+1,as.numeric(vref[3])+1),ylab = "Temperatura máxima",xlab="")
#     
#   #}
#    abline(h=vref[2],col="red",lwd=2)
#   abline(h=vref[3],col="red",lwd=2)
  
  for (i in 1:length(data_station)){
        vref.na=which(data_station[[i]][,2] < vref[2] | data_station[[i]][,2] >vref[3])
        data_station[[i]][vref.na,2] = NA
        data[[i+1]] = vref.na
        print(i)
      }
   
   data[[1]] = data_station
   
   print("QC_rangos OK")
  return(data)
}

#QC datos consecutivos
QC_consec = function(data_station,vref){
  data = list()
  
  for (i in 1:length(data_station)){
    
    x <-data.frame(hasta=cumsum(rle(data_station[[i]][,2])$lengths),cant_iguales=rle(data_station[[i]][,2])$lengths,valor=rle(data_station[[i]][,2])$values)
    x$desde = x$hasta -x$cant_iguales + 1  
    x <-na.omit(x)
    if (variable== "prec") { #Si la variable es precipitación se omiten los 0
      x = x[x$valor>0,]
    }
    
    
    error = x[x$cant_iguales>vref[4],] 
    if (nrow(error)>0){
      for (k in 1:nrow(error)) {
        if (k==1){vref.na<-error$desde[k]:error$hasta[k]}
        else{vref.na<-c(vref.na,error$desde[k]:error$hasta[k])}
      }
      data_station[[i]][vref.na,2] = NA
      data[[i+1]] = vref.na
      
    }
    print(i)
          }
  
  data[[1]] = data_station
  
  print("QC_consec OK")
  
  return(data)
}

#QC datos con saltos
QC_saltos = function(data_station,vref){
  data = list()
  for (i in 1:length(data_station)){
    if(variable!="prec"){
      vref.na=which(abs(diff(data_station[[i]][,2]))>=vref[5])
      vref.na=unique(sort(c(vref.na,vref.na+1)))
     data_station[[i]][vref.na,2] = NA
      data[[i+1]] = vref.na
      
    }
      }
  
  data[[1]] = data_station
  print("QC_saltos OK")
  
  return(data)
}

#QC tmax<tmin
QC_temp = function(data_station,vref){
  data = list()
    vref.na=which(data_station[,4] < data_station[,5])
    data_station[vref.na,c(4,5)] = NA
 
    
    data[[1]] = data_station
    data[[2]] = vref.na
    print("QC_temp OK")
    
    return(data)
}

#QC identificación de datos atípicos
QC_out = function(data_station,vref){
 
  data = list()
  
  for (i in 1:length(data_station)){
    x = data_station[[i]][,2]
    year = substring(data_station[[i]][,1],1,4)
    month = month.abb[as.numeric(substring(data_station[[i]][,1],5,6))]
    month = factor(month, levels=month.abb)
    
    if (variable== "prec") { #Si la variable es precipitación se omiten los 0
      x[x<=0]= NA
    }
    
    val = boxplot(x~month,range=as.numeric(vref[6]),plot=F)
    
    for(j in 1:12){
      data_station[[i]]$lim_inf[month==month.abb[j]]=val$stats[1,j]
      data_station[[i]]$lim_sup[month==month.abb[j]]=val$stats[5,j]
      
    }
    
    out_m = which(x > data_station$lim_sup || x < data_station$lim_inf)  
   
    if(variable!="prec"){
      
      tiff("box_month.tiff",compression = 'lzw',height = 5,width = 15,units="in", res=200)
      par(mfrow=c(2,1),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 1.5)
      boxplot(x~month,range=as.numeric(vref[6]),plot=T)
      
      plot(dates[1:nrow(data_station[[i]])],x,type="l")
      lines(dates[1:nrow(data_station[[i]])],data_station[[i]]$lim_inf,col="red",lty=2)
      lines(dates[1:nrow(data_station[[i]])],data_station[[i]]$lim_sup,col="red",lty=2)
      dev.off()
      
    }
       
    
    val_year = boxplot(x~year,range=as.numeric(vref[6]),plot=F)
    
    year_n = unique(year)
    for(k in 1:length(year_n)){
      data_station[[i]]$lim_inf_y[year==year_n[k]]=val_year$stats[1,k]
      data_station[[i]]$lim_sup_y[year==year_n[k]]=val_year$stats[5,k]
      
    }
    
    out_y = which(x > data_station[[i]]$lim_sup_y || x < data_station[[i]]$lim_inf_y)  
    
    tiff("box_year.tiff",compression = 'lzw',height = 5,width = 15,units="in", res=200)
        dev.off()
    
    
      data[[i+1]] = c(out_y,out_m)
    
  }
  
  print("QC_out OK")
  
   return(data)
  
  }
    
 
QC_all = function(data,vref){
  QC1 = QC_rango(data_station,vref) #QC rango de valores por fuera reemplaza por NA
  QC2 = QC_consec(QC1[[1]],vref) #QC valores iguales consecutivos reemplaza por NA
  QC3 = QC_saltos(QC2[[1]],vref) #QC valores con saltos consecutivos reemplaza por NA
  QC4 = QC_temp(QC3[[1]],vref) #QC tmax<tmin reemplaza por NA
  
  QC_final = QC_out(QC4[[1]],vref)
  
  tmax_qc = cbind(data$tmax, QC4[[1]]$tmax,QC4[[2]])
  
  dir.create("Temp. Máx",showWarnings = F)
  
  return(QC_final)
} 

xx=QC_all(data_station,vref)


# 
