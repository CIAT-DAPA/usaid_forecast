### Paquetes Necesarios 
library(raster) 
library(ggplot2)
library(grid)
library(rasterVis)
library(maptools)
library(rgeos)
library(gridExtra)
library(margins)
library(cowplot)

# Predictor del cfsv2 que se desea analizar: 
prec <- "SST" 

# Estas son las variables para las que se puede utilizar, no olvidar 
# utilizar los mismos nombres que estan aquí para que funcionen bien los codigos
# prec= c("SST" ,"U_wind_250","U_wind_850", "rhum_700", "vertical_vel_250")



### Directorio de trabajo
setwd("C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/")
getwd()

final_year<-2013 # año final del periodo de entrenamiento. 

ruta <- "C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/"




# dep_f se refiere a los nombres de los departamentos (o regiones) con los que 
# se hayan realizado las corridas de linux, se recomienda se usen lso mismos
# nombres de los archivos. 
dep_f<-c("casanare",    "cordoba",    "tolima",    "valle", "santander")
depL<-list("casanare", "cordoba", "santander", "tolima", "valle")


# Si el directorio de resultados y los departamentos ya fue creado, no es 
# necesario volver a crearlo. 

# Cree el directorio results donde se almacenan las carpetas de los resultados 
# de cada departamento.
if(dir.exists("results")==FALSE){dir.create("results")}
setwd(paste( getwd(),"/results/" ,sep=""))
print(getwd())





#### Declaración de las constantes

# nombre de los archivos de las variable predictoras, deben ser iguales
# que los que se ingreso en CPT. 
lead<- c("Aug", "Nov","Aug")
# mes de inicio de los trimestres
a<- c(9,12,12)
# Ancho del periodo de estudio para cada trimestre
length_periodo<- c(32,31,31)


# periodo
# solo puede tomar dos valores "Mes" o "bim"
per<-rep("bim",3)

# estos tres vectores deben ser de igual tamaño. 
cbind.data.frame(a, lead, length_periodo, per)

period <-0
for(i in 1:length(a)){
  if(per[i]=="bim"){
    period[i]<-if(a[i]<12){paste0(substr(month.abb[a[i]:(a[i]+1)],1,1), collapse = "")}else if(a[i]==12){period[i]<-paste(substr(month.abb[12],1,1), paste0(substr(month.abb[1],1,1), collapse = ""), sep="")}
  }else if(per=="Mes"){ period[i]<-paste0(substr(month.abb[a[i]],1,1))}
}





## Esta función devuelve las gráficas del goodness index, 
## ruta_c ruta donde se encuentran los archivos
## dep_f son los departamentos que se van tener encuenta en el archivo. 
GoodnessIndex <- function(ruta_c, dep_f, a, lead, period){
  
  good_index=0 # inicialice el vector
  GoodnessIndex=NA # inicialice el data frame
  for(j in 1:length(dep_f)) # realice esto para todos los departamentos
    for(i in 1:length(a)){ # barra para todos los periodos con sus respectivos lead time
      # Lea la tabla 
      goodnex<-read.table(paste(ruta_c,"/GoodnessIndex_",a[i],"_",lead[i],"_precip_",dep_f[j],".txt", sep=""),  skip=6, colClasses=c(rep("numeric",3),"character",rep("numeric",4)))
      # Cambie los nombres del archivo 
      names(goodnex)<-c("x1","y1","cca1","index1", "x2", "y2","cca2", "index2")
      # Encuentre el maximo
      goodnex$index2[dim(goodnex)[1]]==max(goodnex$index2)
      # Extraiga solo la fila donde estara el maximo y adicione
      # las columnas departamento, mes de inicio trimestre y lead time
      good_index<-cbind(dep_f[j], period[i], a[i] ,lead[i],goodnex[dim(goodnex)[1],5:8])
      # Almacene la fila en un data frame
      GoodnessIndex<-rbind(GoodnessIndex,good_index)
    }
  
  # Quite la primera fila ya que es nula
  GoodnessIndex<-GoodnessIndex[-1,]
  # Quite los nombres de las filas
  rownames(GoodnessIndex)=NULL
  # Cambie los nombres de las columnas
  names(GoodnessIndex)=c("Dep",  "Periodo","Mes_ini", "lead", "modos_x", "modos_y", "modos_cca", "GoodnessIndex")
  
  
  ####### Utilice como directorio principal el de resultados. 
  setwd(paste(ruta, "/results/", sep=""))
  ### Guarde todos los Goodness Index en un archivo .csv
  write.csv(x = GoodnessIndex, file = "GoodnessIndex.csv",sep = ",")
  # Retorne la tabla
  return(GoodnessIndex)}




## Corrida para todos los archivos
ruta_c
table<-GoodnessIndex(ruta_c = ruta_c, dep_f = dep_f, a = a, lead = lead, period= period)



ggplot(table, aes(x = Periodo, y= GoodnessIndex)) + geom_point(aes(colour=Periodo)) +
  facet_grid(~Dep) + theme_bw() + theme(legend.position = "none")+
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted")

ggsave("summary_GI.png", width = 7, height = 3)










###########################################################
###########################################################
###########################################################

# ind <- esta función permite crear para una corrida de CPT una tabla 
# que contenga un indicador especifico
# tipo = indicador que se desea analizar
# a <- mes de inicio del trimestre
# lead <- nombre del archivo de la variable predictora
# dep <-departamento a analizar
# ruta_c <- ruta donde se encuentran las salidas del CPT
ind<-function(tipo, a, lead, dep, ruta_c){
  
  datos<-NA # Creación de una trama de datos
  
  # lea los datos y apilelos en una tabla de datos
  for(j in 1:length(a)){
    names_below<-list.files(ruta_c, pattern = paste(tipo, "_", a[j], "_", lead[j], "_precip_", dep, sep=""))
    
    # Ciclo for que barre el número de departamentos que existan 
    # En caso de desear más departamentos modificar este argumento
    data_p<-read.table(paste(ruta_c,"/",names_below,sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric") )
    data_p<-data.frame(Trimestre=rep(a[j], dim(data_p)[1]), rep(lead[j], dim(data_p)[1]), data_p)
    datos<-rbind(datos, data_p) # Lectura de los datos y creación de la tabla global
  }
  
  # Depuración de la base de datos 
  datos<-datos[-1,] # Elimine la primera fila NA
  
  names(datos)<-c("Mes_ini", "lead", "Estacion", "Lat", "Lon",tipo) # Asignarle nombres a las columnas
  datos[,6]<-round(datos[,6],3) # Redondear el número de decimales de la curva
  datos<-datos[datos[,6]!=-999,] # Eliminar los datos faltantes
  
  # Asignación del nombre del trimestre
  Trim<-0
  for(i in 1:dim(datos)[1]){
    if(datos$Mes_ini[i]<11){
      Trim[i] <- paste0(substr(month.abb[datos$Mes_ini[i]:(datos$Mes_ini[i]+2)],1,1), collapse = "")
    }else if(datos$Mes_ini[i]==11){
      Trim[i] <-  paste(paste0(substr(month.abb[11:12],1,1), collapse = ""), substr(month.abb[1],1,1), sep="")
    }else if(datos$Mes_ini[i]==12){
      Trim[i] <- paste(substr(month.abb[12],1,1), paste0(substr(month.abb[1:2],1,1), collapse = ""), sep="")
    }
  }
  
  # cree un data frame con la columna trimestre y el data frame datos
  datos<-cbind.data.frame(Trimestre=Trim, datos)
  
  # entregue el data frame datos 
  return(datos)}

# summary_ind <- Esta funcion guarda automaticamente los mapas de indicadores
# y un archivo departamental resumen todos los indicadores. 
# dep <- departamento a analizar
# ruta_c <- ruta donde se encuentran las salidas del CPT
# a <- mes de inicio del trimestre
# lead <- nombre del archivo de la variable predictora
summary_ind<-function(dep,  ruta_c,  a, lead){
 
  # Indicadores a analizar
  tipo <- list("Pearsons_correlation", "Spearmans_correlation", "k_2AFC_Score",
               "Hit_Score", "Hit_Skill_Score", "LEPS_score", "Gerrity_Score",
               "k_2AFC_cat", "k_2AFC_cont", "ROC_below", "ROC_above")
  
  lista<-list()   # cree cree una lista
  for(i in 1:length(tipo)){
    lista[[i]] <- ind(tipo = tipo[[i]], a, lead, dep, ruta_c)
    names(lista)[i] <-tipo[[i]] 
  } # Aqui se repite la funcion ind para todos los indicadores, es decir
  # que se crea una trama de datos para cada indicador y se alamacena en una lista
  
  # Junte todos los indicadores en un solo data_frame
  summary<-Reduce(function(x, y) merge(x, y, all=TRUE), lista)
  
  
  # Ubiquese en el directorio de cada departamento en la carpeta results
  setwd(paste(ruta, "results/",  sep = ""))
  
  dir.create(paste(getwd(), "/Perfomance_Measures", sep=""))
  
  
  setwd(paste(ruta, "results/Perfomance_Measures",  sep = ""))
  
  
  # Guarde un archivo .csv con todos los indicadores
  write.csv(x = summary, file = paste("Perfomance_Measures_",dep,".csv", sep=""))
  }
  
 



cbind.data.frame(a,lead, period)

### Para correr la funcion para todas las regiones en caso que tenga 
sapply(depL, summary_ind, ruta_c,  a, lead, simplify = T)






for_files<-list()
for(i in 1:length(dep_f)){
  name<-list.files(paste(getwd(), "/" ,sep=""), pattern = "Perfomance_Measures_")
  for_files[[i]]<-read.csv(paste(getwd(), "/" , "/", name ,sep=""))
  for_files[[i]]<-cbind(dep= dep_f[i], for_files[[i]][,-1])
  names(for_files)[i]<-dep_f[i]
}



for_files <-Reduce(function(x, y) rbind.data.frame(x, y),for_files)

setwd(paste(ruta, "results/",  sep = ""))
write.csv(x = for_files, file = "Perfomance_Measures_.csv")









#################################################################
#################################################################
#################################################################


###### Forecast



#dep<- "casanare"
ruta_c





# ForecastP<-
# dep <- departamento a analizar
# ruta_c <- ruta donde se encuentran las salidas del CPT
# a <- mes de inicio del trimestre
# lead <- nombre del archivo de la variable predictora
ForecastP<-function(dep, ruta_c, a, lead){
  Total<-0 # Inicialice un objeto
  
  # Revisar como cambiar esta parte
  # Determinación de los limites departamentales y estaciones a dibular en el cap >.<
  if(dep=="casanare"){
    estaciones_in=data.frame(name="Yopal", Long=-72.388, Lat = 	5.320)
  }else if(dep=="cordoba"){
    estaciones_in=data.frame(name=c("Lorica","Cereté"), Long=c(-75.913,-75.802), Lat = c(9.302,8.840))
  }else if(dep=="tolima"){
    estaciones_in=data.frame(name=c("Ibagué","Espinal"), Long=c(-75.148,-74.960), Lat = c(4.430,4.188))
  }else if(dep=="valle"){
    estaciones_in=data.frame(name="La Unión", Long=-76.062, Lat = 4.531)
  }else if(dep=="santander"){
    estaciones_in=data.frame(name="Villanueva", Long=-73.21, Lat = 6.64)
  }
  
  Coord<-read.csv(paste(ruta,"deps.csv",sep=""), header=T, sep=",")  
  xmin<-Coord[1,dep]; xmax<-Coord[2,dep]; ymin<-Coord[3,dep]; ymax<-Coord[4,dep]
  
  
  # Corte colombia de acuerdo a las coordenadas asignadas
  col2=extent(xmin, xmax, ymin, ymax) # Coordenadas por departamento
  colombia_1=crop(colombia,col2) #  realizar el coorte
  colombia_1@data$id <- rownames(colombia_1@data) # cree una nueva variable en el shp
  colombia_1@data$id <- as.numeric(colombia_1@data$id) # digale que es de caracter númerico
  colombia2 <- fortify(colombia_1, region="id") # convierta el shp en una tabla de datos
  
  # Realice el gráfico de las correlaciones entre las estaciones y el modo 1 de y 
  p <- ggplot(colombia2, aes(x=long,y=lat)) # gráfique el país
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="snow")
  p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
  p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="black",size=0.3)
  
  for(i in 1:length(a)){ # Corra para todas las corridas
    
    # Lea el archivo de probabilidades de acuerdo a 
    w1<-read.table(paste(ruta_c, "/ForecastProbabilities_", a[i], "_", lead[i],"_precip_", dep,".txt", sep=""),sep="", skip =3, nrow=3, fill=TRUE)
    w2<-read.table(paste(ruta_c, "/ForecastProbabilities_", a[i], "_", lead[i],"_precip_", dep,".txt", sep=""),sep="", skip =8, nrow=3, fill=TRUE)
    w3<-read.table(paste(ruta_c, "/ForecastProbabilities_", a[i], "_", lead[i],"_precip_", dep,".txt", sep=""), sep="", skip =13, nrow=3, fill=TRUE)
    
    
    setwd(paste(ruta, "results/", dep, sep = ""))
    
    h1<-data.frame(t(Reduce(function(x, y) merge(x, y, all=TRUE), list(w1, w2, w3))))
    
    
    Trim<-0
    
    if(a[i]<11){
      Trim <- paste0(substr(month.abb[a[i]:(a[i]+2)],1,1), collapse = "")
    }else if(a[i]==11){
      Trim <-  paste(paste0(substr(month.abb[11:12],1,1), collapse = ""), substr(month.abb[1],1,1), sep="")
    }else if(a[i]==12){
      Trim <- paste(substr(month.abb[12],1,1), paste0(substr(month.abb[1:2],1,1), collapse = ""), sep="")
    }
    
    
    o<-cbind.data.frame(Trim, a[i], lead[i],rownames(h1), h1, row.names=NULL)
    names(o)<- c("Trimestre", "Mes_ini", "lead","Estaciones", "Lon", "Lat", "C1_below", "C2_Normal", "C3_Above")
    
    
    
    # Aqui se ingresan los datos de las estaciones
    below <- p + geom_point(data=o, aes(x=Lon, y=Lat, map_id= Estaciones,colour=C1_below),size=2.5)
    below <- below + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
      coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                            legend.text=element_text(size=8),
                            panel.background=element_rect(fill="white",colour="black"),
                            axis.text=element_text(colour="black",size=10),
                            axis.title=element_text(colour="black",size=10,face="bold"),
                            #legend.position = "bottom", 
                            legend.title=element_blank()) 
    # Aqui se colocan los nombres de las estaciones
    below <- below + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
    below <- below + labs(title="Below")
    
    
    # Aqui se ingresan los datos de las estaciones
    normal <- p + geom_point(data=o, aes(x=Lon, y=Lat, map_id= Estaciones,colour=C2_Normal),size=2.5)
    normal <- normal + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
      coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                            legend.text=element_text(size=8),
                            panel.background=element_rect(fill="white",colour="black"),
                            axis.text=element_text(colour="black",size=10),
                            axis.title=element_text(colour="black",size=10,face="bold"),
                            #legend.position = "bottom", 
                            legend.title=element_blank()) 
    # Aqui se colocan los nombres de las estaciones
    normal <- normal + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
    normal <- normal + labs(title="Normal")
    
    
    
    
    # Aqui se ingresan los datos de las estaciones
    above <- p + geom_point(data=o, aes(x=Lon, y=Lat, map_id= Estaciones,colour=C3_Above),size=2.5)
    above <- above + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
      coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                            legend.text=element_text(size=8),
                            panel.background=element_rect(fill="white",colour="black"),
                            axis.text=element_text(colour="black",size=10),
                            axis.title=element_text(colour="black",size=10,face="bold"),
                            #legend.position = "bottom", 
                            legend.title=element_blank()) 
    # Aqui se colocan los nombres de las estaciones
    above <- above + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
    above <- above + labs(title="Above")
    
    
    
    tiff(filename = paste("Prob_", dep, "_", Trim, "_",lead[i],".tif", sep=""), height=400,width=1280,res=100)
    print(grid.arrange(below, normal, above, ncol=3))
    dev.off()
    
    
    
    
    
    max_C<-apply(o[,7:9], 1, max)
    cat<-ifelse(o[,7]==max_C, "Below", ifelse(o[,8]==max_C, "Normal", ifelse(o[,9]==max_C, "Above",0)))
    maximos<-cbind.data.frame(o$Estaciones, o$Lon, o$Lat, max_C, cat)
    
    # Aqui se ingresan los datos de las estaciones
    maxi <- p + geom_point(data=maximos, aes(x=o$Lon, y=o$Lat, map_id= o$Estaciones,colour=max_C, shape=cat),size=2.5)
    maxi <- maxi + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
      coord_equal() + theme( legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                             legend.text=element_text(size=8),
                             panel.background=element_rect(fill="white",colour="black"),
                             axis.text=element_text(colour="black",size=10),
                             axis.title=element_text(colour="black",size=10,face="bold"),
                             #legend.position = "bottom", 
                             legend.title=element_blank()) 
    # Aqui se colocan los nombres de las estaciones
    maxi <- maxi + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
    maxi <- maxi + labs(title="Probabilistic Forecast")
    
    
    
    tiff(filename = paste("MaxiProb_", dep, "_", Trim, "_",lead[i],".tif", sep=""),  height=400,width=650,,res=100)
    print(maxi)
    dev.off()
    
    Total<-rbind(Total, o)
    print(i)
  }
  
  Total<-Total[-1,]
  
  write.csv(x =  Total, file = paste("ForecasProb_",dep,".csv", sep=""))
  return(Total)}


### Para todas las regiones en caso que tenga 
dep<-list("casanare", "cordoba", "santander", "tolima", "valle")
sapply(dep, ForecastP, ruta_c,  a, lead, simplify = T)






setwd(paste(ruta, "/results", sep=""))
getwd()


dep<-c("casanare", "cordoba", "santander", "tolima", "valle")

for_files<-list()
for(i in 1:length(dep)){
  name<-list.files(paste(getwd(), "/", dep[i] ,sep=""), pattern = "ForecasProb_")
  for_files[[i]]<-read.csv(paste(getwd(), "/", dep[i], "/", name ,sep=""))
  for_files[[i]]<-cbind(dep= dep[i], for_files[[i]][,-1])
  names(for_files)[i]<-dep[i]
}



for_files <-Reduce(function(x, y) rbind.data.frame(x, y),for_files)

write.csv(x = for_files, file = "Forecast_Prob.csv")


