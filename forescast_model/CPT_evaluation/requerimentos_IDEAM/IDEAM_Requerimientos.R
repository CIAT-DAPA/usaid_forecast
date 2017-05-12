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


### Directorio de trabajo
setwd("C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/") # Modificar
getwd()


ruta <- "C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/" # Modificar

### Lectura del shp de colombia. # Modificar
colombia<-shapefile("C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/Regimen pluviometrico/regimen_pluviometrico.shp")


# dep_f se refiere a los nombres de los departamentos (o regiones) con los que 
# se hayan realizado las corridas de linux, se recomienda se usen lso mismos
# nombres de los archivos. 
# list.files(paste(getwd(), "/dep/", sep=""))

dep_f<-c("Caribe_Cesar",    "Catatumbo",    "Isla_Pacifico",    "Medellin_Carare",
         "Norte_Amazonia", "Orinoquia_Occidental", "Orinoquia_Oriental", 
         "PacificoNorteyCentral", "PatiayMira", "Risaralda_Saldana", 
         "SanAndres_Providencia", "Sinu_SanJorge_Porce", "Sogamoso_Lebrija_Altiplano",
         "SurMagdalena_Cauca", "Trapecio_Amazonico") # Modificar
depL<-list("Caribe_Cesar",    "Catatumbo",    "Isla_Pacifico",    "Medellin_Carare",
           "Norte_Amazonia", "Orinoquia_Occidental", "Orinoquia_Oriental", 
           "PacificoNorteyCentral", "PatiayMira", "Risaralda_Saldana", 
           "SanAndres_Providencia", "Sinu_SanJorge_Porce", "Sogamoso_Lebrija_Altiplano",
           "SurMagdalena_Cauca", "Trapecio_Amazonico") # Modificar








# Si el directorio de resultados y los departamentos ya fue creado, no es 
# necesario volver a crearlo. 

# Cree el directorio results donde se almacenan las carpetas de los resultados 
# de cada departamento.
if(dir.exists("results")==FALSE){dir.create("results")}
setwd(paste( getwd(),"/results/" ,sep=""))
print(getwd())


files_ext<-"CHIRPS_-_" # *("PPT_-_"   ,"CHIRPS_-_")


#### Declaración de las constantes

# nombre de los archivos de las variable predictoras, deben ser iguales
# que los que se ingreso en CPT. 
lead<- c("Aug", "Nov","Aug") # Modificar

# mes de inicio de los trimestres
a<- c(9,12,12) # Modificar

# Ancho del periodo de estudio para cada trimestre
length_periodo<- c(34,34,34) # Modificar


# periodo
# solo puede tomar dos valores "Mes" o "bim"
per<-rep("bim",3) # Modificar

# estos tres vectores deben ser de igual tamaño. 
cbind.data.frame(a, lead, length_periodo, per)

period <-0
for(i in 1:length(a)){
  if(per[i]=="bim"){
    period[i]<-if(a[i]<12){paste0(substr(month.abb[a[i]:(a[i]+1)],1,1), collapse = "")}else if(a[i]==12){period[i]<-paste(substr(month.abb[12],1,1), paste0(substr(month.abb[1],1,1), collapse = ""), sep="")}
  }else if(per=="Mes"){ period[i]<-paste0(substr(month.abb[a[i]],1,1))}
}

# Modificar
ruta_c<- "C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/Cross_val_IDEAM"


## Esta función devuelve las gráficas del goodness index, 
## ruta_c ruta donde se encuentran los archivos
## dep_f son los departamentos que se van tener encuenta en el archivo. 
GoodnessIndex <- function(ruta_c, dep_f, a, lead, period){
  
  good_index=0 # inicialice el vector
  GoodnessIndex=NA # inicialice el data frame
  for(j in 1:length(dep_f)) # realice esto para todos los departamentos
    for(i in 1:length(a)){ # barra para todos los periodos con sus respectivos lead time
      # Lea la tabla 
      goodnex<-read.table(paste(ruta_c,"/GoodnessIndex_",a[i],"_",lead[i],"_",files_ext,dep_f[j],".txt", sep=""),  skip=6, colClasses=c(rep("numeric",3),"character",rep("numeric",4)))
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
  facet_grid(~Dep) + theme_bw() + theme(legend.position = "none", strip.text.x = element_text(size = 8, angle = 90))+
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted")

ggsave("summary_GI.png", width =11, height = 5)






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
ind<-function(tipo, a, lead, dep, ruta_c, period){
  
  datos<-NA # Creación de una trama de datos
  
  # lea los datos y apilelos en una tabla de datos
  for(j in 1:length(a)){
    names_below<-list.files(ruta_c, pattern = paste(tipo, "_", a[j], "_", lead[j], "_",files_ext, dep, sep=""))
    
    # Ciclo for que barre el número de departamentos que existan 
    # En caso de desear más departamentos modificar este argumento
    data_p<-read.table(paste(ruta_c,"/",names_below,sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric") )
    data_p<-data.frame(rep(period[j], dim(data_p)[1]) ,rep(a[j], dim(data_p)[1]), rep(lead[j], dim(data_p)[1]), data_p)
    datos<-rbind(datos, data_p) # Lectura de los datos y creación de la tabla global
  }
  
  # Depuración de la base de datos 
  datos<-datos[-1,] # Elimine la primera fila NA
  
  names(datos)<-c("period","Mes_ini", "lead", "Estacion", "Lat", "Lon",tipo) # Asignarle nombres a las columnas
  datos[,6]<-round(datos[,6],3) # Redondear el número de decimales de la curva
  datos<-datos[datos[,6]!=-999,] # Eliminar los datos faltantes
  
  
  
  # entregue el data frame datos 
  return(datos)}

# summary_ind <- Esta funcion guarda automaticamente los mapas de indicadores
# y un archivo departamental resumen todos los indicadores. 
# dep <- departamento a analizar
# ruta_c <- ruta donde se encuentran las salidas del CPT
# a <- mes de inicio del trimestre
# lead <- nombre del archivo de la variable predictora
summary_ind<-function(dep,  ruta_c,  a, lead, period){
 
  # Indicadores a analizar
  tipo <- list("Pearsons_correlation", "Spearmans_correlation", "k_2AFC_Score",
               "Hit_Score", "Hit_Skill_Score", "LEPS_score", "Gerrity_Score",
               "k_2AFC_cat", "k_2AFC_cont", "ROC_below", "ROC_above")
  
  lista<-list()   # cree cree una lista
  for(i in 1:length(tipo)){
    lista[[i]] <- ind(tipo = tipo[[i]], a, lead, dep, ruta_c, period)
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
sapply(depL, summary_ind, ruta_c,  a, lead, period, simplify = T)

for_files<-list()
for(i in 1:length(dep_f)){
  name<-list.files(paste(getwd(), "/" ,sep=""), pattern = paste("Perfomance_Measures_",dep_f[i], sep=""))
  for_files[[i]]<-read.csv(paste(getwd(), "/" ,  name ,sep=""))
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

# ForecastP<- esta funcion permite obtener en un solo archivo las probabilidades 
# de todas las corridas o regiones. 
# dep <- departamento a analizar
# ruta_c <- ruta donde se encuentran las salidas del CPT
# a <- mes de inicio del trimestre
# lead <- nombre del archivo de la variable predictora
ForecastP<-function(dep, ruta_c, a, lead, period){
  Total<-0 # Inicialice un objeto

   
  for(i in 1:length(a)){ # Corra para todas las corridas
    
    # Lea el archivo de probabilidades de acuerdo a 
    file<-paste(ruta_c, "/ForecastProbabilities_", a[i], "_", lead[i], "_",files_ext, dep,".txt", sep="")
    
    w1<-read.table(file,sep="", skip =3, nrow=3,  fill=TRUE)
    w2<-read.table(file,sep="", skip =8, nrow=3, fill=TRUE)
    w3<-read.table(file, sep="", skip =13, nrow=3, fill=TRUE)
    
    
    rownames(w1)[3] <- "below" ; rownames(w2)[3] <- "normal";  rownames(w3)[3] <- "Above"
    
    h1<-data.frame(names(w1), Reduce(function(x, y) merge(x, y), list(t(w1), t(w2), t(w3) )))
    
    
    o<-cbind.data.frame(period[i], a[i], lead[i], h1, row.names=NULL)
    names(o)<- c("Periodo", "Mes_ini", "lead","Estaciones",  "Lat", "Lon", "C1_below", "C2_Normal", "C3_Above")
    
  
    Total<-rbind(Total, o)
    print(i)
  }
  
  Total<-Total[-1,]
  
  
  setwd(paste(ruta, "results/", sep = ""))
  dir.create(paste(getwd(), "/ForecasProb", sep=""))
  setwd(paste(ruta, "results/ForecasProb",  sep = ""))
  write.csv(x =  Total, file = paste("ForecasProb_",dep,".csv", sep=""))
  return(Total)}

cbind.data.frame(a,lead, period)
### Para todas las regiones en caso que tenga 
sapply(depL, ForecastP, ruta_c,  a, lead,period, simplify = T)



for_files<-list()
for(i in 1:length(dep_f)){
  name<-list.files(paste(getwd() ,sep=""), pattern = paste("ForecasProb_",dep_f[i], sep=""))
  for_files[[i]]<-read.csv(paste(getwd(), "/" , "/", name ,sep=""))
  for_files[[i]]<-cbind(dep= dep_f[i], for_files[[i]][,-1])
  names(for_files)[i]<-dep_f[i]
}

for_files <-Reduce(function(x, y) rbind.data.frame(x, y),for_files)
setwd(paste(ruta, "results/",  sep = ""))
write.csv(x = for_files, file = "Forecast_Prob.csv")



# Corte colombia de acuerdo a las coordenadas asignadas
colombia_1=colombia #  realizar el coorte
colombia_1@data$id <- rownames(colombia_1@data) # cree una nueva variable en el shp
colombia_1@data$id <- as.numeric(colombia_1@data$id) # digale que es de caracter númerico
colombia2 <- fortify(colombia_1, region="id") # convierta el shp en una tabla de datos

# Realice el gráfico de las correlaciones entre las estaciones y el modo 1 de y 
p <- ggplot(colombia2, aes(x=long,y=lat)) # gráfique el país
p <- p + geom_polygon(aes(fill=hole,group=group),fill="snow")
p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="black",size=0.3)


setwd(paste(ruta, "results/ForecasProb",  sep = ""))
for(i in 1:length(a)){
  o<-subset(for_files, Periodo==period[i] & lead== lead[i])

  # Aqui se ingresan los datos de las estaciones
  below <- p + geom_point(data=o, aes(x=Lon, y=Lat, map_id= Estaciones,colour=C1_below))
  below <- below + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
    coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                          legend.text=element_text(size=8),
                          panel.background=element_rect(fill="white",colour="black"),
                          axis.text=element_text(colour="black",size=10),
                          axis.title=element_text(colour="black",size=10,face="bold"),
                          #legend.position = "bottom", 
                          legend.title=element_blank()) 
  # Aqui se colocan los nombres de las estaciones
   below <- below + labs(title="Below")
  
  
  # Aqui se ingresan los datos de las estaciones
  normal <- p + geom_point(data=o, aes(x=Lon, y=Lat, map_id= Estaciones,colour=C2_Normal))
  normal <- normal + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
    coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                          legend.text=element_text(size=8),
                          panel.background=element_rect(fill="white",colour="black"),
                          axis.text=element_text(colour="black",size=10),
                          axis.title=element_text(colour="black",size=10,face="bold"),
                          #legend.position = "bottom", 
                          legend.title=element_blank()) 
  # Aqui se colocan los nombres de las estaciones
  normal <- normal + labs(title="Normal")
  
  
  
  
  # Aqui se ingresan los datos de las estaciones
  above <- p + geom_point(data=o, aes(x=Lon, y=Lat, map_id= Estaciones,colour=C3_Above))
  above <- above + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
    coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                          legend.text=element_text(size=8),
                          panel.background=element_rect(fill="white",colour="black"),
                          axis.text=element_text(colour="black",size=10),
                          axis.title=element_text(colour="black",size=10,face="bold"),
                          #legend.position = "bottom", 
                          legend.title=element_blank()) 
  # Aqui se colocan los nombres de las estaciones
   above <- above + labs(title="Above")
  
  
  
  tiff(filename = paste("Prob_",  period[i], "_",lead[i],".tif", sep=""), height=600,width=1480,res=100)
  print(grid.arrange(below, normal, above, ncol=3))
  dev.off()
  
  
  
  
  
  max_C<-apply(o[,7:9], 1, max)
  cat<-ifelse(o[,7]==max_C, "Below", ifelse(o[,8]==max_C, "Normal", ifelse(o[,9]==max_C, "Above",0)))
  maximos<-cbind.data.frame(o$Estaciones, o$Lon, o$Lat, max_C, cat)
  
  # Aqui se ingresan los datos de las estaciones
  maxi <- p + geom_point(data=maximos, aes(x=o$Lon, y=o$Lat, map_id= o$Estaciones,colour=max_C, shape=cat))
  maxi <- maxi + scale_colour_gradientn(colours = colorRampPalette(c("aliceblue", "steelblue2" , "khaki", "orange1", "red", "firebrick4"))(10),limits=c(0,100))+ 
    coord_equal() + theme( legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                           legend.text=element_text(size=8),
                           panel.background=element_rect(fill="white",colour="black"),
                           axis.text=element_text(colour="black",size=10),
                           axis.title=element_text(colour="black",size=10,face="bold"),
                           #legend.position = "bottom", 
                           legend.title=element_blank()) 
  # Aqui se colocan los nombres de las estaciones
   maxi <- maxi + labs(title="Probabilistic Forecast")
  
  
  
  tiff(filename = paste("MaxiProb_", period[i], "_",lead[i],".tif", sep=""),  height=500,width=650,res=100)
  print(maxi)
  dev.off()
  
  
   
}







