### Paquetes Necesarios 
library(ggplot2)
library(rasterVis)
library(reshape)



### Directorio de trabajo
setwd("C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/results/plataforma") # Modificar
getwd()



#Condicional de acuerdo al departamento devuelve el id de las estaciones

conditional <- function(x){
  if(x == "Caribe_Cesar"){ x = c("32", "49") } else if(x == "") {}
}


### Aqui se almacenaran los resultados de la validacion cruzada
ruta <- "C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/Cross_val_IDEAM/" # Modificar

table_Ind<-function(gFiles, Pfiles, Kfiles, Can.files, prob.files){
  # Para un archivo individual 
  goodnex<-read.table(paste(ruta,gFiles, sep=""),  skip=6, colClasses=c(rep("numeric",3),"character",rep("numeric",4)))
  goodnex <- max(goodnex[,ncol(goodnex)])
  Pearson<-read.table(paste(ruta,Pfiles, sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric"))[,c(1,4)]
  names(Pearson) <- c("id","Pearson")
  kendall<-read.table(paste(ruta,Kfiles, sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric"))[,c(1,4)]
  names(kendall) <- c("id","kendall")
  canonical<-read.table(paste(ruta,Can.files, sep=""),  skip=3)[,2]
  
  
  # Lea el archivo de probabilidades de acuerdo a 
  file<-paste(ruta_c, "/ForecastProbabilities_", a[i], "_", lead[i], "_",files_ext, dep,".txt", sep="")
  
  w1<-read.table(paste(ruta,prob.files, sep=""),sep="", skip =3, nrow=3,  fill=TRUE, check.names=FALSE)
  w2<-read.table(paste(ruta,prob.files, sep=""),sep="", skip =8, nrow=3, fill=TRUE)
  w3<-read.table(paste(ruta,prob.files, sep=""), sep="", skip =13, nrow=3, fill=TRUE)
  

  # Para el mes en el caso que me den el mes de inicio y no el central 
  x<- as.numeric(strsplit(Can.files, "_")[[1]][2]) 
  if(x ==  12){ x = 1} else if(x != 12){ x = x}
  
  
  # despues de haber definido el mes central del periodo para hayar el a\no
  if(x==12 | x == 1) {
    year <- as.numeric(strsplit(strsplit(rownames(w1)[3], "/")[[1]][2], "-")[[1]][1])
  } else if(x!=12 | x != 1){
    year <- as.numeric(strsplit(rownames(w1)[3], "/")[[1]][3])
  }
  
  # Ejemplo para el departamento
  dep <- strsplit(strsplit(Can.files, "_-_")[[1]][2],".txt")[[1]]
  # Solo extraiga las estaciones de interes
  id <- conditional(dep)
  
  
  
  rownames(w1)[3] <- "below" ; rownames(w2)[3] <- "normal";  rownames(w3)[3] <- "Above"
  
  #### Revisar cuando hagamos una corrida que pasa con los nombres de las variables
  forecast<- data.frame(year= year  , month = x,  id = as.numeric(names(w1)), Reduce(function(x, y) merge(x, y), list(t(w1), t(w2), t(w3) )))[, -c(4:5)]
  forecast<-forecast[which(forecast$id %in% id),]

  
 
  Ind<-merge(Pearson,  kendall, all=TRUE)
  Ind<-Ind[which(Ind$id %in% id),]
  Ind<-cbind.data.frame(year= year  , month = x, Ind, goodnex,  corCanonica = t(canonical), row.names = NULL)

  
  forecast <- 
    
    sum(forecast$id %in% id)
  
  
  
 

  
return(Ind)}




### Esta funcion guarda un archivo con todas 
### las metricas de las corridas para todos los trimestres?
### Lo que debo de preguntar es si para todos los departamentos juntos
metrics_and_forecast <- function(ruta){
  gFiles <- list.files(ruta, pattern = "GoodnessIndex")
  Pfiles <- list.files(ruta, pattern = "Pearsons_correlation")
  Kfiles <- list.files(ruta, pattern = "k_2AFC_Score_")
  Can.files<-list.files(ruta, pattern= "CanonicalCorrelations")
  prob.files<- list.files(ruta, pattern = "ForecastProbabilities")

  haber<-mapply(table_Ind, gFiles, Pfiles, Kfiles, Can.files, prob.files, SIMPLIFY = FALSE)
  
  
  Sys.Date()
  
  
}





