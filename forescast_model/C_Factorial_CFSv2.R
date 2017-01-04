### Paquetes Necesarios
library(FactoMineR) # En este paquete se encuentran la función para realizar el AFM
library(ggplot2) # Paquete para realizar graficos especiales
library(raster)  # Paquete para manejar la información espacial (raster)
library(cowplot) # Apoyo para el paquete ggplo2
library(ade4) # Paquete para realizar análisis multivariado (PCA)
library(grid) # Paquete apoyo para la realización de los gráficos con ggplot2
library(nortest) # paquete para realizar la prueba de normalidad del modelo de regresion
library(lmtest) # paquete para realizar pruebas sobre los supuestos del modelo de regresión
library(rasterVis) # paquete para realizar graficos alterno al paquete raster
library(gridExtra) # Paquete apoyo para la realización de los gráficos con ggplot2
library(pROC) # paquete para realizar las curvas ROC
library(FactoClass) # paquete para realizar analisis cluster

### Seleecion del directorio de trabajo
setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/")
getwd()

ruta=getwd() # se define la ruta principal donde se guardan los archivos








#################################################################################
#################################################################################
######################## Manejo de archivos
#################################################################################
#################################################################################



### Transformar un archivo .tsv(CPT) en raster de CSFv2
transform_raster=function(x){
  mapa_base=raster() # crea un raster por defecto con las dimensiones de la SST CFSv2
  val=c(as.matrix(t(x),ncol=1,byrow = T)) # convierta los valores del raster en un vector (entrega un "character")
  val=as.numeric(val) # convierta los valores a numericos
  val[val==-999.000]=NA # Convierta la codificacion a datos NA
  values(mapa_base)=val # almacene los valores en el raster 
  return(mapa_base) # retorne el raster
}


# Esta función transforma el archivo .tsv(CPT SST CFSv2) 
rasterize=function(dates) { 
  
  if(require(raster)==FALSE){install.packages("raster")} # Instale el paquete raster de ser necesario
  library("raster") # instale el paquete raster de ser necesario
  pos_years=!is.na(dates[1,]) # muestre en que posiciones se encuentran las fechas 
  year_month=dates[1,][pos_years] # extraiga las fechas
  
  # Extraiga los años a partir del mes central 
  if(substr(year_month[2],6,7)=="12"){year=as.numeric(substr(year_month[-1],1,4))+1
  }else{year=as.numeric(substr(year_month[-1],1,4))} 
  
  # Posición de las filas a eliminar
  total_row_delete=c(-1,-3,-(which(dates[,1]=="90.0")-2),-which(dates[,1]=="-90.0"),-which(dates[,1]==""))
  
  dates=dates[total_row_delete,-1] # Elimine las filas con informacion no necesaria y las coordenadas
  list_dates=split(dates,sort(rep(year,180)))# Crea un objeto tipo lista separando por año
  all_raster=lapply(list_dates,transform_raster) # cada objeto de la lista se convierte en un raster 
  layers=stack(all_raster) # Convierta el objeto en un stack 
  layers_crop=crop(layers,extent(-180, 180, -30, 30)) # recorte la información solopara el trópico
  
  return(layers_crop) # devuelva el stack recortado solo para la región tropico 
}



### Esta función convierte los datos de las estacines en datos trimestrales
data_trim=function(Estaciones_C, a){ #Los argumentos son el conjunto de las estaciones 
  ## y el mes de incio del periodo (a)
  stations=Estaciones_C 
  stations=stations[-1:-2,] # Quite las dos primeras filas (coordenadas)
  year=sort(rep(1981:2013,12)) # cree un vector para los años
  month=rep(1:12,length(1981:2013)) # cree el vector de meses
  data_station=cbind.data.frame(year,month,stations, row.names=NULL) # cree un data frame 
  pos=seq(a,dim(data_station)[1],12) #  posiciones ne las que se encuentra el mes a
  pos_select=sort(c(pos,pos+1,pos+2)) # muestre las posiciones del trimestre
  # Agregue los datos del trimestre y luego sumelos. 
  data_out=aggregate(data_station[pos_select,-1:-2],by=list(sort(rep(1:(length(pos)),3))),sum)
  data_out_final=na.omit(data_out[,-1]) # Elimine los NA
  years_y=na.omit(year[pos+1]) # Elimine los NA
  data_out_final=data.frame(years_y,data_out_final) # Cree un data frame con los datos finales
  return(data_out_final)
} # devuelva los datos finales








#################################################################################
#################################################################################
######################## Paletas de colores y raster de apoyo para graficas 
#################################################################################
#################################################################################



#### Declaración paletas de colores para los gráficos

jet.colors <-colorRampPalette(c( "white", "yellow", "#FF7F00", "red", "#7F0000"))
jet=colorRampPalette(c("#2166AC", "snow","#B2182B" ))


myThemec <- BuRdTheme()
myThemec$regions$col=colorRampPalette(c( "white", "yellow", "#FF7F00", "red", "#7F0000", "#B72330", "#B41D2D", "#B2182B"))(30)
myThemec$panel.background$col = "gray30"



myTheme <- BuRdTheme() # Asigne un tema ya existente a 
myTheme$regions$col=colorRampPalette(c("#2166AC", "white","snow","#B2182B"))(20)
myTheme$panel.background$col = "gray30"


myTheme1 <- BuRdTheme()
myTheme1$regions$col=colorRampPalette(c("#2166AC", "snow","#B2182B"))(20)


myTheme2 <- BuRdTheme()
myTheme2$regions$col=colorRampPalette(c("black"))


# Raster de apoyo para gráficos

#map=results$SST[[1]]
#map[which(is.na(results$SST[[1]][]))][]=0
#map[which(map[]!=0)]=NA
#writeRaster(map,paste(ruta,"/map.tif",sep=""))
map<-raster(paste(ruta,"/map.tif", sep = "")) # Lectura de un archivo raster para graficar
plot(map) # grafico





#################################################################################
#################################################################################
######################## Funciones de Correlación 
#################################################################################
#################################################################################



### Funcion coeficiente de correlacion de Pearson
# Permite calcular el mapa de correlaciones de una estacion en particular
# station = nombre o coluumna de la estacion a la cual se le desea calcular el mapa de correlaciones
# variable oceanoatmosferica (TSM en este caso) con la cual se desea calcular las correlaciones
pearson_1<-function(station, var_ocanoAt){
  #### Variación de las estaciones
  
  #### Correlación de Pearson 
  ocean=which(!is.na(var_ocanoAt[[1]][])) # tome las posiciones en las que la variable sea diferente de NA
  correl=array(NA,length(ocean)) # relice un arreglo del tamaño de oceano 
  var_table=var_ocanoAt[] # Realice una tabla de la variable
  
  for(i in 1:length(ocean)){ # En todos los pixeles diferentes de NA
    var_pixel=var_table[ocean[i],] # Extraiga el pixel i 
    correl[i]=cor(station,var_pixel, method = "pearson") # realice la correlación entre el pixel i el modo 1 de x
  } # 
  
  correl_map_p=var_ocanoAt[[1]] # Cree un raster vacio 
  correl_map_p[]=NA 
  correl_map_p[ocean]=correl # Almacene en el raster los NA 
  
  return(correl_map_p)}

### Funcion coeficiente de correlacion de Spearman
# Permite calcular el mapa de correlaciones de una estacion en particular
# station = nombre o coluumna de la estacion a la cual se le desea calcular el mapa de correlaciones
# variable oceanoatmosferica (TSM en este caso) con la cual se desea calcular las correlaciones
spearman_1<-function(station, var_ocanoAt){
  #### Correlación de Spearman
  ocean=which(!is.na(var_ocanoAt[[1]][])) # tome las posiciones en las que la variable sea diferente de NA
  correl=array(NA,length(ocean)) # relice un arreglo del tamaño de oceano 
  var_table=var_ocanoAt[] # Realice una tabla de la variable
  
  for(i in 1:length(ocean)){ # En todos los pixeles diferentes de NA
    var_pixel=var_table[ocean[i],] # Extraiga el pixel i 
    correl[i]=cor(station,var_pixel, method = "spearman") # realice la correlación entre el pixel i el modo 1 de x
  } # 
  
  correl_map_s=var_ocanoAt[[1]] # Cree un raster vacio 
  correl_map_s[]=NA 
  correl_map_s[ocean]=correl # Almacene en el raster los NA 
  
  return(correl_map_s)}

## extract_function, permite extraer solo los valores de un raster mayores a 0.5 en valor absoluto
## Sirve para filtrar las refiones con alta correlación de un mapa de correlaciones
## lista = es un objeto tipo lista de entrada (que contiene varios raster)
## en caso de que lsita sea un stack cortara los raster respecto al primer raster
extract_function<-function(lista){
  correl_1=lista # inicialice un raster
  correl_1[]=NA  # todos sus valores son NA
  correl_1[which(abs(lista[])>0.5)]<-lista[which(abs(lista[])>0.5)] # almacene los valores 
  #en las posiciones que cumplen la condición 
  return(correl_1)}



### Esta función realiza los mapas de correlaciones para el departamento,  
### lead, inicio del trimestre y variable de respuesta y deseada
### dep= departamento en el cual se ubica la estación (a pronosticar)
### lead =  nombre del archivo de la sst (MAM, DEF ...)
### a = mes de incio del trimestre
### answer = tipo de variable de respuesta (mean, pca, o el nombre de la estación a pronsticar)
correlation<-function(dep, lead, a, answer){
  
  ### Hay que leer el archivo de estaciones y seleccionar una sola (preliminar)
  Estaciones_C <- read.delim(paste(ruta,"/dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
  SST<-read.table(paste(ruta,"/CFSv2_evaluacion/",lead,".tsv",sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
  
  ## Conversión a raster de la SST y de los datos de las estaciones a trimestrales
  SST=rasterize(SST)
  data<-data_trim(Estaciones_C, a)
  
  
  ## Condición de los archivos dependiendo del trimestre a pronosticar
  if(a == 12 | lead== "MAM_Nov"| lead== "MAM_Sep"| lead== "JJA_Dec"){
    data<-data[data$years_y!=1981 & data$years_y!=1982,]
    var_ocanoAt <-SST[[paste("X", 1983:2013,sep="")]]
  } else  if(a != 12 & lead!= "MAM_Nov"& lead!= "MAM_Sep"& lead!= "JJA_Dec"){
    data<-data[data$years_y!=1981,]
    var_ocanoAt <-SST[[paste("X", 1982:2013,sep="")]]
  }
  
  
  # elimine la columna de los a;is 
  data=data[,-1]
  # inicialice el vector estación 
  station<-0
  ### Condición para la variable de respuesta
  if(answer=="mean"){
    station =  apply(data, 1, mean) 
  } else if(answer == "pca"){
    pca<-dudi.pca(t(data),scan = FALSE)
    station<-pca$co
  } else station = data[,answer]
  
  #### Correlación de Pearson 
  correl_map_p<-pearson_1(station, var_ocanoAt)
  
  
  ### Correlación de Spearman
  correl_map_s<-spearman_1(station, var_ocanoAt)
  
  
  ### Cree un stack para los mapas de correlaciones
  correl<-stack(correl_map_p, correl_map_s)
  names(correl)=c("Pearson", "Spearman")
  rasters<-list(ind=station, data=data, SST=var_ocanoAt, maps=correl)
  return(rasters)}
### Esta función retorna la variable y seleccionada
### los datos de la estación trimestrales (sin la variable year)
### los datos de la sst para el lead seleccionado (depurados de acuerdo al triemstre)
### y los mapas de correlaciones (tanto para pearson como spearman en un stack)



### Esta función entrega los mapas de correlaciones de todas las estaciones de un departamento
### Permite obtener un area general con las regiones más frecuentes
### dep = nombre del archivo de departamentos (casanare)
### lead = nombre del archivo de de la TSM con el que se realizara el ajuste (lead)
### a = mes de incio del trimestre
sta_complete<-function(dep, lead, a){
  
  ### Hay que leer el archivo de estaciones y seleccionar una sola (preliminar)
  Estaciones_C <- read.delim(paste(ruta,"/dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
  SST<-read.table(paste(ruta,"/CFSv2_evaluacion/",lead,".tsv",sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
  
  ## Conversión a raster de la SST y de los datos de las estaciones a trimestrales
  SST=rasterize(SST)
  data<-data_trim(Estaciones_C, a)
  
  
  
  ## Condición de los archivos dependiendo del trimestre a pronosticar
  if(a == 12 | lead== "MAM_Nov"| lead== "MAM_Sep"| lead== "JJA_Dec"){
    data<-data[data$years_y!=1981 & data$years_y!=1982,]
    var_ocanoAt <-SST[[paste("X", 1983:2013,sep="")]]
  } else  if(a != 12 & lead!= "MAM_Nov"& lead!= "MAM_Sep"& lead!= "JJA_Dec"){
    data<-data[data$years_y!=1981,]
    var_ocanoAt <-SST[[paste("X", 1982:2013,sep="")]]
  }
  
  # elimine la columna de los a;is 
  data=data[,-1]
  
  ### Correlación de Pearson para todas las estaciones del departamento
  prueba_pearson<-list() # cree una lista donde se almacenan los rasters
  prueba_pearson_1<-apply(data, 2, pearson_1, var_ocanoAt ) # haga un mapa de correlación
  # por cada columna
  prueba_pearson<-stack(prueba_pearson_1) # cree un stack
  
  ### Correlación de Spearman para todas las estaciones del departamento
  prueba_spearman<-list()# cree una lista donde se almacenan los rasters
  prueba_spearman_1<-apply(data, 2, spearman_1, var_ocanoAt)# haga un mapa de correlación
  # por cada columna
  prueba_spearman<-stack(prueba_spearman_1) # cree un stack
  
  ## Cree un mapa de las regiones más frecuentes (areas con correlación mayor a 0.5)
  areas_s<-sum(abs(prueba_spearman)>0.5)
  areas_p<-sum(abs(prueba_pearson)>0.5)
  
  ## Haga los cortes de las zonas con correlaciones altas  (abs(corr)>0.5)
  cut_s<-list()
  cut_s<-lapply(prueba_spearman_1, extract_function)
  cut_s<-stack(cut_s)
  
  cut_p<-list()
  cut_p<-lapply(prueba_pearson_1, extract_function)
  cut_p<-stack(cut_s)
  
  # Almacene las respuestas en una lista
  answer<-list(prueba_pearson, prueba_spearman, areas_p, areas_s,  cut_p, cut_s)
  names(answer)=c("cor_pear", "cor_spe", "areas_p", "areas_s", "cut_p", "cut_s")
  return(answer)}




















########################
########################
###### Seleccion de la region predictora con analisis cluster
########################
########################



## Parametros con los que se corre la función 
dep="cordoba"
lead="DEF_Aug" 
a=12



# corrida del departamento
all_stations<-sta_complete(dep, lead, a)


plot(all_stations$areas_p, colNA="gray30", col=jet.colors(20), main="Pearson") 



stations<-34
plot(all_stations$areas_p, colNA="gray30", col=jet.colors(20), main="Pearson") 


all_stations$cor_pear[][which(abs(all_stations$cor_pear)[]<0.5)]=0


dim_1<-dim(rasterToPoints(all_stations$areas_p))[1]
barplot(table(rasterToPoints(all_stations$areas_p)[,3]))


position<-which(all_stations$cor_pear[[1]][]==0)

freq=rasterToPoints(all_stations$areas_p)[,3]
#class(freq)
#table(freq[position])
freq=freq[position]

#### Aqui los individuos son los pixeles. 
#### Entonces el ACP se puede realizar sin problemas.


cluster <- cbind(rasterToPoints(all_stations$cor_pear), 
                 freq=rasterToPoints(all_stations$areas_p)[,3])


cluster1<-FactoClass( cluster , dudi.pca, k.clust =3,
                      scanFC = FALSE,  nfcl = 10)








#### Correlación de Pearson 
ocean=which(!is.na(all_stations$areas_p[])) # tome las posiciones en las que la variable sea diferente de NA

cluster_values<-all_stations$areas_p
plot(cluster_values)
cluster_values[ocean]<-cluster1$cluster
plot(cluster_values, col=c("red","blue", "green", "orange"))


x11()
par(mfrow=c(1,2))
plot(all_stations$areas_p, colNA="gray30", col=jet.colors(20), main="Pearson") 
plot(cluster_values, colNA="black", col=c("red","blue", "green"))



plot(cluster_values==1, colNA="black")
plot(cluster_values==2, colNA="black")
plot(cluster_values==3, colNA="black")




plot(cluster_values==1, colNA="black")

cluster_values[cluster_values==1]="NA"
plot(cluster_values)



##### Extraccion de los clusters como raster individuales


# Esta función permite extraer el raster de una región especifica 
# x representa el numero de cluster que se desea extraer
# cluster_values es el raster de los cluster
ext_by_clust<-function(x, cluster_values){
  # obtenga las posiciones donde el raster toma el valor x
  ocean=which(cluster_values[]==x)#  tome las posiciones en las que la variable sea diferente de NA
  
  cluster<-cluster_values # Cree un raster
  cluster[]=NA # inicialicelo con valores NA
  cluster[ocean]<-1 # Llene las posiciones donde toma los valores x, con 1
  print(plot(cluster)) # Grafique la region x
  return(cluster)} # devuelva el raster de la region x



num_clust<-list(2,3) #Declare en un objeto tipo lista los cluster de interes
# Cree un stack con las formas de los cluster
num_zone<-stack(sapply(num_clust, ext_by_clust, cluster_values))
# Cambie los nombres de los objetos del stack
names(num_zone)<-paste("cluster", num_clust, sep="_")
plot(num_zone) # Grafique los objetos






################################################################
################################################################ 






answer="DoctrinaLa"


results<-correlation(dep, lead, a,answer)

























############################ Pruebas para revisar
############################ si el MFA presenta problemas
############################ y si el DMFA puede arreglarlos








region_2<-mask(results$SST, cluster_2)
plot(region_2)














# num_zone ahora sera un stack, supongo por ahora que no tendra más de 5 capas. 

DMFA_C<-function(dep, a, lead,lead_num, num_zone, results, answer){
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  
  ###3 Aqui deberian de ir condicionantes para 
  
  
  
  
  
  
  
  ##### Para utilizar este tipo de analisis se debe crear una variable columna 
  res.dmfa = DMFA ( iris, num.fact = 5)
  summary(res.dmfa)
  dim(res.dmfa$var$coord[,1:3])
  
  
  ### Prueba para verificar las coordenadas que se debian utilizar
  #res<-MFA(t(iris[,-5]), group=c(50,50,50), type=rep("s",3),
  #         ncp=5, name.group=c("setosa","versicolor","virginica"))
  #dim(res$global.pca$ind$coord[,1:3])
  
  
  
  
  
   return(entrega)}







