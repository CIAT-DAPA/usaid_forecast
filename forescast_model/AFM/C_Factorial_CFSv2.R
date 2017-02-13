### Paquetes Necesarios
library(FactoMineR) # En este paquete se encuentran la funcion para realizar el AF
library(ggplot2) # Paquete para realizar graficos especiales
library(raster)  # Paquete para manejar la informacion espacial (raster)
library(cowplot) # Apoyo para el paquete ggplo2
library(ade4) # Paquete para realizar analisis multivariado (PCA)
library(grid) # Paquete apoyo para la realizacion de los graficos con ggplot2
library(nortest) # paquete para realizar la prueba de normalidad del modelo de regresion
library(lmtest) # paquete para realizar pruebas sobre los supuestos del modelo de regresion
library(rasterVis) # paquete para realizar graficos alterno al paquete raster
library(gridExtra) # Paquete apoyo para la realizacion de los graficos con ggplot2
library(pROC) # paquete para realizar las curvas ROC
library(FactoClass) # paquete para realizar analisis cluster
library(rasterVis) # Paquete para realizar graficas

### Seleecion del directorio de trabajo
setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/")
getwd()

ruta=getwd() # se define la ruta principal donde se guardan los archivos










################# Modulo 1




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


# Esta funcion transforma el archivo .tsv(CPT SST CFSv2) 
rasterize=function(dates) { 
  
  if(require(raster)==FALSE){install.packages("raster")} # Instale el paquete raster de ser necesario
  library("raster") # instale el paquete raster de ser necesario
  pos_years=!is.na(dates[1,]) # muestre en que posiciones se encuentran las fechas 
  year_month=dates[1,][pos_years] # extraiga las fechas
  
  # Extraiga los años a partir del mes central 
  if(substr(year_month[2],6,7)=="12"){year=as.numeric(substr(year_month[-1],1,4))+1
  }else{year=as.numeric(substr(year_month[-1],1,4))} 
  
  # Posicion de las filas a eliminar
  total_row_delete=c(-1,-3,-(which(dates[,1]=="90.0")-2),-which(dates[,1]=="-90.0"),-which(dates[,1]==""))
  
  dates=dates[total_row_delete,-1] # Elimine las filas con informacion no necesaria y las coordenadas
  list_dates=split(dates,sort(rep(year,180)))# Crea un objeto tipo lista separando por año
  all_raster=lapply(list_dates,transform_raster) # cada objeto de la lista se convierte en un raster 
  layers=stack(all_raster) # Convierta el objeto en un stack 
  layers_crop=crop(layers,extent(-180, 180, -30, 30)) # recorte la informacion solopara el tropico
  
  return(layers_crop) # devuelva el stack recortado solo para la region tropico 
}



### Esta funcion convierte los datos de las estacines en datos trimestrales
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



#### Declaracion paletas de colores para los graficos

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
myTheme1$panel.background$col = "white"


myTheme2 <- BuRdTheme()
myTheme2$regions$col=colorRampPalette(c("black"))


# Raster de apoyo para graficos

#map=results$SST[[1]]
#map[which(is.na(results$SST[[1]][]))][]=0
#map[which(map[]!=0)]=NA
#writeRaster(map,paste(ruta,"/map.tif",sep=""))
map<-raster(paste(ruta,"/map.tif", sep = "")) # Lectura de un archivo raster para graficar
plot(map) # grafico





#################################################################################
#################################################################################
######################## Funciones de Correlacion 
#################################################################################
#################################################################################



### Funcion coeficiente de correlacion de Pearson
# Permite calcular el mapa de correlaciones de una estacion en particular
# station = nombre o coluumna de la estacion a la cual se le desea calcular el mapa de correlaciones
# variable oceanoatmosferica (TSM en este caso) con la cual se desea calcular las correlaciones
pearson_1<-function(station, var_ocanoAt){
  #### Variacion de las estaciones
  
  #### Correlacion de Pearson 
  ocean=which(!is.na(var_ocanoAt[[1]][])) # tome las posiciones en las que la variable sea diferente de NA
  correl=array(NA,length(ocean)) # relice un arreglo del tamaño de oceano 
  var_table=var_ocanoAt[] # Realice una tabla de la variable
  
  for(i in 1:length(ocean)){ # En todos los pixeles diferentes de NA
    var_pixel=var_table[ocean[i],] # Extraiga el pixel i 
    correl[i]=cor(station,var_pixel, method = "pearson") # realice la correlacion entre el pixel i el modo 1 de x
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
  #### Correlacion de Spearman
  ocean=which(!is.na(var_ocanoAt[[1]][])) # tome las posiciones en las que la variable sea diferente de NA
  correl=array(NA,length(ocean)) # relice un arreglo del tamaño de oceano 
  var_table=var_ocanoAt[] # Realice una tabla de la variable
  
  for(i in 1:length(ocean)){ # En todos los pixeles diferentes de NA
    var_pixel=var_table[ocean[i],] # Extraiga el pixel i 
    correl[i]=cor(station,var_pixel, method = "spearman") # realice la correlacion entre el pixel i el modo 1 de x
  } # 
  
  correl_map_s=var_ocanoAt[[1]] # Cree un raster vacio 
  correl_map_s[]=NA 
  correl_map_s[ocean]=correl # Almacene en el raster los NA 
  
  return(correl_map_s)}

## extract_function, permite extraer solo los valores de un raster mayores a 0.5 en valor absoluto
## Sirve para filtrar las refiones con alta correlacion de un mapa de correlaciones
## lista = es un objeto tipo lista de entrada (que contiene varios raster)
## en caso de que lsita sea un stack cortara los raster respecto al primer raster
extract_function<-function(lista){
  correl_1=lista # inicialice un raster
  correl_1[]=NA  # todos sus valores son NA
  correl_1[which(abs(lista[])>0.5)]<-lista[which(abs(lista[])>0.5)] # almacene los valores 
  #en las posiciones que cumplen la condicion 
  return(correl_1)}



### Esta funcion realiza los mapas de correlaciones para el departamento,  
### lead, inicio del trimestre y variable de respuesta y deseada
### dep= departamento en el cual se ubica la estacion (a pronosticar)
### lead =  nombre del archivo de la sst (MAM, DEF ...)
### a = mes de incio del trimestre
### answer = tipo de variable de respuesta (mean, pca, o el nombre de la estacion a pronsticar)
correlation<-function(dep, lead, a, answer){
  
  ### Hay que leer el archivo de estaciones y seleccionar una sola (preliminar)
  Estaciones_C <- read.delim(paste(ruta,"/dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
  SST<-read.table(paste(ruta,"/CFSv2_evaluacion/",lead,".tsv",sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
  
  ## Conversion a raster de la SST y de los datos de las estaciones a trimestrales
  SST=rasterize(SST)
  data<-data_trim(Estaciones_C, a)
  
  
  ## Condicion de los archivos dependiendo del trimestre a pronosticar
  if(a == 12 | lead== "MAM_Nov"| lead== "MAM_Sep"| lead== "JJA_Dec"){
    data<-data[data$years_y!=1981 & data$years_y!=1982,]
    var_ocanoAt <-SST[[paste("X", 1983:2013,sep="")]]
  } else  if(a != 12 & lead!= "MAM_Nov"& lead!= "MAM_Sep"& lead!= "JJA_Dec"){
    data<-data[data$years_y!=1981,]
    var_ocanoAt <-SST[[paste("X", 1982:2013,sep="")]]
  }
  
  
  # elimine la columna de los a;is 
  data=data[,-1]
  # inicialice el vector estacion 
  station<-0
  ### Condicion para la variable de respuesta
  if(answer=="mean"){
    station =  apply(data, 1, mean) 
  } else if(answer == "pca"){
    pca<-dudi.pca(t(data),scan = FALSE)
    station<-pca$co
  } else station = data[,answer]
  
  #### Correlacion de Pearson 
  correl_map_p<-pearson_1(station, var_ocanoAt)
  
  
  ### Correlacion de Spearman
  correl_map_s<-spearman_1(station, var_ocanoAt)
  
  
  ### Cree un stack para los mapas de correlaciones
  correl<-stack(correl_map_p, correl_map_s)
  names(correl)=c("Pearson", "Spearman")
  rasters<-list(ind=station, data=data, SST=var_ocanoAt, maps=correl)
  return(rasters)}
### Esta funcion retorna la variable y seleccionada
### los datos de la estacion trimestrales (sin la variable year)
### los datos de la sst para el lead seleccionado (depurados de acuerdo al triemstre)
### y los mapas de correlaciones (tanto para pearson como spearman en un stack)



### Esta funcion entrega los mapas de correlaciones de todas las estaciones de un departamento
### Permite obtener un area general con las regiones mas frecuentes
### dep = nombre del archivo de departamentos (casanare)
### lead = nombre del archivo de de la TSM con el que se realizara el ajuste (lead)
### a = mes de incio del trimestre
sta_complete<-function(dep, lead, a){
  
  ### Hay que leer el archivo de estaciones y seleccionar una sola (preliminar)
  Estaciones_C <- read.delim(paste(ruta,"/dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
  SST<-read.table(paste(ruta,"/CFSv2_evaluacion/",lead,".tsv",sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
  
  ## Conversion a raster de la SST y de los datos de las estaciones a trimestrales
  SST=rasterize(SST)
  data<-data_trim(Estaciones_C, a)
  
  
  
  ## Condicion de los archivos dependiendo del trimestre a pronosticar
  if(a == 12 | lead== "MAM_Nov"| lead== "MAM_Sep"| lead== "JJA_Dec"){
    data<-data[data$years_y!=1981 & data$years_y!=1982,]
    var_ocanoAt <-SST[[paste("X", 1983:2013,sep="")]]
  } else  if(a != 12 & lead!= "MAM_Nov"& lead!= "MAM_Sep"& lead!= "JJA_Dec"){
    data<-data[data$years_y!=1981,]
    var_ocanoAt <-SST[[paste("X", 1982:2013,sep="")]]
  }
  
  # elimine la columna de los a;is 
  data=data[,-1]
  
  ### Correlacion de Pearson para todas las estaciones del departamento
  prueba_pearson<-list() # cree una lista donde se almacenan los rasters
  prueba_pearson_1<-apply(data, 2, pearson_1, var_ocanoAt ) # haga un mapa de correlacion
  # por cada columna
  prueba_pearson<-stack(prueba_pearson_1) # cree un stack
  
  ### Correlacion de Spearman para todas las estaciones del departamento
  prueba_spearman<-list()# cree una lista donde se almacenan los rasters
  prueba_spearman_1<-apply(data, 2, spearman_1, var_ocanoAt)# haga un mapa de correlacion
  # por cada columna
  prueba_spearman<-stack(prueba_spearman_1) # cree un stack
  
  ## Cree un mapa de las regiones mas frecuentes (areas con correlacion mayor a 0.5)
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











############ Modulo 2




########################
########################
###### Seleccion de la region predictora con analisis cluster
########################
########################



## Parametros con los que se corre la funcion 
dep="casanare"


lead_v<-c(paste(rep("MAM",3),c("Feb","Nov","Sep"), sep="_"),
        paste(rep("JJA",3),c("May","Feb","Dec"), sep="_"),
        paste(rep("SON",3),c("Aug","May","Mar"), sep="_"),
        paste(rep("DEF",3),c("Nov","Aug","Jun"), sep="_"))



lead_v
lead= "DEF_Jun"
lead
a=12
a

# Corrida de los mapas de correlacion para todo el departamento
all_stations<-sta_complete(dep, lead, a)
# Grafico departamental de la region mas frecuente
print(plot(all_stations$areas_p, colNA="gray30", col=jet.colors(10), main="Areas Pearson")) 
  
# Se le asigna el valor de 0 a todos los pixeles con correlacion menor a 0.5
all_stations$cor_pear[][which(abs(all_stations$cor_pear)[]<0.5)]=0  
  
# Vuelve NA todos los pixeles cuya frecuencia 
# de alta correlación sea menor al 10% de las estaciones
for(i in 1:dim(all_stations$cor_pear)[3]){
  all_stations$cor_pear[[i]][][which(all_stations$areas_p[]<=(dim(all_stations$cor_pear)[3]*0.05))]=NA
}
# En el caso de casanare no se eliminan pixeles  
print(plot(all_stations$cor_pear, col=jet.colors(10))) # grafica
  
  

# Encuentre las posiciones de los pixles que tenangan una frecuecnia mayor al 5% 
# de las estaciones del departamento 
positions=which(all_stations$areas_p[]>(dim(all_stations$cor_pear)[3]*0.05))
freqcuencyC<-all_stations$areas_p # Cree un raster
freqcuencyC[]<-NA  # Elimine los valores 
# Guarde solo los valores que cumplen la condicion
freqcuencyC[positions]<-all_stations$areas_p[positions] 
#plot(freqcuencyC)
  
# Extraiga los valores de los pixeles
freq=rasterToPoints(freqcuencyC)[,3]
#table(freq[positions])
  
#### Aqui los individuos son los pixeles, el ACP se puede realizar sin problemas.
  cluster <- cbind(rasterToPoints(all_stations$cor_pear), 
                   freq=freq)# cree una base de datos con las coordenadas, 
                             #las correlaciones y la frecuencia de las correlaciones 

# Creacion de los cluster (regiones)
k_clust<-4#numero de clusters deseados
# clusters por defecto entrega 3 graficas 
cluster1<-FactoClass( cluster , dudi.pca, k.clust =k_clust,
                      scanFC = FALSE,  nfcl = 10)
#x11()
plotFactoClass(cluster1) # Entrega el plano factorial con la ubicacion de los cluster

table(cluster1$cluster)




#### Cree el raster de los cluster
# tome las posiciones en las que la variable sea diferente de NA
ocean=which(!is.na(all_stations$cor_pear[[1]][])) 
cluster_values<-all_stations$cor_pear[[1]] # Cree un raster
cluster_values[]<-NA # Elimine los valores 
cluster_values[ocean]<-cluster1$cluster # Guarde los valosres del cluster en el raster
plot(cluster_values) # Grafique el raster



#### Paleta de colores
myTheme <- BuRdTheme() # Asigne un tema ya existente a 
myTheme$regions$col=colorRampPalette(c("gray30"))(20)
myTheme$panel.background$col = "white"


myTheme0 <- BuRdTheme()
myTheme0$regions$col=colorRampPalette(c("#2166AC", "lightcoral","#B2182B"))(20)


areas_draw<-levelplot(all_stations$areas_p, par.settings=myTheme1, margin=FALSE, main=paste("Areas ", dep, " - ", lead ,sep="")) 
cluster_draw<-levelplot(cluster_values, par.settings=myTheme0, margin=FALSE, main= "Clusters") + levelplot(map, par.settings=myTheme)

tiff(paste(ruta,"/results_graphs/DMFA/",dep, "/",dep,"_", lead,".tif",sep=""), height=400,width=700,res=100,compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
grid.arrange(areas_draw,cluster_draw,ncol=1)
dev.off()



setwd(paste(ruta,"/results_graphs/DMFA/",dep,sep=""))
getwd()
writeRaster(cluster_values, filename=paste(ruta,"/results_graphs/DMFA/",dep,"/",substring(dep,1,4),"_", lead,".tif", sep=""))






##### Extraccion de los clusters como raster individuales

# Union de los cluster con problemas antes de correr los modelos

dep="valle"


lead_v
lead= "JJA_Dec" 
lead
a=6
a

setwd(paste(ruta,"/results_graphs/DMFA/",dep,sep=""))
prueba<-getwd()

region<-raster(paste(prueba,"/",substring(dep,1,4),"_", lead,".tif", sep = "")) # Lectura de un archivo raster para graficar
plot(region) # grafico



table(region[]==1|region[]==4)
region[which(region[]==1|region[]==4)]=1

table(region[])

#as.numeric(names(table(region[])))




areas_draw<-levelplot(all_stations$areas_p, par.settings=myTheme1, margin=FALSE, main=paste("Areas ", dep, " - ", lead ,sep="")) 
cluster_draw<-levelplot(region, par.settings=myTheme0, margin=FALSE, main= "Clusters") + levelplot(map, par.settings=myTheme)

tiff(paste(ruta,"/results_graphs/DMFA/",dep, "/",dep,"_", lead,".tif",sep=""), height=400,width=700,res=100,compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
grid.arrange(areas_draw,cluster_draw,ncol=1)
dev.off()



setwd(paste(ruta,"/results_graphs/DMFA/",dep,sep=""))
getwd()
writeRaster(region, filename=paste(ruta,"/results_graphs/DMFA/",dep,"/",substring(dep,1,4),"_", lead,".tif", sep=""))






#as.numeric(names(table(cluster_values[])))








# Esta funcion permite extraer el raster de una region especifica 
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



num_clust<-list(1,2,3,4) #Declare en un objeto tipo lista los cluster de interes
# Cree un stack con las formas de los cluster
num_zone<-stack(sapply(num_clust, ext_by_clust, cluster_values))
# Cambie los nombres de los objetos del stack
names(num_zone)<-paste("cluster", num_clust, sep="_")
plot(num_zone) # Grafique los objetos






################################################################
################################################################ 






answer="StaIsabel"
dep="santander"
lead_v
lead="DEF_Nov"
a=12
results<-correlation(dep, lead, a,answer)
plot(results$maps)






############################ Pruebas para revisar
############################ si el MFA presenta problemas
############################ y si el DMFA puede arreglarlos


#region_2<-mask(results$SST, cluster_2)
#plot(region_2)



##### Para utilizar este tipo de analisis se debe crear una variable columna 
#res.dmfa = DMFA ( iris, num.fact = 5)
#summary(res.dmfa)
#dim(res.dmfa$var$coord[,1:3])


### Prueba para verificar las coordenadas que se debian utilizar
#res<-MFA(t(iris[,-5]), group=c(50,50,50), type=rep("s",3),
#         ncp=5, name.group=c("setosa","versicolor","virginica"))
#dim(res$global.pca$ind$coord[,1:3])


setwd(paste(ruta,"/results_graphs/DMFA/",dep,sep=""))
prueba<-getwd()

region<-raster(paste(prueba,"/",substring(dep,1,4),"_", lead,".tif", sep = "")) # Lectura de un archivo raster para graficar
plot(region) # grafico


as.numeric(names(table(cluster_values[])))




num_clust<-list(1,2,3,4) #Declare en un objeto tipo lista los cluster de interes
# Cree un stack con las formas de los cluster
num_zone<-stack(sapply(num_clust, ext_by_clust, cluster_values))
# Cambie los nombres de los objetos del stack
names(num_zone)<-paste("cluster", num_clust, sep="_")
plot(num_zone) # Grafique los objetos








# num_zone ahora sera un stack, supongo por ahora que no tendra mas de 4 capas. 
DMFA_C<-function(dep, a, lead,lead_num, num_zone, results, answer){
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  #### Correción de las regiones predictoras
  num_zone
  results$SST
  
  Total_zone<-1:dim(num_zone)[3]
  
  # Primero hagamos la prueba para dos regiones (obvio lo repetimos)
    
  curts<-list()
  
  for(i in 1:length(Total_zone)){
    curts[[i]]<-mask(results$SST, num_zone[[i]])
    names(curts)[i]<-paste("region",i,sep="_")
  }

  
  # Creación de la matriz de zonas para el DAFM
  # En la primera columna se encuentra el idenfiticador de la region 
  if(length(curts)==2){ # 2 zonas 
    
    T_region_1<-cbind.data.frame(names(curts)[1],na.omit(curts$region_1[]))
    names(T_region_1)[1]="factor"
    T_region_2<-cbind.data.frame(names(curts)[2],na.omit(curts$region_2[]))
    names(T_region_2)[1]="factor"
    
    
    zonas<-rbind(T_region_1,T_region_2)
    
  } else if(length(curts)==3){## 3 zonas
    T_region_1<-cbind.data.frame(names(curts)[1],na.omit(curts$region_1[]))
    names(T_region_1)[1]="factor"
    T_region_2<-cbind.data.frame(names(curts)[2],na.omit(curts$region_2[]))
    names(T_region_2)[1]="factor"
    T_region_3<-cbind.data.frame(names(curts)[3],na.omit(curts$region_3[]))
    names(T_region_3)[1]="factor"
    
    zonas<-rbind(T_region_1,T_region_2,T_region_3)
    
  } else  if(length(curts)==4){ ## 4 zonas
    T_region_1<-cbind.data.frame(names(curts)[1],na.omit(curts$region_1[]))
    names(T_region_1)[1]="factor"
    T_region_2<-cbind.data.frame(names(curts)[2],na.omit(curts$region_2[]))
    names(T_region_2)[1]="factor"
    T_region_3<-cbind.data.frame(names(curts)[3],na.omit(curts$region_3[]))
    names(T_region_3)[1]="factor"
    T_region_4<-cbind.data.frame(names(curts)[4],na.omit(curts$region_4[]))
    names(T_region_4)[1]="factor"
    
    zonas<-rbind(T_region_1,T_region_2,T_region_3,T_region_3)
  } 

  ##### Para utilizar este tipo de analisis se debe crear una variable columna 
  res.dmfa = DMFA (zonas, num.fact = 1)
  #summary(res.dmfa)
  #dim(res.dmfa$var$coord[,1:3])
  
  #setwd(paste("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/results_graphs/", dep, "/",answer, "/", trim, "/", sep=""))
  
  
  setwd(paste(ruta, "/results_graphs/",sep=""))
  getwd()
  
  # No encuentro las contribuciones del grupo (todavia)
  #res.dmfa$group$cos2
  #res.dmfa$eig
  #res.dmfa$var$coord[,1:3]
  
  #Guarde el archivo con lso valores propios
  write.csv(res.dmfa$eig, file=paste("eigGroup",answer, trim,lead_num ,".csv", sep="_"))
  # Imprima el resumen del AFM
  
  
  ###### Guardado automatico de la grafica de valores propios
  png(file =paste("barEig",answer, trim, lead_num ,".png", sep="_"), bg = "transparent", width = 500, height = 300)
  barplot(res.dmfa$eig[,1],main=paste("Eigenvalues",dep,trim,"-",lead),names.arg=1:nrow(res.dmfa$eig))
  dev.off()
  
  
  # correlación entre las componentes y el indicador
  corr<-cbind.data.frame(cor1=cor(results$ind, res.dmfa$var$coord[,1]),
                         cor2=cor(results$ind, res.dmfa$var$coord[,2]),
                         cor3=cor(results$ind, res.dmfa$var$coord[,3]))
  
  
  
  
  
  
  # Realice un gráfico entre la primera componente y el indicador de la estación y agregue la correlación
  datos=as.data.frame(cbind(res.dmfa$var$coord[,1], results$ind))
  gp = ggplot(datos, aes(datos[,1], datos[,2])) +  geom_point(shape=19, size=3,colour="#CC0000")
  gp = gp +  geom_smooth(method=lm, colour="#000099") + labs(x="Primera Componente AFM",y="Precipitación (mm)") 
  gp = gp + geom_text(data = data.frame(), aes(max(datos[,1])-0.1, max(datos[,2])+10, label = paste("r = ", round(cor(datos[,1], datos[,2]),3), sep="")))
  gp = gp + theme_bw()
  gp
  # Realice un gráfico entre la segunda componente y el indicador de la estación y agregue la correlación
  datos2=as.data.frame(cbind(res.dmfa$var$coord[,2], results$ind))
  p2 = ggplot(datos2, aes(datos2[,1], datos2[,2])) +  geom_point(shape=19, size=3,colour="#CC0000")
  p2 = p2 +  geom_smooth(method=lm, colour="#000099") + labs(x="Segunda Componente AFM",y="Precipitación (mm)") 
  p2 = p2 + geom_text(data = data.frame(), aes(max(datos2[,1])-0.1, max(datos2[,2])+10, label = paste("r = ", round(cor(datos2[,1], datos2[,2]),3), sep="")))
  p2 = p2 + theme_bw()
  p2
  # Realice un gráfico entre la segunda componente y el indicador de la estación y agregue la correlación
  datos3=as.data.frame(cbind(res.dmfa$var$coord[,3], results$ind))
  p3 = ggplot(datos3, aes(datos3[,1], datos3[,2])) +  geom_point(shape=19, size=3,colour="#CC0000")
  p3 = p3 +  geom_smooth(method=lm, colour="#000099") + labs(x="Tercera Componente AFM",y="Precipitación (mm)") 
  p3 = p3 + geom_text(data = data.frame(), aes(max(datos3[,1])-0.1, max(datos3[,2])+10, 
                                               label = paste("r = ", round(cor(datos3[,1], datos3[,2]),3), sep="")))
  p3 = p3 + theme_bw()
  p3
 
  
  
  # Guarde las dos imagenes automaticamente
  tiff(paste(ruta,"/results_graphs/disp","_",answer, "_",trim,"_",lead_num,".tif",sep=""), height=300,width=600,res=80,
       compression="lzw") 
  grid.arrange(gp,p2, p3,ncol=3)
  dev.off()
  
  
  # Guarde las componentes en un archivo .csv
  write.csv(res.dmfa$var$coord, file=paste("componentes",answer, a, lead_num ,".csv", sep="_"))
  # Guarde lo que vaya a entregar en una lista que entrega, la correlaciónm, la contribución y las componentes
  entrega<-list(corr=corr, res_global.pca=res.dmfa$var$coord[,1:3])
  return(entrega)}









# num_zone ahora sera un stack, supongo por ahora que no tendra mas de 4 capas. 
DMFA_P<-function(dep, a, lead,lead_num, num_zone, results, answer){
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  #### Correción de las regiones predictoras
  num_zone
  results$SST
  
  Total_zone<-1:dim(num_zone)[3]
  
  # Primero hagamos la prueba para dos regiones (obvio lo repetimos)
  
  curts<-list()
  
  for(i in 1:length(Total_zone)){
    curts[[i]]<-mask(results$SST, num_zone[[i]])
    names(curts)[i]<-paste("region",i,sep="_")
  }
  
  
  # Creación de la matriz de zonas para el DAFM
  # En la primera columna se encuentra el idenfiticador de la region 
  if(length(curts)==2){ # 2 zonas 
    
    T_region_1<-cbind.data.frame(names(curts)[1],na.omit(curts$region_1[]))
    names(T_region_1)[1]="factor"
    T_region_2<-cbind.data.frame(names(curts)[2],na.omit(curts$region_2[]))
    names(T_region_2)[1]="factor"
    
    
    zonas<-rbind(T_region_1,T_region_2)
    
  } else if(length(curts)==3){## 3 zonas
    T_region_1<-cbind.data.frame(names(curts)[1],na.omit(curts$region_1[]))
    names(T_region_1)[1]="factor"
    T_region_2<-cbind.data.frame(names(curts)[2],na.omit(curts$region_2[]))
    names(T_region_2)[1]="factor"
    T_region_3<-cbind.data.frame(names(curts)[3],na.omit(curts$region_3[]))
    names(T_region_3)[1]="factor"
    
    zonas<-rbind(T_region_1,T_region_2,T_region_3)
    
  } else  if(length(curts)==4){ ## 4 zonas
    T_region_1<-cbind.data.frame(names(curts)[1],na.omit(curts$region_1[]))
    names(T_region_1)[1]="factor"
    T_region_2<-cbind.data.frame(names(curts)[2],na.omit(curts$region_2[]))
    names(T_region_2)[1]="factor"
    T_region_3<-cbind.data.frame(names(curts)[3],na.omit(curts$region_3[]))
    names(T_region_3)[1]="factor"
    T_region_4<-cbind.data.frame(names(curts)[4],na.omit(curts$region_4[]))
    names(T_region_4)[1]="factor"
    
    zonas<-rbind(T_region_1,T_region_2,T_region_3,T_region_3)
  } 
  
  ##### Para utilizar este tipo de analisis se debe crear una variable columna 
  res.dmfa = DMFA (zonas, num.fact = 1)

  # correlación entre las componentes y el indicador
  corr<-cbind.data.frame(cor1=cor(results$ind, res.dmfa$var$coord[,1]),
                         cor2=cor(results$ind, res.dmfa$var$coord[,2]),
                         cor3=cor(results$ind, res.dmfa$var$coord[,3]))
  
  
  corr_test<-cbind.data.frame(cor1=cor.test(results$ind, res.dmfa$var$coord[,1])$p.value,
                         cor2=cor.test(results$ind, res.dmfa$var$coord[,2])$p.value,
                         cor3=cor.test(results$ind, res.dmfa$var$coord[,3])$p.value)
 
  # Guarde lo que vaya a entregar en una lista que entrega, la correlaciónm, la contribución y las componentes
  entrega<-list(corr=corr, corr_test=corr_test, res_global.pca=res.dmfa$var$coord[,1:3])
  return(entrega)}







## Esta función realiza el modelo de regresión lineal multiple (o simple)
## Se seleccionan cuantas componentes entran a la modelación y automaticamente guarda información 
## relevante sobre el modelo como valores observados, pronosticados, gráfico de supuestos
## Tambien entrega en una lista indicadores como RMSE, R^2-adj, entre otros
## dep = departamento donde se encuentra la estación 
## lead = lead time con el que se pronostica ("MAM", "Feb")
## lead_num = lead time con el que se pronostica ("Sim", "0", "3")
## a = mes de incio del trimestre
## answer = estación variable de respuesta (ej = "Turipana", "Nataima", ...)
## num_zone = número de zonas con el que se realiza el AFM
## comp = vector de tamaño 3 de 0 y 1 (1 indica que esa componente va a ser utilizada en el modelo)
modelo<-function(dep,lead,lead_num, a, answer, num_zone){
  
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  
  # Con esta función se consiguen los valores de la SST y de la estación (answer) deseada 
  # (además de mapas de correlaciones e información departamental)
  results<-correlation(dep, lead, a,answer)
  # Esta función corre el AFM y entrega las componentes para ingresar al modelo
  MFA<-DMFA_P(dep, a, lead,lead_num, num_zone, results, answer)
  
  comp<-ifelse(MFA$corr_test<0.05,1,0)
  
  print(comp)
  
  # En este condicional se ajusta el modelo de regresión lineal
  # dependiendo de los valores del vector cop se decide que tipo de modelo se ajsuta
  # es decir con cuantas y cuales componentes
  # demolo de la forma lm(results$ind~MFA$componentes)
  if(comp[1]==1 & comp[2]==1 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,1]+MFA$res_global.pca[,2]+MFA$res_global.pca[,3])
  }else if(comp[1]==1 & comp[2]==1 & comp[3]==0){
    modelo<-lm(results$ind~MFA$res_global.pca[,1]+MFA$res_global.pca[,2])
  }else if(comp[1]==1 & comp[2]==0 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,1]+MFA$res_global.pca[,3])
  }else if(comp[1]==1 & comp[2]==0 & comp[3]==0){
    modelo<-lm(results$ind~MFA$res_global.pca[,1])
  }else if(comp[1]==0 & comp[2]==1 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,2]+MFA$res_global.pca[,3])
  }else if(comp[1]==0 & comp[2]==1 & comp[3]==0){
    modelo<-lm(results$ind~MFA$res_global.pca[,2])
  }else if(comp[1]==0 & comp[2]==0 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,3])
  }else if(comp[1]==0 & comp[2]==0 & comp[3]==0){
    print(modelo<-"ERRORRR !!!")
  }
  
  
  ### Cambia el directorio de guardado de los archivos
  setwd(paste(ruta, "/results_graphs/", sep=""))
  
  
  adj_r_squared=summary(modelo)$adj.r.squared # Estrae el adj_r_squared del modelo
  # A  partir del estadístico F se calcula el valor_p del modelo
  p_value<-1-pf(summary(modelo)$fstatistic[1], summary(modelo)$fstatistic[2], summary(modelo)$fstatistic[3])
  summary(modelo) # imprima el resumen del modelo
  
  # Guarde el resumen gráfico de los supuestos
  tiff(paste(ruta,"/results_graphs/Csup_", answer,"_",a,"_",lead_num,".tif",sep=""), height=450,width=700,res=100,
       compression="lzw") 
  layout(matrix(1:6,2)) # ponga todos los gráficos en el display (dividido)
  plot(modelo) # Realiza el resumen gráfico del modelo
  # Correlogramas para verificar el supuesto de independencia (temporal)
  acf(modelo$residuals)
  pacf(modelo$residuals) 
  dev.off()
  
  # Guarde otros indicadores relevantes tales como la media, el intercepto, y las pruebas sobre los supestos
  # tambien el RMSE y la correlación de pearson entre los obs y lo pronsticado
  media<-mean(results$ind) 
  Intercept<-modelo$coefficients[1]
  var<-bptest(modelo)$p.value # igualdad de varianza
  nor<-shapiro.test(modelo$residuals)$p.value # normalidad
  box<-Box.test (modelo$residuals, lag = 5)$p.value # independencia
  cor_b<-round(cor(results$ind, modelo$fitted.values),3)
  RMSE_b<-round(sqrt(sum((modelo$fitted.values-results$ind)^2)/length(results$ind)),3)
  
  # Cree un data frame con todos los indicadores y sumele el nú
  summary_m<-data.frame(media,Intercept,adj_r_squared,p_value, var, nor, box, RMSE_b, num_coef=length(modelo$coefficients), row.names = NULL)
  
  # Guarde automaticamente en un archivo .csv lo obervado, pronosticado y los residuales
  results_model<-cbind.data.frame(obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals)
  write.csv(results_model, paste("CresultsModel_", answer,"_",a,"_",lead_num,".csv",sep=""))
  
  # Guarde automaticamente un gráfico de dispersión entre lo observado y lo pronosticado
  ajuste=ggplot(data.frame(), aes(results$ind, modelo$fitted.values)) +  geom_point(shape=19, size=3,colour="#CC0000")
  ajuste=  ajuste +  geom_smooth(method=lm, colour="#000099") + labs(x="Precipitación obs (mm)",y="Precipitación pron (mm)") + theme_bw()
  ajuste=  ajuste +  geom_text(data = data.frame(), aes(max(results$ind)-70, min(modelo$fitted.values)+60, label = paste("r = ", round(cor(results$ind, modelo$fitted.values),3), sep="")))
  ajuste=  ajuste +  geom_text(data = data.frame(), aes(max(results$ind)-70, min(modelo$fitted.values)+10, label = paste("RMSE = ", round(sqrt(sum((modelo$fitted.values-results$ind)^2)/length(results$ind)),3), sep="")))
  
  tiff(paste(ruta,"/results_graphs/Cajuste_", answer,"_",a,"_",lead_num,".tif",sep=""), height=450,width=700,res=100,
       compression="lzw") 
  print(ajuste)
  dev.off()
  
  # Cree una lista donde se guarde los indicadores del modelo, lo observado, lo pronosticado y los residuales del modelo, tambien se almacenan las coefficientes
  entrega<-list(comp=comp, summary_m=summary_m, obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals,  coeff=modelo$coefficients)
  
  # Devuelva la lista 
  return(entrega)}






### Modelo parcial 

## Esta función es similar a la anterior, con la diferencia que no guarda auntomaticamente
## los archivos
modelop<-function(dep,lead,lead_num, a, answer, num_zone){
  
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  
  # Con esta función se consiguen los valores de la SST y de la estación (answer) deseada 
  # (además de mapas de correlaciones e información departamental)
  results<-correlation(dep, lead, a,answer)
  # Esta función corre el AFM y entrega las componentes para ingresar al modelo
  MFA<-DMFA_P(dep, a, lead,lead_num, num_zone, results, answer)
  
  comp<-ifelse(MFA$corr_test<0.1,1,0)

  
  # En este condicional se ajusta el modelo de regresión lineal
  # dependiendo de los valores del vector cop se decide que tipo de modelo se ajsuta
  # es decir con cuantas y cuales componentes
  # demolo de la forma lm(results$ind~MFA$componentes)
  if(comp[1]==1 & comp[2]==1 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,1]+MFA$res_global.pca[,2]+MFA$res_global.pca[,3])
  }else if(comp[1]==1 & comp[2]==1 & comp[3]==0){
    modelo<-lm(results$ind~MFA$res_global.pca[,1]+MFA$res_global.pca[,2])
  }else if(comp[1]==1 & comp[2]==0 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,1]+MFA$res_global.pca[,3])
  }else if(comp[1]==1 & comp[2]==0 & comp[3]==0){
    modelo<-lm(results$ind~MFA$res_global.pca[,1])
  }else if(comp[1]==0 & comp[2]==1 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,2]+MFA$res_global.pca[,3])
  }else if(comp[1]==0 & comp[2]==1 & comp[3]==0){
    modelo<-lm(results$ind~MFA$res_global.pca[,2])
  }else if(comp[1]==0 & comp[2]==0 & comp[3]==1){
    modelo<-lm(results$ind~MFA$res_global.pca[,3])
  }else if(comp[1]==0 & comp[2]==0 & comp[3]==0){
    print(modelo<-"ERRORRR !!!")
  }
  
  
 
  adj_r_squared=summary(modelo)$adj.r.squared # Estrae el adj_r_squared del modelo
  # A  partir del estadístico F se calcula el valor_p del modelo
  p_value<-1-pf(summary(modelo)$fstatistic[1], summary(modelo)$fstatistic[2], summary(modelo)$fstatistic[3])
  summary(modelo) # imprima el resumen del modelo
 
  # Guarde otros indicadores relevantes tales como la media, el intercepto, y las pruebas sobre los supestos
  # tambien el RMSE y la correlación de pearson entre los obs y lo pronsticado
  media<-mean(results$ind) 
  Intercept<-modelo$coefficients[1]
  var<-bptest(modelo)$p.value # igualdad de varianza
  nor<-shapiro.test(modelo$residuals)$p.value # normalidad
  box<-Box.test (modelo$residuals, lag = 5)$p.value # independencia
  cor_b<-round(cor(results$ind, modelo$fitted.values),3)
  RMSE_b<-round(sqrt(sum((modelo$fitted.values-results$ind)^2)/length(results$ind)),3)
  
  # Cree un data frame con todos los indicadores y sumele el nú
  summary_m<-data.frame(media,Intercept,adj_r_squared,p_value, var, nor, box, RMSE_b, num_coef=length(modelo$coefficients), row.names = NULL)
  
  # Guarde automaticamente en un archivo .csv lo obervado, pronosticado y los residuales
  results_model<-cbind.data.frame(obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals)
 
  # Cree una lista donde se guarde los indicadores del modelo, lo observado, lo pronosticado y los residuales del modelo, tambien se almacenan las coefficientes
  entrega<-list(comp=comp, summary_m=summary_m, obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals,  coeff=modelo$coefficients)
  
  # Devuelva la lista 
  return(entrega)}

pruebas<-modelop(dep,lead,lead_num, a, answer, num_zone)

pruebas




###### Revisar como aplicar esto en el codigo de DAFM
probabilities=function(fores,Y,sd_s){
  
  terciles=apply(Y,2,function(x)quantile(x,c(1/3,2/3),type=6))
  below=(pnorm(terciles[1,],fores,sd_s))*100
  normal=(pnorm(terciles[2,],fores,sd_s)-pnorm(terciles[1,],fores,sd_s))*100
  above=(1-pnorm(terciles[2,],fores,sd_s))*100
  id=substr(names(Y),2,nchar(names(Y)))
  prob_output=cbind.data.frame(id,below,normal,above)
  
  return(prob_output)
}




#################### Validación Cruzada

## Esta función realiza la validación cruzada del modelo
## quita periodos de tiempo de 5 años, despues ajusta el modelo
## realiza el pronostico de esos 5 años y los almancena.
## por último realiza la correlación de kendall entre el conjunto de las series prontosicadas
## y la observada. 

## dep = departamento donde se encuentra la estación 
## lead = lead time con el que se pronostica ("MAM", "Feb")
## lead_num = lead time con el que se pronostica ("Sim", "0", "3")
## a = mes de incio del trimestre
## answer = estación variable de respuesta (ej = "Turipana", "Nataima", ...)
## num_zone = número de zonas con el que se realiza el AFM
## comp = vector de tamaño 3 de 0 y 1 (1 indica que esa componente va a ser utilizada en el modelo)





cross_validation <-function(dep,lead,lead_num, a, answer, num_zone){
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  
  results<-correlation(dep, lead, a,answer) # corre lo mapas de correlaciones y devuelve
  # la información de la estación y la sst (trimestral)
  MFA<-DMFA_P(dep, a, lead, lead_num, num_zone, results, answer) ## Corre la información del AFM
  
  comp<-ifelse(MFA$corr_test<0.1,1,0)
  
  
  # Crea una matriz con las posiciones que se deben eliminar en cada validación. 
  j=matrix(c(1:30),5,6, byrow = F) ## Matriz de posiciones
  forecasts=matrix(c(1:30),5,6, byrow = F) ## Matriz en blanco de pronostico
  
  
  ## Crea una matriz para almacenar los coeficientes de regresión
  coef<-matrix(c(1:(sum(comp)+1)),6,(sum(comp)+1), byrow = T)
  z<-cbind.data.frame(y=results$ind, x1=MFA$res_global.pca[,1], x2=MFA$res_global.pca[,2], x3=MFA$res_global.pca[,3])
  
  
  # Realiza la validación cruzada 
  for(i in 1:6){
    
    results_i<-list(ind=results$ind[-j[,i]], SST=results$SST[[-j[,i]]])
    
    MFA1<-DMFA_P(dep, a, lead, lead_num, num_zone, results_i, answer) ## Corre la información del AFM
    # Crea un data frame con los resultados y las componentes 
    data<-cbind.data.frame(y=results_i$ind, x1=MFA1$res_global.pca[,1], x2=MFA1$res_global.pca[,2], x3=MFA1$res_global.pca[,3])
    
    
    # condicionando por las diferentes combinaciones de parametros del modelo
    # corre el modelo
    # almacena los pronosticos de las posiciones quitadas
    # alamacena los coeficientes del modelo en cada caso 
    if(comp[1]==1 & comp[2]==1 & comp[3]==1){ 
      modelo<-lm(data$y ~ data$x1 + data$x2 + data$x3)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]+modelo$coefficients[3]*z[j[,i],3]+modelo$coefficients[4]*z[j[,i],4]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==1 & comp[2]==1 & comp[3]==0){
      modelo<-lm(data$y ~data$x1 + data$x2)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]+modelo$coefficients[3]*z[j[,i],3]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==1 & comp[2]==0 & comp[3]==1){
      modelo<-lm( data$y ~data$x1 + data$x3)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]+modelo$coefficients[3]*z[j[,i],3]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==1 & comp[2]==0 & comp[3]==0){
      modelo<-lm(data$y ~data$x1)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==1 & comp[3]==1){
      modelo<-lm(data$y ~ data$x2 + data$x3)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]+modelo$coefficients[3]*z[j[,i],3]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==1 & comp[3]==0){
      modelo<-lm(data$y ~ data$x2)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==0 & comp[3]==1){
      modelo<-lm(data$y ~ data$x3)
      forecasts[,i]=modelo$coefficients[1]+modelo$coefficients[2]*z[j[,i],2]
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==0 & comp[3]==0){
      print(modelo<-"ERRORRR !!!")
    }
  }
  
  
  forecast=as.numeric(forecasts) ### La matriz de pronosticos conviertala en vector
  forecast[forecast<0]=0 ## Todos los pronosticos negativos son iguales a 0 por definición de la precipitación.
  
  
  # se hace un data frame con los datos observados y los prontosicados
  cv<-cbind.data.frame(obs=results$ind[1:30], forecasts= forecast)
  
  # se calcula el goodness index que corresponde al coeficiente de correlación
  # de Pearson
  kendall<-cor(cv$obs, cv$forecasts, method = "kendall")
  pearson<-cor(cv$obs, cv$forecasts, method = "pearson")
  spearman<-cor(cv$obs, cv$forecasts, method = "spearman")
  
  RMSE <- sqrt(sum((cv$forecasts-cv$obs)^2)/length(cv$obs)) # Total
  RMSE_porc <- (RMSE/mean(cv$obs))*100
  
  
  ########### Curva ROC
  cv<-cv[order(cv$obs),]
  q_obs<-quantile(cv$obs, c(0.33,0.66))
  below_o<-ifelse(cv$obs<q_obs[1],1,0)
  
  q_pron<-quantile(cv$forecasts, c(0.33,0.66)) 
  below_p<-ifelse(cv$forecasts<q_pron[1],1,0)
  
  
  table(below_p, below_o)
  below<-cbind.data.frame(cv$obs,cv$forecasts, below_o, below_p)
  
  roc_below<-roc(response = below$below_o, predictor = below$`cv$forecasts`, smooth=F)
  auc_below<-auc(roc_below)
  #plot(roc_below, col="red",grid=TRUE)
  
  roc_below<-roc(response = below$below_o, predictor = below$`cv$forecasts`, smooth=F)
  auc_below<-auc(roc_below)
  
  
  above_o<-ifelse(cv$obs>q_obs[2],1,0)
  above_p<-ifelse(cv$forecasts>q_pron[2],1,0)
  
  
  table(above_p, above_o)
  above<-cbind.data.frame(cv$obs,cv$forecasts, above_o, above_p)
  
  roc_above<-roc(response = above$above_o, predictor = above$`cv$forecasts`, smooth=F)
  auc_above<-auc(roc_above)
  #plot(roc_below, col="red",grid=TRUE)
  
  
  
  indi=data.frame(kendall=kendall,pearson=pearson, spearman=spearman, RMSE=RMSE,  RMSE_porc= RMSE_porc, roc_below=auc_below , roc_above=auc_above)
  
  #  Realice una lista donde se almacenan todos los resultados
  resultados=list(cv=cv, kendall=kendall, coeficientes=coef, indicadores=indi) ## Almacene los pronosticos y los resumenes de los modelos en una lista
  
  
  # Directorio en el cual se guardan los archivos. 
  setwd(paste(ruta,"/results_graphs/",sep=""))
  
  
  #paste(ruta,"/results_graphs/", dep,"/", answer, "/", trim, "/modelo/",sep="")
  
  
  tiff(paste("ROC", answer, a, lead_num, ".tif",sep="_"), width = 620, height = 620, res=100,
       compression="lzw")
  plot(1-roc_below$specificities, roc_below$sensitivities,  type="l", col="red3", 
       ylab="Hit rate", xlab="False-alarm rate", lwd=2, main="Relative Operating Characteristics")
  abline(a=0, b=1, lwd=2)
  lines(1-roc_above$specificities, roc_above$sensitivities, lwd=2,  col="blue")
  legend(0.65,0.12, lwd=c(2,2), legend =c(paste("Below(",auc_below ,")",sep=""), paste("Above(",auc_above ,")",sep="")), col=c("red3","blue"))
  dev.off()
  
  
  # Almacene un archivo con la validación cruzada y otro con los parametros
  write.csv(cv, file = paste("cv_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  write.csv(coef, file = paste("coef_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  write.csv(indi, file = paste("ind_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  
  # retorne la lista de resultados.
  return(resultados)}

lead_num<-3


GI<-0
mo_p<-0
i=1
#for(i in 1:dim(tab_comp)[1]){
  GI[i]<-cross_validation(dep,lead[i],lead_num[i], a[i], answer, num_zone)$kendall
  data<-cbind.data.frame(dep, answer,a[i], lead_num[i], dim(num_zone)[3][i],GI[i])
  mo_p=rbind(mo_p, data)  
#}

#mop<-mo_p[-1, ]
mop<-rbind(mop,mo_p[-1, ])
mop



setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()
# Almacene el archivo
#write.csv(mop, file = "summary_models_cv.csv", row.names = TRUE)












### Validación Retrospectiva

## Esta función realiza la validación retrospectiva del modelo
## Elimina un periodo de años y luego pronostica el siguiente
## es decir, incialmente toma como periodo de entrenamiento desde 1982 hasta 2005 para pronosticar 2006
## despues ajusta el modelo con el periodo 1982 a 2006 para pronosticar 2007, así sucesivamente
## hasta llegar a 2013. 

## dep = departamento donde se encuentra la estación 
## lead = lead time con el que se pronostica ("MAM", "Feb")
## lead_num = lead time con el que se pronostica ("Sim", "0", "3")
## a = mes de incio del trimestre
## answer = estación variable de respuesta (ej = "Turipana", "Nataima", ...)
## num_zone = número de zonas con el que se realiza el AFM
## comp = vector de tamaño 3 de 0 y 1 (1 indica que esa componente va a ser utilizada en el modelo)


restrospective_validation<-function(dep,lead,lead_num, a, answer, num_zone){
  
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  results<-correlation(dep, lead, a,answer) # corre lo mapas de correlaciones y devuelve
  # la información de la estación y la sst (trimestral)
  
  
  # Condiciona la cantidad de pronosticos de acuerdo al trimestre
  # puesto que DEF posee menos observaciones, el año de referencia se toma como año al 
  # que pertenece el mes central del periodo. 
  if(a == 12 | lead== "MAM_Nov"| lead== "MAM_Sep"| lead== "JJA_Dec"){
    length_r=2013-2005 # número de años a pronosticar
    year<-1983:2013 # años a pronosticar
    # Crea una matriz para almacenar los pronosticos. 
    pron_year1<-2006:2013 # caso 2
    pron_year<-2005:2012 # caso 1 
    forecasts=matrix(which(year>2005),length_r,1, byrow = F) ## Matriz en blanco de pronostico
  } else if(a != 12 & lead!= "MAM_Nov"& lead!= "MAM_Sep"& lead!= "JJA_Dec") {
    length_r=2013-2004
    year<-1982:2013
    pron_year1<-2005:2013 # caso 2
    pron_year<-2004:2012 # caso 1 
    forecasts=matrix(which(year>2004),length_r,1, byrow = F) ## Matriz en blanco de pronostico
  } 
  
  # crea una matriz para almacenar los coeficientes de regresión de todas las iteraciones
  coef<-matrix(c(1:(sum(comp)+1)),length_r,(sum(comp)+1), byrow = T)
  
  ###################################################################
  
  # Realiza la validación retrospectiva
  for(i in 1:length_r){ # repita esto, de acuerdo a la cantidad de prontosicos
    
    j=matrix(which(year>pron_year[i]),length(which(year>pron_year[i])),1,
             byrow = F) ## Matriz de posiciones
    results_i<-list(ind=results$ind[-j], SST=results$SST[[-j]])
    
    MFA1<-DMFA_P(dep, a, lead,lead_num, num_zone, results_i, answer) ## Corre la información del AFM
    # Crea un data frame con los resultados y las componentes 
    data<-cbind.data.frame(y=results_i$ind, x1=MFA1$res_global.pca[,1], 
                           x2=MFA1$res_global.pca[,2], x3=MFA1$res_global.pca[,3])
    
    
    q=matrix(which(year>pron_year1[i]),length(which(year>pron_year1[i])),1,
             byrow = F) ## Matriz de posiciones
    print(cbind(i, t(year[year>=pron_year1[i]])))
    if(i != 8){
      results_q<-list(ind=results$ind[-q], SST=results$SST[[-q]])
    }else if(i == 8 & length_r==9){
      results_q<-list(ind=results$ind[-q], SST=results$SST[[-q]])
    }else if(i == 8 & length_r==8){ 
      results_q<-list(ind=results$ind, SST=results$SST)}
    if(i == 9 & length_r==9){ results_q<-list(ind=results$ind, SST=results$SST)} 
    
    
    
    MFA2<-DMFA_P(dep, a, lead,lead_num, num_zone, results_q, answer) ## Corre la información del AFM
    z<-cbind.data.frame(y=results_q$ind, x1=MFA2$res_global.pca[,1],
                        x2=MFA2$res_global.pca[,2], x3=MFA2$res_global.pca[,3])
    
    eval<-z[forecasts[i],] # Evalue solo la posición que se va a prontosicar (año objetivo)
    
    print(eval)
    
    if(comp[1]==1 & comp[2]==1 & comp[3]==1){
      modelo<-lm(data$y ~ data$x1 + data$x2 + data$x3)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x1+modelo$coefficients[3]*eval$x2+modelo$coefficients[4]*eval$x3
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==1 & comp[2]==1 & comp[3]==0){
      modelo<-lm(data$y ~data$x1 + data$x2)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x1+modelo$coefficients[3]*eval$x2
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==1 & comp[2]==0 & comp[3]==1){
      modelo<-lm( data$y ~data$x1 + data$x3)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x1+modelo$coefficients[3]*eval$x3
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==1 & comp[2]==0 & comp[3]==0){
      modelo<-lm(data$y ~data$x1)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x1
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==1 & comp[3]==1){
      modelo<-lm(data$y ~ data$x2 + data$x3)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x2+modelo$coefficients[3]*eval$x3
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==1 & comp[3]==0){
      modelo<-lm(data$y ~ data$x2)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x2
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==0 & comp[3]==1){
      modelo<-lm(data$y ~ data$x3)
      forecasts[i,]=modelo$coefficients[1]+modelo$coefficients[2]*eval$x3
      coef[i,]<-modelo$coefficients
    }else if(comp[1]==0 & comp[2]==0 & comp[3]==0){
      print(modelo<-"ERRORRR !!!")
    } 
    
  }
  forecasts
  
  forecasts=as.numeric(forecasts) ### La matriz de pronosticos conviertala en vector
  forecasts[forecasts<0]=0 ## Todos los pronosticos negativos son iguales a 0 por definición de la precipitación.
  
  
  
  
  # cree una trama de datos con el año, la observación y lo pronosticado
  retro<-cbind.data.frame(year =year[which(year>=pron_year1[1])] , obs=results$ind[which(year>=pron_year1[1])], forecasts= forecasts)
  
  # calcule el goodness index
  kendall<-cor(results$ind[which(year>=pron_year1[1])], forecasts, method = "kendall")
  
  
  
  # cree una lista con los resultados
  resultados=list(kendall=kendall, retro= retro, coeficientes=coef) ## Almacene lso pronosticos y los resumenes de los modelos en una lista
  
  # cambie el directorio donde se guardaran los archivos.
  #setwd(paste(ruta,"/results_graphs/", dep, "/", answer, "/", trim,"/modelo/",sep=""))
  setwd(paste(ruta,"/results_graphs/",sep=""))
  
  write.csv( retro, file = paste(" retro_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  write.csv(coef, file = paste("coef_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  
  # Retorne la lista de resultados.
  return(resultados)}

# Declare las variables de la función 
dep<-"santander"
answer<-"StaIsabel"
lead<-"DEF_Aug"
lead_num<-3
a<-12

# Vuelva al espacio de trabajo original 
setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()






# Corra la función y almacene el goodness index
GI_r<-0
mo_pr<-0
i=1
#for(i in 1:dim(tab_comp)[1]){
  GI_r[i]<-restrospective_validation(dep,lead,lead_num, a, answer, num_zone)$kendall
  datar<-cbind.data.frame(dep, answer,a[i], lead_num[i], num_zone[i],GI_r[i])
  mo_pr=rbind(mo_pr, datar)  
#}


mopr<-rbind(mopr,mo_pr[-1, ])
mopr




setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()


# Almacene el archivo
#write.csv(mop, file = "summary_models_retro.csv", row.names = TRUE)













