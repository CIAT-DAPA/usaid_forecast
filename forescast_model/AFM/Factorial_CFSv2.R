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
###### Corridas preliminares para determinar la región predictora
########################




### Nombre de las estaciones y sitios de estudio

estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima","Turipana")
sitios<-c("Lorica","Yopal","Ibagué","LaUnion","Espinal","Cereté")
cbind(estaciones, sitios)



# Declare los argumentos de las funciones
dep="valle"
answer="CentAdmoLaUnion"
lead_num<-rep(c(0,3,5), 4)

# Timestre
a <-rep(seq(3,12,3), each = 3)

# Lead times (nombres)
lead<-c(paste(rep("MAM",3),c("Feb","Nov","Sep"), sep="_"),
        paste(rep("JJA",3),c("May","Feb","Dec"), sep="_"),
        paste(rep("SON",3),c("Aug","May","Mar"), sep="_"),
        paste(rep("DEF",3),c("Nov","Aug","Jun"), sep="_"))






##### Función para gráficary guardar automaticamente 
graphs<- function(results, file){
  
  # grafico de correlaciones de sperman y pearson
  complete<-levelplot(results$maps, par.settings=myTheme) 
  
  # Recorta solamente las regiones que cumple con las condiciones abs(corr)>0.5
  cortes<-list()
  cortes[[1]]<-extract_function(results$maps$Pearson)
  cortes[[2]]<-extract_function(results$maps$Spearman)
  cortes<-stack(cortes)
  names(cortes)=c("Pearson", "Spearman")
  # Grafica los cortes anteriores
  curts<-levelplot(cortes, par.settings=myTheme1) + levelplot(map, par.settings=myTheme2)
  
  # Guardado automatico de las imagenes
  tiff(paste(ruta, "/results_graphs/", dep, "/", answer, "/cor",file,".tif",sep=""), height=650,width=1000,res=100,
       compression="lzw") 
  print(grid.arrange(complete,curts,ncol=1))
  dev.off()
}

# Corre automaticamente los graficos de correlacion para
for(i in 1:12){ #Corra para todos los lead times
  # Corra los mapas de correlaciones.
  results<-correlation(dep, lead[i], a[i], answer)
  # Nombre del archivo
  file<-paste(answer,"_", a[i], "_", lead_num[i], sep="")
  # Corra los graficos y guardelos automaticamente
  graphs(results, file)
}  






### Graficos para todas las estaciones - All stations


# Declare los argumentos de las funciones
dep<-"valle"
# Lead times (nombres)
lead<-c(paste(rep("MAM",3),c("Feb","Nov","Sep"), sep="_"),
        paste(rep("JJA",3),c("May","Feb","Dec"), sep="_"),
        paste(rep("SON",3),c("Aug","May","Mar"), sep="_"),
        paste(rep("DEF",3),c("Nov","Aug","Jun"), sep="_"))
lead_num<-rep(c(0,3,5), 4)
a <-rep(seq(3,12,3), each = 3)


### Para guardar todos los gráficos de todos los mapas de correlaciones de las estaciones
for(i in 1:length(a)){ # corra para todos los lead
  
  # corra los mapas de correlaciones para todas las estaciones 
  all_stations<-sta_complete(dep, lead[i], a[i])
  # nombre del archivo
  file<-paste(dep,"_", a[i], "_", lead_num[i], sep="")
  
  # guardado automatico en .tif de los mapas del coeficiente de correlación de pearson completos
  tiff(paste(ruta, "/results_graphs/",dep,"/corEcp",file,".tif",sep=""), height=650,width=1000,res=100,
       compression="lzw") 
  print(levelplot(all_stations$cor_pear, par.settings=myTheme))
  dev.off()
  

  
  # guardado automatico en .tif de los mapas del coeficiente de correlación de spearman completos
  tiff(paste(ruta, "/results_graphs/",dep,"/corEcs",file,".tif",sep=""), height=650,width=1000,res=100,
       compression="lzw") 
  print(levelplot(all_stations$cor_spe, par.settings=myTheme))
  dev.off()
  
  
  # guardado automatico en .tif de los mapas de  los cortes de las regiones
  # inflyentes con la correlacion de pearson para todas las estataciones
  tiff(paste(ruta, "/results_graphs/",dep,"/corEcp_curt",file,".tif",sep=""), height=650,width=1000,res=100,
       compression="lzw") 
  print(levelplot(all_stations$cut_p, par.settings=myTheme1) + levelplot(map, par.settings=myTheme2))
  dev.off()
  
  
  
  
  # guardado automatico en .tif de los mapas de  los cortes de las regiones
  # inflyentes con la correlacion de spearman para todas las estataciones
  tiff(paste(ruta, "/results_graphs/",dep,"/corEcs_curt",file,".tif",sep=""), height=650,width=1000,res=100,
       compression="lzw") 
  print(levelplot(all_stations$cut_s, par.settings=myTheme1) + levelplot(map, par.settings=myTheme2))
  dev.off()
}








##### Grafica las zonas mas frecuentes (con alta correlacion para todo el departamento)
dep<-c("casanare","cordoba","tolima","valle")
for(j in 1:4){ # corra para los cuatro departamentos
  for(i in 1:length(a)){ #para todos los trimestres y lead (en total el vector tiene 12 posiciones)
    #corra los mapas de correlaciones para todas las estaciones del departamento
    all_stations<-sta_complete(dep[j], lead[i], a[i])
    # Recorte de las zonas mas frecuentes
    areas<-stack(all_stations$areas_p, all_stations$areas_s) # stack de las zonas
    names(areas)=c("Pearson", "Spearman") # titulos de cada raster
    file<-paste(dep[j],"_", a[i], "_", lead_num[i], sep="") # nombre del archivo
    # Guardado automatico de las zonas más frecuentes en .tif
    tiff(paste(ruta, "/results_graphs/", dep[j],"/areaps",file,".tif",sep=""), height=650,width=1000,res=100,
         compression="lzw") 
    print(levelplot(areas, par.settings=myThemec)) # grafico
    dev.off()
  }
}







#################################################################################
#################################################################################
######################## Seleccion de las areas predictoras
#################################################################################
#################################################################################




# Se comienza con la automatizacion cuando se comente el codigo




## Parametros con los que se corre la función 
dep="cordoba"
lead="DEF_Aug" 
a=12
# Solo se corrio la función para los modelos que CPT selecciono como los mejores modelos
all_stations<-sta_complete(dep, lead, a)



# Graficas para seleccionar las regiones predictoras
layout(matrix(c(1,2), ncol=2))
plot(all_stations$areas_p, colNA="gray30", col=jet.colors(20), main="Pearson") 
plot(all_stations$areas_s, colNA="gray30", col=jet.colors(20), main="Spearman") 
graphics.off()

plot(all_stations$areas_s+all_stations$areas_p, colNA="gray30", col=jet.colors(20), main="Spearman + Pearson") 

### Con estas funciones se puede realizar el area predictora solo dando click encima de la región deseada
e<-drawExtent()
e<-as.numeric(as.character(e))

#region=crop(results$SST, e)
f<-drawExtent(col="blue")
f<-as.numeric(as.character(f))
#region_f<-crop(results$SST, f)
g<-drawExtent(col="chartreuse4")
g<-as.numeric(as.character(g))




h<-drawExtent(col="darkviolet")
h<-as.numeric(as.character(h))



j<-drawExtent(col="pink")


rbind(t(e),t(f),t(g),t(h))

########





library("FactoClass")

## Parametros con los que se corre la función 
dep="cordoba"
lead="DEF_Nov" 
a=12
answer="DoctrinaLa"
# Solo se corrio la función para los modelos que CPT selecciono como los mejores modelos
all_stations<-sta_complete(dep, lead, a)




all_stations$cor_pear[][which(abs(all_stations$cor_pear)[]<0.5)]=0



plot(all_stations$cor_pear)



cluster <- cbind(rasterToPoints(all_stations$cor_pear), freq=rasterToPoints(all_stations$areas_p)[,3])
#filtro<-cluster[which(cluster$freq>0.2*34),]

#cluster<-rasterToPoints(all_stations$areas_p)
cluster1<-FactoClass( cluster , dudi.pca, k.clust = 4,
            scanFC = FALSE,  nfcl = 10)   

cluster1$carac.cont

library(dplyr)
head(cluster)
cluster_df <- data.frame(cluster, ind_cluster = cluster1$cluster)

summary_cluster <- cluster_df %>%
  group_by(ind_cluster) %>%
  summarise_each(funs(mean)) %>%
  as.data.frame()

summary_cluster



#### Variación de las estaciones

#### Correlación de Pearson 
ocean=which(!is.na(all_stations$areas_p[])) # tome las posiciones en las que la variable sea diferente de NA

cluster_values<-all_stations$areas_p
plot(cluster_values)
cluster_values[ocean]<-cluster1$cluster
plot(cluster_values, col=c("red","blue", "green", "orange"))


table(cluster1$cluster)




ocean=which(cluster_values[]==2)#  tome las posiciones en las que la variable sea diferente de NA

cluster_1<-cluster_values
cluster_1[]=NA
cluster_1[ocean]<-1
plot(cluster_1)


results<-correlation(dep, lead, a,answer)
region_2<-mask(results$SST, cluster_1)




dim(region_2)



































###### 




















########

## Lea la tabla con las regiones predictoras seleccionadas
tabla=read.table("clipboard",header = T)


### Esta función realiza los gráficos de las areas predictoras
### el objeto fila es en el cual se encuentran las coordenadas de las areas predictoras
areas_graph<-function(fila){
  ## Cree el grafico en ggplot (raster donde solo se gráfica con color los continentes)
  plot_areas<-gplot(all_stations$areas_p) + geom_tile(aes(fill = value)) +
    coord_equal() + theme_bw()+theme(legend.position="none", panel.grid.major = element_line(colour = "white")) + labs(x="Long", y="Lat")+
    scale_fill_gradient2(low="white",mid = "white", high="white",name = " ") 
  
  ## Gráfique como rectangulos las areas predictoras sobre el mapa
  plot_areas <- plot_areas + geom_rect(xmin = tabla[fila,4], xmax = tabla[fila,5], ymin = tabla[fila,6], ymax = tabla[fila,7],fill="lightpink", alpha=0.2) #+ geom_text(data = data.frame(), aes(tabla[fila,5]+10, tabla[fila,7]+5, label = "1"))
  
  plot_areas <- plot_areas + geom_rect(xmin = tabla[fila,8], xmax = tabla[fila,9], ymin = tabla[fila,10], ymax = tabla[fila,11],fill="yellow", alpha=0.2) #+ geom_text(data = data.frame(), aes(tabla[fila,9]+10, tabla[fila,11]+5, label = "2"))
  
  plot_areas <- plot_areas + geom_rect(xmin = tabla[fila,12], xmax = tabla[fila,13], ymin = tabla[fila,14], ymax = tabla[fila,15],fill="slateblue", alpha=0.2) #+  geom_text(data = data.frame(), aes(tabla[fila,13]+10, tabla[fila,15]+5, label = "3"))
  
  plot_areas <- plot_areas + geom_rect(xmin = tabla[fila,16], xmax = tabla[fila,17], ymin = tabla[fila,18], ymax = tabla[fila,19],fill="yellowgreen", alpha=0.2) #+geom_text(data = data.frame(), aes(tabla[fila,17]+10, tabla[fila,19]+5, label = "4"))
  
  # plot_areas <- plot_areas + geom_rect(xmin = tabla[fila,21], xmax = tabla[fila,22], ymin = tabla[fila,23], ymax = tabla[fila,24],fill="darkturquoise", alpha=0.2) #+ geom_text(data = data.frame(), aes(tabla[fila,21]+10, tabla[fila,23]+5, label = "5"))
  
  #print(plot_areas)
  ## Guarde las areas predictoras
  
  setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/maps_correlation/")
  ggsave(paste(tabla[fila,2],"_",tabla[fila,1],"_",tabla[fila,3],".png", sep = ""),width =6 ,height =1.5,dpi=200 )
  print(paste(tabla[fila,2],"_",tabla[fila,1],"_",tabla[fila,3],".png", sep = ""))
}
fila=1:dim(tabla)[1] ## Cree vector de filas
for(i in 1:dim(tabla)[1]){areas_graph(i)} # Gráfique las filas




setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/")
getwd()



## Para generar la legenda
#df <- data.frame( x = 1,  y = rep(1:4), z = factor(rep(1:4)))
#ggplot(df, aes(x,y, fill=z)) +   geom_tile() + theme_classic() + theme_nothing()+
#scale_fill_manual(values= c("lightpink","yellow", "slateblue","yellowgreen"))+
#  geom_text(data = df, aes(1, 1:4, label = 1:4), size = 10)















####################################################################################


## Esta función permite realizar el AFM
## Esta configurada para una tabla con 20 columnas de 
## las regiones se encuentran entre las columnas 4 y 19
## Maximo se pueden ingresar 5 regiones predictoras en la función
## dep = departamento 
## a = inicio del trimestre
## lead = nombre del archivo lead time
## results = resultados de la función correlation 
## answer = estación variable de respuesta 
MFA_C<-function(dep, a, lead,lead_num, num_zone, results, answer){
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  ## nombre de la fila que se va a estudiar (departamento - trimestre donde se encuentra las estaciones)
  fila=paste(answer,trim,sep="_")
  regiones<-tabla[fila,names(tabla)[5:20]] # elimine las celdas que no corresponden a coordenadas
  regiones<-regiones[1:(4*num_zone)] #determine cuantas regiones se utilizaran
  
  ### AFM condicionado por el número de regiones
  if(num_zone==2){ # 2 zonas 
    # Cree la región predictora y corte la SST de acuerdo a la región
    region_1<-extent(as.numeric(regiones[1:4])); region_1=crop(results$SST, region_1)
    region_2<-extent(as.numeric(regiones[5:8])); region_2=crop(results$SST, region_2)
    
    # cree un data frame de las regiones predictoras omitiendo las celdas sin información
    zonas<-as.data.frame(rbind(na.omit(region_1[]), na.omit(region_2[])))
    # Corra el AFM, tenga encuenta que el argumento type="s" ("s" variables are scaled to unit variance)
    # En esta linea se tiene que delimitar claramente los grupos
    res <- MFA(t(zonas), group=c(dim(na.omit(region_1[]))[1],dim(na.omit(region_2[]))[1]),type=rep("s",2), name.group=c("zona1", "zona2"))
     
    
  } else if(num_zone==3){## 3 zonas
    
    region_1<-extent(as.numeric(regiones[1:4])); region_1=crop(results$SST, region_1)
    region_2<-extent(as.numeric(regiones[5:8])); region_2=crop(results$SST, region_2)
    region_3<-extent(as.numeric(regiones[9:12])); region_3=crop(results$SST, region_3)
    
    zonas<-as.data.frame(rbind(na.omit(region_1[]), na.omit(region_2[]), na.omit(region_3[])))
    res <- MFA(t(zonas), group=c(dim(na.omit(region_1[]))[1],dim(na.omit(region_2[]))[1], dim(na.omit(region_3[]))[1]),type=rep("s",3), name.group=c("zona1", "zona2",  "zona3"))
    
    
  } else  if(num_zone==4){ ## 4 zonas
    region_1<-extent(as.numeric(regiones[1:4])); region_1=crop(results$SST, region_1)
    region_2<-extent(as.numeric(regiones[5:8])); region_2=crop(results$SST, region_2)
    region_3<-extent(as.numeric(regiones[9:12])); region_3=crop(results$SST, region_3)
    region_4<-extent(as.numeric(regiones[13:16])); region_4=crop(results$SST, region_4)
    
    zonas<-as.data.frame(rbind(na.omit(region_1[]), na.omit(region_2[]), na.omit(region_3[]), na.omit(region_4[])))
    res <- MFA(t(zonas), group=c(dim(na.omit(region_1[]))[1],dim(na.omit(region_2[]))[1], dim(na.omit(region_3[]))[1], dim(na.omit(region_4[]))[1]),type=rep("s",4), name.group=c("zona1", "zona2",  "zona3",  "zona4"))

    
  } 
  
  
  setwd(paste("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/results_graphs/", dep, "/",answer, "/", trim, "/", sep=""))
  
  
  #Guarde las guarde el archivo de contribuciones 
  write.csv(res$group$contrib, file=paste("contr",answer, trim,lead_num ,".csv", sep="_"))
  #Guarde el archivo con lso valores propios
  write.csv(res$eig, file=paste("eigGroup",answer, trim,lead_num ,".csv", sep="_"))
  # Imprima el resumen del AFM
  
  
  ###### Guardado automatico de la grafica de valores propios
  png(file =paste("barEig",answer, trim, lead_num ,".png", sep="_"), bg = "transparent", width = 500, height = 300)
  barplot(res$eig[,1],main=paste("Eigenvalues",dep,trim,"-",lead),names.arg=1:nrow(res$eig))
  dev.off()
  
  
  # correlación entre las componentes y el indicador
  corr<-cbind.data.frame(cor1=cor(results$ind, res$global.pca$ind$coord[,1]),cor2=cor(results$ind, res$global.pca$ind$coord[,2]), cor3=cor(results$ind, res$global.pca$ind$coord[,3]))
  
  # Realice un gráfico entre la primera componente y el indicador de la estación y agregue la correlación
  datos=as.data.frame(cbind(res$global.pca$ind$coord[,1], results$ind))
  gp = ggplot(datos, aes(datos[,1], datos[,2])) +  geom_point(shape=19, size=3,colour="#CC0000")
  gp = gp +  geom_smooth(method=lm, colour="#000099") + labs(x="Primera Componente AFM",y="Precipitación (mm)") 
  gp = gp + geom_text(data = data.frame(), aes(max(datos[,1])-0.5, max(datos[,2])+20, label = paste("r = ", round(cor(datos[,1], datos[,2]),3), sep="")))
  gp = gp + theme_bw()
  # Realice un gráfico entre la segunda componente y el indicador de la estación y agregue la correlación
  datos2=as.data.frame(cbind(res$global.pca$ind$coord[,2], results$ind))
  p2 = ggplot(datos2, aes(datos2[,1], datos2[,2])) +  geom_point(shape=19, size=3,colour="#CC0000")
  p2 = p2 +  geom_smooth(method=lm, colour="#000099") + labs(x="Segunda Componente AFM",y="Precipitación (mm)") 
  p2 = p2 + geom_text(data = data.frame(), aes(max(datos2[,1])-0.5, max(datos2[,2])+20, label = paste("r = ", round(cor(datos2[,1], datos2[,2]),3), sep="")))
  p2 = p2 + theme_bw()
  
  # Guarde las dos imagenes automaticamente
  tiff(paste(ruta,"/results_graphs/", dep, "/", answer,"/", trim,"/disp","_",answer, "_",trim,"_",lead_num,".tif",sep=""), height=400,width=550,res=100,
       compression="lzw") 
  grid.arrange(gp,p2,ncol=2)
  dev.off()
  # Guarde las componentes en un archivo .csv
  write.csv(res$global.pca$ind$coord, file=paste("componentes",answer, a, lead_num ,".csv", sep="_"))
  # Guarde lo que vaya a entregar en una lista que entrega, la correlaciónm, la contribución y las componentes
  entrega<-list(corr=corr, contrib=res$group$contrib, res_global.pca=res$global.pca$ind$coord[,1:3])
  return(entrega)}
#rownames(tabla)<-paste(tabla[,"Station"],tabla[,1], sep="_")







## Lea la tabla con las coordenadas de las regiones
tabla=read.table("clipboard",header = T)


# Esta información se usa como referencia, son los sitios 
# de interes en el estudio
estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima","Turipana")
sitios<-c("Lorica","Yopal","Ibagué","LaUnion","Espinal","Cereté")
cbind(estaciones, sitios)



dep="casanare"

## lead
if(a==12){
  lead=c("DEF_Nov","DEF_Aug", "DEF_Jun")
}else  if(a==3){ 
  lead=c("MAM_Feb","MAM_Nov", "MAM_Sep")}else  if(a==6){
    lead=c("JJA_May","JJA_Feb","JJA_Dec")
  }else  if(a==9){
    lead=c("SON_Aug", "SON_May", "SON_Mar")
  }

answer= "AptoYopal"
lead_num=c(0,3,5)
a<-c(12,3,6,9)
num_zone<-tabla[,"num_zone"]

for(j in 1:4){
  if(a[j]==12){
    lead=c("DEF_Nov","DEF_Aug", "DEF_Jun")
  }else if(a[j]==3){ 
    lead=c("MAM_Feb","MAM_Nov", "MAM_Sep")}else if(a[j]==6){
      lead=c("JJA_May","JJA_Feb","JJA_Dec")
    }else if(a[j]==9){
      lead=c("SON_Aug", "SON_May", "SON_Mar")}
  for(i in 1:3){
    results<-correlation(dep, lead[i], a[j],answer)
    MFA<-MFA_C(dep, a[j], lead[i], lead_num[i], num_zone[j], results, answer)
  }
} 




setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/")
getwd()











## Esta función es similar a la anterior, con la diferencia que no guarda auntomaticamente
## los archivos
MFA_P<-function(dep, a, lead, num_zone, results, answer){
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  ## nombre de la fila que se va a estudiar (departamento - trimestre donde se encuentra las estaciones)
  fila=paste(answer,trim,sep="_")
  regiones<-tabla[fila,names(tabla)[5:20]] # elimine las celdas que no corresponden a coordenadas
  regiones<-regiones[1:(4*num_zone)] #determine cuantas regiones se utilizaran
  
  ### AFM condicionado por el número de regiones
  if(num_zone==2){ # 2 zonas 
    # Cree la región predictora y corte la SST de acuerdo a la región
    region_1<-extent(as.numeric(regiones[1:4])); region_1=crop(results$SST, region_1)
    region_2<-extent(as.numeric(regiones[5:8])); region_2=crop(results$SST, region_2)
    
    # cree un data frame de las regiones predictoras omitiendo las celdas sin información
    zonas<-as.data.frame(rbind(na.omit(region_1[]), na.omit(region_2[])))
    # Corra el AFM, tenga encuenta que el argumento type="s" ("s" variables are scaled to unit variance)
    # En esta linea se tiene que delimitar claramente los grupos
    res <- MFA(t(zonas), group=c(dim(na.omit(region_1[]))[1],dim(na.omit(region_2[]))[1]),type=rep("s",2), name.group=c("zona1", "zona2"))
    
    
  } else if(num_zone==3){## 3 zonas
    
    region_1<-extent(as.numeric(regiones[1:4])); region_1=crop(results$SST, region_1)
    region_2<-extent(as.numeric(regiones[5:8])); region_2=crop(results$SST, region_2)
    region_3<-extent(as.numeric(regiones[9:12])); region_3=crop(results$SST, region_3)
    
    zonas<-as.data.frame(rbind(na.omit(region_1[]), na.omit(region_2[]), na.omit(region_3[])))
    res <- MFA(t(zonas), group=c(dim(na.omit(region_1[]))[1],dim(na.omit(region_2[]))[1], dim(na.omit(region_3[]))[1]),type=rep("s",3), name.group=c("zona1", "zona2",  "zona3"))
    
    
  } else  if(num_zone==4){ ## 4 zonas
    region_1<-extent(as.numeric(regiones[1:4])); region_1=crop(results$SST, region_1)
    region_2<-extent(as.numeric(regiones[5:8])); region_2=crop(results$SST, region_2)
    region_3<-extent(as.numeric(regiones[9:12])); region_3=crop(results$SST, region_3)
    region_4<-extent(as.numeric(regiones[13:16])); region_4=crop(results$SST, region_4)
    
    zonas<-as.data.frame(rbind(na.omit(region_1[]), na.omit(region_2[]), na.omit(region_3[]), na.omit(region_4[])))
    res <- MFA(t(zonas), group=c(dim(na.omit(region_1[]))[1],dim(na.omit(region_2[]))[1], dim(na.omit(region_3[]))[1], dim(na.omit(region_4[]))[1]),type=rep("s",4), name.group=c("zona1", "zona2",  "zona3",  "zona4"))
    
    
  } 
  
  #setwd(paste("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/results_graphs/", dep, "/",answer, "/", trim, "/", sep=""))
  
  
  corr<-cbind.data.frame(cor1=cor(results$ind, res$global.pca$ind$coord[,1]),cor2=cor(results$ind, res$global.pca$ind$coord[,2]), cor3=cor(results$ind, res$global.pca$ind$coord[,3]))
  corr_p<-cbind.data.frame(cor1t=cor.test(results$ind, res$global.pca$ind$coord[,1])$p.value,cor2t=cor.test(results$ind, res$global.pca$ind$coord[,2])$p.value,cor3t=cor.test(results$ind, res$global.pca$ind$coord[,3])$p.value)
  
  entrega<-list(corr=corr, corr_test=corr_p, contrib=res$group$contrib, res_global.pca=res$global.pca$ind$coord[,1:3])
  return(entrega)}






tabla=read.table("clipboard",header = T)
rownames(tabla)<-paste(tabla[,"Station"],tabla[,1], sep="_")

answer<- "CentAdmoLaUnion"
dep<-"valle"
a<-tabla[,"a"]
num_zone<-tabla[,"num_zone"]

datos<-0
for(j in 1:4){
  
  if(a[j]==12){
    lead=c("DEF_Nov","DEF_Aug", "DEF_Jun")
  }else if(a[j]==3){ 
    lead=c("MAM_Feb","MAM_Nov", "MAM_Sep")}else if(a[j]==6){
      lead=c("JJA_May","JJA_Feb","JJA_Dec")
    }else if(a[j]==9){
      lead=c("SON_Aug", "SON_May", "SON_Mar")}

  for(i in 1:3){
    results<-correlation(dep, lead[i], a[j],answer)
    MFA<-MFA_P(dep, a[j], lead[i], num_zone[j], results, answer)
    datos_p=data.frame(a[j],answer,lead[i], MFA$corr, MFA$corr_test)
    datos=rbind(datos, datos_p)
  }
} # Aqui se almacena el coeficiente de correlación de las 3 primeras componentes
# Cree una tabla global para el coeficiente de correlación 
datosp<-rbind(datosp,datos[-1,])



##Organice la información global 
#datosp<-datosp[-1,]
names(datosp)<-c("a","Station","lead","cor1", "cor2", "cor3", "test1", "test2","test3")
row.names(datosp)<-paste(substring(datosp[,2],1,5),datosp[,1], datosp[, "lead"], sep="_")
## Guarde en un archivo la tabla
write.csv(datosp, file = "correlaciones.csv")





















#### Gráfico de Correlaciones
correlaciones <- read.csv("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/correlaciones.csv", row.names=1)
correlaciones[,"a"]<-as.factor(correlaciones[,"a"])

# Este es un gráfico de barras de las correlaciones de cada componente 
# con cada estación agrupadas por trimestre y lead time 
c <- ggplot(correlaciones, aes(a,cor1, fill=as.factor(lead_num)) )
c <- c + geom_bar(stat = "identity", position="dodge")
c <- c + facet_grid(. ~ Station) + theme_bw() 
c <- c + geom_hline(yintercept = c(0.25,-0.25)) + labs( x="", y="Cor comp 1") + ylim(c(-0.6,0.8)) + guides(fill=guide_legend(title="Lead Time"))


d <- ggplot(correlaciones, aes(a,cor2, fill=as.factor(lead_num)) )
d <-d + geom_bar(stat = "identity", position="dodge")
d <- d + facet_grid(. ~ Station) + theme_bw() 
d <- d + geom_hline(yintercept = c(0.25,-0.25)) + labs( x="", y="Cor comp 2") + ylim(c(-0.6,0.8)) + guides(fill=guide_legend(title="Lead Time"))

f <- ggplot(correlaciones, aes(a,cor3, fill=as.factor(lead_num)) )
f <-f + geom_bar(stat = "identity", position="dodge")
f <- f + facet_grid(. ~ Station) + theme_bw() 
f <-f + geom_hline(yintercept = c(0.25,-0.25)) + labs( x="", y="Cor comp 3") + ylim(c(-0.6,0.8)) + guides(fill=guide_legend(title="Lead Time"))

x11()
grid.arrange(c,d,f, ncol=1)

# Posiciones en las que se cumple que el valor absoluto de la correlación
# sea mayor a 0.25
cond<-ifelse(abs(correlaciones[,5:7])>0.3,yes = 1,no = 0)

suma<-apply(cond, 1, sum) # Diga por filas cuantas componentes cumplen la condición 


cbind(cond,suma)


rownames(correlaciones)[which(suma<1)] # Muestre cuales modelos no pueden ajustarse
# es decir no  cumplen con la condición 
correlaciones[which(suma<1),] # Muestre las correlaciones de aquellos modelos que no cumplen

















# Lea la tabla donde se tienen las coordenadas de las regiones predictoras
tabla=read.table("clipboard",header = T)
rownames(tabla)<-paste(tabla[,"Station"],tabla[,"Trimestre" ], sep="_")

# Lea la tabla en la cual tiene almacenados los modelos que se correran 
# En esta tabla se tiene la información de cuales componentes ingresaran al modelo
tab_comp<-read.table("clipboard",header = T)
rownames(tab_comp)<-paste(substring(tab_comp[,2],1,5),tab_comp[,1], tab_comp[,3], sep="_")


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
modelo<-function(dep,lead,lead_num, a, answer, num_zone, comp){
  
  
  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  
  # Con esta función se consiguen los valores de la SST y de la estación (answer) deseada 
  # (además de mapas de correlaciones e información departamental)
  results<-correlation(dep, lead, a,answer)
  # Esta función corre el AFM y entrega las componentes para ingresar al modelo
  MFA<-MFA_P(dep, a, lead, num_zone, results, answer)
  
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
  setwd(paste(ruta, "/results_graphs/", dep, "/",answer,"/", trim ,"/modelo/", sep=""))
  
  
  adj_r_squared=summary(modelo)$adj.r.squared # Estrae el adj_r_squared del modelo
  # A  partir del estadístico F se calcula el valor_p del modelo
  p_value<-1-pf(summary(modelo)$fstatistic[1], summary(modelo)$fstatistic[2], summary(modelo)$fstatistic[3])
  summary(modelo) # imprima el resumen del modelo
  
  # Guarde el resumen gráfico de los supuestos
  tiff(paste(ruta,"/results_graphs/",dep,"/", answer,"/", trim ,"/modelo/Csup_", answer,"_",a,"_",lead_num,".tif",sep=""), height=450,width=700,res=100,
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
  
  tiff(paste(ruta,"/results_graphs/",dep,"/", answer,"/", trim ,"/modelo/Cajuste_", answer,"_",a,"_",lead_num,".tif",sep=""), height=450,width=700,res=100,
       compression="lzw") 
  print(ajuste)
  dev.off()
  
  # Cree una lista donde se guarde los indicadores del modelo, lo observado, lo pronosticado y los residuales del modelo, tambien se almacenan las coefficientes
  entrega<-list(summary_m=summary_m, obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals,  coeff=modelo$coefficients)
  
  # Devuelva la lista 
  return(entrega)}




# Corra todos los modelos
dep<-"valle"
answer<-"CentAdmoLaUnion"
lead<-tab_comp[,"lead"]
lead_num<-tab_comp[,"lead_num"]
a<-tab_comp[,"a"]
num_zone<-tab_comp[,"num_zone"]
comp<-tab_comp[,c("cor1","cor2", "cor3")]

mo_p<-0

# Guarde para todos los modelos los indicadores calculados en un data frame
for(i in 1:dim(tab_comp)[1]){
  model<-modelo(dep,lead[i],lead_num[i],a[i], answer, num_zone[i], comp[i,])$summary_m
  row.names(model)=paste(substring(answer,1,5),a[i], lead_num[i], sep="_")
  mo_p=rbind(mo_p, model)  
}

# Guarde automaticamente una tabla con todos los indicadores de los modelos corridos
mop<-rbind(mop,mo_p[-1, ])
mop

setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM/")
getwd()
write.csv(mop, file = "summary_models.csv", row.names = TRUE)









### Modelo parcial 

## Esta función es similar a la anterior, con la diferencia que no guarda auntomaticamente
## los archivos
modelop<-function(dep,lead,lead_num, a, answer, num_zone, comp){

  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
  results<-correlation(dep, lead, a,answer)
  MFA<-MFA_P(dep, a, lead, num_zone, results, answer)
  
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
  
  
  adj_r_squared=summary(modelo)$adj.r.squared
  p_value<-1-pf(summary(modelo)$fstatistic[1], summary(modelo)$fstatistic[2], summary(modelo)$fstatistic[3])
  summary(modelo)
  
  
  
  
  #tiff(paste(ruta,"/results_graphs/",dep,"/", answer,"/", trim ,"/modelo/psup_", answer,"_",a,"_",lead_num,".tif",sep=""), height=450,width=700,res=100,
  #     compression="lzw") 
  #layout(matrix(1:6,2))
  #plot(modelo)
  #acf(modelo$residuals)
  #pacf(modelo$residuals) 
  #dev.off()
  
  
  media<-mean(results$ind)
  Intercept<-modelo$coefficients[1]
  var<-bptest(modelo)$p.value # igualdad de varianza
  nor<-shapiro.test(modelo$residuals)$p.value # normalidad
  box<-Box.test (modelo$residuals, lag = 5)$p.value # independencia
  cor_b<-round(cor(results$ind, modelo$fitted.values),3)
  RMSE_b<-round(sqrt(sum((modelo$fitted.values-results$ind)^2)/length(results$ind)),3)
  
  summary_m<-data.frame(media,Intercept,adj_r_squared,p_value, var, nor, box, RMSE_b, num_coef=length(modelo$coefficients), row.names = NULL)
  
  
  entrega<-list(summary_m=summary_m, obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals,  coeff=modelo$coefficients)
  results_model<-cbind.data.frame(obs=results$ind, pron=modelo$fitted.values, residuales=modelo$residuals)
  
  
  #setwd(paste(ruta, "/results_graphs/", dep, "/",answer,"/", trim ,"/modelo/", sep=""))
  #write.csv(results_model, paste("presultsModel_", answer,"_",a,"_",lead_num,".csv",sep=""))
  
  #ajuste=ggplot(data.frame(), aes(results$ind, modelo$fitted.values)) +  geom_point(shape=19, size=3,colour="#CC0000")
  #ajuste=  ajuste +  geom_smooth(method=lm, colour="#000099") + labs(x="Precipitación obs (mm)",y="Precipitación pron (mm)") + theme_bw()
  #ajuste=  ajuste +  geom_text(data = data.frame(), aes(max(results$ind)-70, min(modelo$fitted.values)+60, label = paste("r = ", round(cor(results$ind, modelo$fitted.values),3), sep="")))
  #ajuste=  ajuste +  geom_text(data = data.frame(), aes(max(results$ind)-70, min(modelo$fitted.values)+10, label = paste("RMSE = ", round(sqrt(sum((modelo$fitted.values-results$ind)^2)/length(results$ind)),3), sep="")))

  
  #tiff(paste(ruta,"/results_graphs/", dep, "/",answer,"/", trim ,"/modelo/pajuste_", answer,"_",a,"_",lead_num,".tif",sep=""), height=450,width=700,res=100,
  #     compression="lzw") 
  #print(ajuste)
  #dev.off()
  
  return(entrega)}

tab_comp<-read.table("clipboard",header = T)
rownames(tab_comp)<-paste(substring(tab_comp[,2],1,5),tab_comp[,1], tab_comp[,3], sep="_")

dep<-"cordoba"
answer<-"Turipana"
lead<-tab_comp[,"lead"]
lead_num<-tab_comp[,"lead_num"]
a<-tab_comp[,"a"]
num_zone<-tab_comp[,"num_reg"]
comp<-tab_comp[,c("cor1","cor2", "cor3")]

mo_p<-0
for(i in 1:dim(tab_comp)[1]){
  model<-modelop(dep,lead[i],lead_num[i],a[i], answer, num_zone[i], comp[i,])$summary_m
  row.names(model)=paste(substring(answer,1,5),a[i], lead_num[i], sep="_")
  mo_p=rbind(mo_p, model)  
}
mop<-rbind(mop,mo_p[-1, ])

### Guarde automaticamente el archivo con el resumen de los modelos. 
#setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()

#write.csv(mop, file = "summary_models_parcial.csv", row.names = TRUE)














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





cross_validation <-function(dep,lead,lead_num, a, answer, num_zone, comp){

  # determine el trimestre que se va a pronosticar en base al mes de incio del periodo 
  if(a==3){trim<-"MAM"}else if(a==6){ trim<-"JJA"}else if(a==9){trim<-"SON"}else if(a==12){trim<-"DEF"}else print("ERRORR !!!!")
  
  
    
  results<-correlation(dep, lead, a,answer) # corre lo mapas de correlaciones y devuelve
  # la información de la estación y la sst (trimestral)
  MFA<-MFA_P(dep, a, lead, num_zone, results, answer) ## Corre la información del AFM
  
  # Crea una matriz con las posiciones que se deben eliminar en cada validación. 
  j=matrix(c(1:30),5,6, byrow = F) ## Matriz de posiciones
  forecasts=matrix(c(1:30),5,6, byrow = F) ## Matriz en blanco de pronostico
  
  
  ## Crea una matriz para almacenar los coeficientes de regresión
  coef<-matrix(c(1:(sum(comp)+1)),6,(sum(comp)+1), byrow = T)
  z<-cbind.data.frame(y=results$ind, x1=MFA$res_global.pca[,1], x2=MFA$res_global.pca[,2], x3=MFA$res_global.pca[,3])
  
  
  # Realiza la validación cruzada 
  for(i in 1:6){
    
    results_i<-list(ind=results$ind[-j[,i]], SST=results$SST[[-j[,i]]])
    
    MFA1<-MFA_P(dep, a, lead, num_zone, results_i, answer) ## Corre la información del AFM
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
  setwd(paste(ruta,"/results_graphs/", dep,"/", answer, "/", trim, "/modelo/",sep=""))
  
  
  
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









# Lea la tabla donde se tienen las coordenadas de las regiones predictoras
tabla=read.table("clipboard",header = T)
rownames(tabla)<-paste(tabla[,"Station"],tabla[,"Trimestre" ], sep="_")

# Lea la tabla en la cual tiene almacenados los modelos que se correran 
# En esta tabla se tiene la información de cuales componentes ingresaran al modelo
tab_comp<-read.table("clipboard",header = T)
rownames(tab_comp)<-paste(substring(tab_comp[,2],1,5),tab_comp[,1], tab_comp[,3], sep="_")

# Declare las variables de la función 
dep<-"valle"
answer<-"CentAdmoLaUnion"
lead<-tab_comp[,"lead"]
lead_num<-tab_comp[,"lead_num"]
a<-tab_comp[,"a"]
num_zone<-tab_comp[,"num_zone"]
comp<-tab_comp[,c("cor1","cor2", "cor3")]


# Vuelva al espacio de trabajo original 
setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()

# Corra la función y almacene el goodness index
GI<-0
mo_p<-0
for(i in 1:dim(tab_comp)[1]){
  GI[i]<-cross_validation(dep,lead[i],lead_num[i], a[i], answer, num_zone[i], comp[i,])$kendall
  data<-cbind.data.frame(dep, answer,a[i], lead_num[i], num_zone[i],GI[i])
  mo_p=rbind(mo_p, data)  
}


mop<-rbind(mop,mo_p[-1, ])
mop




setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()


# Almacene el archivo
write.csv(mop, file = "summary_models_cv.csv", row.names = TRUE)

























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


restrospective_validation<-function(dep,lead,lead_num, a, answer, num_zone, comp){
  
  
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
  
  
  
  # Realiza la validación retrospectiva
  for(i in 1:length_r){ # repita esto, de acuerdo a la cantidad de prontosicos
    
    j=matrix(which(year>pron_year[i]),length(which(year>pron_year[i])),1,
             byrow = F) ## Matriz de posiciones
    results_i<-list(ind=results$ind[-j], SST=results$SST[[-j]])
    MFA1<-MFA_P(dep, a, lead, num_zone, results_i, answer) ## Corre la información del AFM
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
    
    
    
    MFA2<-MFA_P(dep, a, lead, num_zone, results_q, answer) ## Corre la información del AFM
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
  setwd(paste(ruta,"/results_graphs/", dep, "/", answer, "/", trim,"/modelo/",sep=""))
  
  write.csv( retro, file = paste(" retro_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  write.csv(coef, file = paste("coef_" , answer,"_",a,"_",lead_num,".csv", sep=""))
  
  # Retorne la lista de resultados.
  return(resultados)}



# Lea la tabla donde se tienen las coordenadas de las regiones predictoras
tabla=read.table("clipboard",header = T)
rownames(tabla)<-paste(tabla[,"Station"],tabla[,"Trimestre" ], sep="_")

# Lea la tabla en la cual tiene almacenados los modelos que se correran 
# En esta tabla se tiene la información de cuales componentes ingresaran al modelo
tab_comp<-read.table("clipboard",header = T)
rownames(tab_comp)<-paste(substring(tab_comp[,2],1,5),tab_comp[,1], tab_comp[,3], sep="_")





# Declare las variables de la función 
dep<-"valle"
answer<-"CentAdmoLaUnion"
lead<-tab_comp[,"lead"]
lead_num<-tab_comp[,"lead_num"]
a<-tab_comp[,"a"]
num_zone<-tab_comp[,"num_zone"]
comp<-tab_comp[,c("cor1","cor2", "cor3")]



# Vuelva al espacio de trabajo original 
setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()






# Corra la función y almacene el goodness index
GI<-0
mo_p<-0
for(i in 1:dim(tab_comp)[1]){
  GI[i]<-restrospective_validation(dep,lead[i],lead_num[i], a[i], answer, num_zone[i], comp[i,])$kendall
  data<-cbind.data.frame(dep, answer,a[i], lead_num[i], num_zone[i],GI[i])
  mo_p=rbind(mo_p, data)  
}


mop<-rbind(mop,mo_p[-1, ])
mop




setwd("C:/Users/AESQUIVEL/Google Drive/Exp_2_AFM")
getwd()


# Almacene el archivo
write.csv(mop, file = "summary_models_retro.csv", row.names = TRUE)

























###############################################################
###############################################################
###############################################################
###############################################################

###   Gráfico


tab<-read.table("clipboard",header = T) # Lea la tabla que tiene almacenada las validaciones
tab[which(tab[,"a"]==12),"a"]=0 # al trimestre 12 (DEF) asignele el valor de 0 para que aparezca 
# primero en los gráficos. 


# declare las etiquetas para el gráfico, labels es para los triemstres
labels<-as_labeller(c("0"="DEF","3"="MAM","6"="JJA", "9"="SON"))
# labels_e es para cambiar el nombre las estaciones a los sitios de interes. 
labels_e<-as_labeller(c("AptoYopal"="Yopal","DoctrinaLa"="Lorica","Turipana" ="Cereté", "AptoPerales"="Ibagué", "Nataima" = "Espinal", "CentAdmoLaUnion"="La Unión"))

# Gráfique los GI condicionando por sitio de estudio y trimestre
ggplot(tab,aes(x=good,y=retro,shape=as.factor(lead_num),color=as.factor(lead_num), size=0.2))+
  geom_point()+ facet_grid(a~answer,  labeller = labeller(a = labels, answer=labels_e))+
  theme_bw()+ scale_size(guide=F)+scale_color_discrete(name="Lead Time")+
  scale_shape(name="Lead Time")+ylab("Goodness Index - Retroactive")+
  xlab("Goodness Index - Cross Validated")+
  theme(strip.text.x = element_text(size = 11)) + 
  geom_hline(yintercept = 0.3, colour = "black", linetype = "dotted") + 
  geom_vline(xintercept = 0.3, colour = "black", linetype = "dotted") 

# guarde el gráfico. 
getwd()
ggsave("best_model.png",width =12 ,height =6,dpi=200 )


