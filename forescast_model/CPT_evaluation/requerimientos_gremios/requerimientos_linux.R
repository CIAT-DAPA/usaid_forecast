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

# Predictor del cfsv2 que se desea analizar 
prec <- "SST"

# Estas son las variables para las que se puede utilizar, no olvidar 
# utilizar los mismos nombres que estan aquí para que funcionen bien los codigos
# prec= c("SST" ,"U_wind_250","U_wind_850", "rhum_700", "vertical_vel_250")



### Directorio de trabajo
setwd("C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/")
getwd()

final_year<-2013






######################################
######################################
######################################
######################################


## Ruta principal donde se encuentran las carpetas con los archivos  
ruta <- "C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/"



ruta_l<-"C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/"
### Lectura del shp
colombia<-shapefile(paste(ruta_l,"/colombia/colombia_depts.shp",sep=""))
shp<-shapefile(paste(ruta_l,"/shp/mapa_mundi.shp",sep=""))





####### Organizar el directorio correctamente
dep_f<-c("casanare",    "cordoba",    "tolima",    "valle", "santander")

if(dir.exists("results")==FALSE){dir.create("results")}
setwd(paste( getwd(),"/results/" ,sep=""))
print(getwd())

if(list.dirs()=="."){lapply(dep_f, dir.create)}

#setwd(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/results_graphs_C/", prec, "/", sep=""))
getwd()








################################################################
################################################################

################         Funciones         #####################

################################################################
################################################################


#### Crea un raster a partir de la tabla .tsv, pero esta funcion es un apoyo de rasterize
##
if(prec=="SST"){
  #####################################################
  #### Para la TSM 
  #### Crea un raster a partir de la tabla .tsv, pero esta funcion es un apoyo de rasterize
  transform_raster=function(x){
    mapa_base=raster()
    val=c(as.matrix(t(x),ncol=1,byrow = T))
    val=as.numeric(val)
    val[val==-999.000]=NA
    values(mapa_base)=val
    return(mapa_base)
  }
  # Con esta función se depura y rasteriza la tabla .tsv
  rasterize=function(dates, prec) { 
    
    if(require(raster)==FALSE){install.packages("raster")}
    library("raster")
    pos_years=!is.na(dates[1,])
    year_month=dates[1,][pos_years]
    if(substr(year_month[2],6,7)=="12"){year=as.numeric(substr(year_month[-1],1,4))+1
    }else{year=as.numeric(substr(year_month[-1],1,4))}
    total_row_delete=c(-1,-3,-(which(dates[,1]=="90.0")-2),-which(dates[,1]=="-90.0"),-which(dates[,1]==""))
    dates=dates[total_row_delete,-1]
    list_dates=split(dates,sort(rep(year,180)))
    all_raster=lapply(list_dates,transform_raster)
    layers=stack(all_raster)
    layers_crop=crop(layers,extent(-180, 180, -30, 30))
    
    return(layers_crop)
  }
  
} else if(prec!="SST"){
  ####################################################
  #### Para los nuevos predictores 
  #### Crea un raster a partir de la tabla .tsv, pero esta funcion es un apoyo de rasterize
  transform_raster=function(x){ 
    # Primero se crea un raster teniendo encuenta la resolución espacial de la tabla .tsv  
    mapa_base=raster(nrows=180, ncols=360,xmn=0,xmx=359, ymn=-90,ymx=90) # Dimensiones del raster
    val=c(as.matrix(t(x),ncol=1,byrow = T)) 
    val=as.numeric(val)
    val[val==-999.000]=NA
    values(mapa_base)=val
    return(mapa_base)
  }
  # Con esta función se depura y rasteriza la tabla .tsv
  # El Argumento prec permite tener el cuenta el tipo de predictor
  # cuando se rasteriza el .tsv
  rasterize=function(dates, prec) { 
    
    if(require(raster)==FALSE){install.packages("raster")}
    library("raster")
    pos_years=!is.na(dates[1,]) # Muestra en que lugares de la fila 1 hay información
    year_month=dates[1,][pos_years] # Muestra la información d ela fila
    
    
    if(substr(year_month[2],6,7)=="12"){year=as.numeric(substr(year_month[-1],1,4))+1
    }else{year=as.numeric(substr(year_month[-1],1,4))}
    
    
    if(prec=="U_wind_250" | prec=="U_wind_850"){
      
      total_row_delete=c(-1,-3,-(which(dates[,1]=="90.0")-2),-which(dates[,1]=="-90.0"),-which(dates[,1]==""))
      dates_1=dates[total_row_delete,-1] # Elimina la información no relevante
      
    } else if(prec== "rhum_700" | prec=="vertical_vel_250"){
      
      total_row_delete=c(-1,-3,-(which(dates[,1]=="90")-2),-which(dates[,1]=="-90"),-which(dates[,1]==""))
      dates_1=dates[total_row_delete,-1] # Elimina la información no relevante
      
    }
    
    #
    list_dates=split(dates_1,sort(rep(year,180))) # Se divide la tabla de datos por año
    all_raster=lapply(list_dates,transform_raster) ## Transforma las tablas de datos en rasters
    layers=stack(all_raster) # Crea un stack
    
    
    if(prec=="U_wind_250"){
      r1 <- crop(layers,extent(0, 50, -25, 25))
      r2 <- crop(layers,extent(220, 357.5, -25, 25))
      res(r2) <- c(xres(r1), yres(r1))
      
      x <- list(r1, r2)
      x$overwrite <- TRUE
      layers_crop <- do.call(merge, x)
      names(layers_crop)<-  names(r1)
      #plot(layers_crop)  
    }else if(prec=="U_wind_850"){
      layers_crop <- crop(layers,extent(0, 357.5, -45, 45))
      #plot(layers_crop)  
    }else if(prec=="rhum_700"){
      layers_crop <- crop(layers,extent(0, 357.5, -30, 30))
      #plot(layers_crop)  
    }else if(prec=="vertical_vel_250"){
      r1 <- crop(layers,extent(0, 45, -20, 20))
      r2 <- crop(layers,extent(135, 357.5, -20, 20))
      res(r2) <- c(xres(r1), yres(r1))
      
      x <- list(r1, r2)
      x$overwrite <- TRUE
      layers_crop <- do.call(merge, x)
      names(layers_crop)<-  names(r1)
      #plot(layers_crop)  
    }
    
    return(layers_crop)
  }
}



### Esta función convierte los datos de las estacines en datos trimestrales
data_trim=function(Estaciones_C, a){ #Los argumentos son el conjunto de las estaciones 
  
  ## y el mes de incio del periodo (a)
  stations=Estaciones_C 
  stations=stations[-1:-2,] # Quite las dos primeras filas (coordenadas)
  year=sort(rep(1981:final_year,12)) # cree un vector para los años
  month=rep(1:12,length(1981:final_year)) # cree el vector de meses
  data_station=cbind.data.frame(year,month,stations, row.names=NULL) # cree un data frame 
  pos=seq(a,dim(data_station)[1],12) #  posiciones ne las que se encuentra el mes a
  pos_select=sort(c(pos,pos+1,pos+2)) # muestre las posiciones del trimestre
  # Agregue los datos del trimestre y luego sumelos. 
  data_out=aggregate(data_station[pos_select,-1:-2],by=list(sort(rep(1:(length(pos)),3))),sum)
  data_out_final=na.omit(data_out[,-1]) # Elimine los NA
  years_y=na.omit(year[pos+1]) # Elimine los NA
  data_out_final=data.frame(years_y,data_out_final) # Cree un data frame con los datos finales
  return(data_out_final)} # devuelva los datos finales





#### Gráfico 1 - Correlación entre las componentes y la SST

# var_oceanoAt= variable oceano atmosferica
# y serie = el modo en y
# xserie = el modo en x
# Estaciones_C= archivo de estaciones en el cual se realizo CPT
# Colombia= shp del país
# names_file = nombre del trimestre pronosticado y su lag (DEF_DEF ó DEF_0)
# a = mes de inicio del trimestre, b = segundo mes del trimestre, c = tercer mes del trimestre
# length_periodo = Longitud del periodo de entrenamiento
## cca_Maps función que realiza el gráfico cca_map de CPT, las imagenes se almacenan en la ruta
cca_maps<-function(var_ocanoAt, names_file, yserie, Estaciones_C, xserie, lead, ruta,  a, xmin, xmax, ymin, ymax, estaciones_in, prec){
  
  ocean=which(!is.na(var_ocanoAt[[1]][])) # tome las posiciones en las que la variable sea diferente de NA
  correl=array(NA,length(ocean)) # relice un arreglo del tamaño de oceano 
  var_table=var_ocanoAt[] # Realice una tabla de la variable
  
  for(i in 1:length(ocean)){ # En todos los pixeles diferentes de NA
    var_pixel=var_table[ocean[i],] # Extraiga el pixel i 
    correl[i]=cor(xserie$X1,var_pixel) # realice la correlación entre el pixel i el modo 1 de x
  } # 
  
  correl_map=var_ocanoAt[[1]] # Cree un raster vacio 
  correl_map[]=NA 
  correl_map[ocean]=correl # Almacene en el raster los NA 
  
  
  # Realice el mapa de Correlaciones entre la variable y el modo 1 de x
  if(prec=="SST"){

    Map_x<- gplot(correl_map) + geom_tile(aes(fill = value)) + coord_equal() + 
      scale_fill_gradient2(low="#2166AC",mid = "white", high="#B2182B",name = " ",  limits=c(-1,1)) + 
      labs(title=" ",x="Long",y="Lat")  + theme(legend.key.height=unit(0.5,"cm"),legend.key.width=unit(2,"cm"),
                                                legend.text=element_text(size=10),
                                                panel.background=element_rect(fill="white",colour="black"),
                                                axis.text=element_text(colour="black",size=10),
                                                axis.title=element_text(colour="black",size=10,face="bold"),
                                                legend.position = "bottom", 
                                                legend.title = element_text(size = 10.5))
    
    
  }else if(prec!="SST"){
    Map_x<-gplot(correl_map) + geom_tile(aes(fill = value)) + coord_equal() + 
      scale_fill_gradient2(low="#2166AC",mid = "white", high="#B2182B",name = " ",  limits=c(-1,1)) + 
      labs(title=" ",x="Long",y="Lat")  + theme(legend.key.height=unit(0.5,"cm"),legend.key.width=unit(2,"cm"),
                                                legend.text=element_text(size=10),
                                                panel.background=element_rect(fill="white",colour="black"),
                                                axis.text=element_text(colour="black",size=10),
                                                axis.title=element_text(colour="black",size=10,face="bold"),
                                                legend.position = "bottom", 
                                                legend.title = element_text(size = 10.5))
    
    shp=crop(shp, extent(correl_map)) #  realizar el corte
    
    
    # Corte colombia de acuerdo a las coordenadas asignadas
    shp@data$id <- rownames(shp@data) # cree una nueva variable en el shp
    shp@data$id <- as.numeric(shp@data$id) # digale que es de caracter númerico
    shp2 <- fortify(shp, region="id") # convierta el shp en una tabla de datos
    
    Map_x<-Map_x + geom_polygon(data = shp2, aes(long, lat, group = group), 
                                colour = alpha("gray30", 1/3), size = 0.7, fill=NA) 
  }
  
 
  
  ###### Graficos de y
  # Convierta los datos de las estaciones en trimestrales 
  data<-data_trim(Estaciones_C, a)
  
  # La organización de la información se hace de acuerdo al mes de estudio.
  if(a == 12){ 
    data<-data[data$years_y!=1982,]
  } else  data<-data[data$years_y!=1981,]
  
  
  
  correl_y=0 # inicialice las correlaciones con x
  for(i in 2:length(data)){ # realice las correlaciones para todas las estaciones
    correl_y[i-1]<-cor(data[,i],yserie$X1) # correlaciones entre la estación i y el modo 1 de x
  }
  
  Estacion=names(Estaciones_C) # extraiga los nombres de las estaciones
  coor<-data.frame(t(Estaciones_C[1:2,]), row.names = NULL) # extraiga las coordenadas
  # Cree un data frame con la información de las estaciones y las correlaciones
  datos2<-data.frame(Estacion,Long=coor$cpt.X, Lat=coor$cpt.Y,  Correly=correl_y, row.names = NULL)
  datos2$Correly=round(datos2$Correly ,3) # redondee el valor de las correlaciones a tres cifras
  
  # Corte colombia de acuerdo a las coordenadas asignadas
  col2=extent(xmin, xmax, ymin, ymax) # Coordenadas por departamento
  colombia=crop(colombia,col2) #  realizar el coorte
  colombia@data$id <- rownames(colombia@data) # cree una nueva variable en el shp
  colombia@data$id <- as.numeric(colombia@data$id) # digale que es de caracter númerico
  colombia2 <- fortify(colombia, region="id") # convierta el shp en una tabla de datos
  
  
  # Realice el gráfico de las correlaciones entre las estaciones y el modo 1 de y 
  p <- ggplot(colombia2, aes(x=long,y=lat)) # gráfique el país
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 80")
  p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
  p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.3)
  # Aqui se ingresan los datos de las estaciones
  p <- p + geom_point(data=datos2, aes(x=Long, y=Lat, map_id=Estacion,col=Correly),size=2.5)
  p <- p + scale_color_gradient2(low="#2166AC",mid = "white", high="#B2182B", name=" ", limits=c(-1,1))+ coord_equal()
  p<-  p + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                 legend.text=element_text(size=8),
                 panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=10),
                 axis.title=element_text(colour="black",size=10,face="bold"),
                 #legend.position = "bottom", 
                 legend.title = element_text(size = 10.5))
  # Aqui se colocan los nombres de las estaciones
  p <- p + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
  
  
  ## Gráficos Componentes 
  
  # Se crea una trama de datos con la fecha y las componentes 
  datos<-data.frame(date=data$years_y, X=xserie$X1, Y=yserie$X1, row.names = NULL)
  datos$X=round(datos$X ,4) # redondee los modos 
  datos$Y=round(datos$Y ,4) # redondee los modos 
  datos[datos$X==-999.0000,2:3]=0 # quite los valore NA
  datos[,2:3]=datos[,2:3]*100 # multipliquelos * 100
  
  # gráfico de los modos 
  modos<-  ggplot(datos, aes(date)) +   geom_line(aes(y = X ),  colour="#B2182B") + 
    geom_line(aes(y = Y),  colour="#2166AC")  + 
    geom_hline(yintercept = 0, colour="gray") + theme_bw() + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
    guides(colour = guide_legend(title = " ")) + labs(title=paste("Correlación = ", round(cor(datos$X,datos$Y),3),sep = ""),x="",y="Scores (X roja; Y azul) (*100)") 
  modos <- modos  +   scale_x_continuous(breaks = seq(1982,final_year,3))
  
  #  Guarde los cca_maps de correlaciones
  tiff(paste(ruta,"/cca_maps_",names_file,".tif",sep=""), height=300,width=1600,res=100,
       compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
  grid.arrange(Map_x,modos,p,ncol=5, layout_matrix = rbind(c(1,1,2,3,3)))
  dev.off()
}




ruta_c <- "C:/Users/AESQUIVEL/Desktop/CPT_Linux/cpt_requerimientos/Cross_validated_15_5_10"


maps_dep<-function(dep, a, lead, length_periodo){
# Lectura d eas estaciones para cada departamento.
Estaciones_C <- read.delim(paste(ruta_l,"dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
# Determinación de los limites departamentales y estaciones a dibular en el cap >.<
if(dep=="casanare"){
  xmin<- -73.5; xmax<- -71; ymin<-  4; ymax<-  6
  estaciones_in=data.frame(name="Yopal", Long=-72.388, Lat = 	5.320)
}else if(dep=="cordoba"){
  xmin<- -76.6; xmax<- -74.6; ymin<-  7; ymax<-  10
  estaciones_in=data.frame(name=c("Lorica","Cereté"), Long=c(-75.913,-75.802), Lat = c(9.302,8.840))
}else if(dep=="tolima"){
  xmin<- -76.2; xmax<- -74; ymin<-  2.8; ymax<-  5.5
  estaciones_in=data.frame(name=c("Ibagué","Espinal"), Long=c(-75.148,-74.960), Lat = c(4.430,4.188))
}else if(dep=="valle"){
  xmin<- -77.5; xmax<- -75.6; ymin<-  3; ymax<-  5
  estaciones_in=data.frame(name="La Unión", Long=-76.062, Lat = 4.531)
}else if(dep=="santander"){
  xmin<- -75; xmax<- -72; ymin<- 5; ymax<- 8
  estaciones_in=data.frame(name="Villanueva", Long=-73.21, Lat = 6.64)
}

#############
if(prec=="SST"){
 
  ##### Si la region es optimizada correr este for
  
  zone<-read.csv(paste(ruta,"zone.csv",sep=""), header=T, sep=",")  
  
  zone<-zone[which(zone$prec==prec),c("a", "Coord", dep)]
  print(c("a", "Coord", dep))
  
  lista<-split(zone, zone$a)
  lista<-rep(lista, length(a))
  
   for(i in 1:length(a)){
    xserie <- read.csv(paste(ruta_c, "/X_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    yserie <- read.csv(paste(ruta_c,"/Y_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    names_file <- paste(a[i],"_",lead[i],"_", dep,sep="")
    
    SST<-read.table(paste(ruta,"/",prec,"/",lead[i],".tsv",sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
    ## Conversión a raster
    SST<-rasterize(SST)
    
    var_ocanoAt <-SST[[1:length_periodo[i]]]
    b<-extent(lista[[i]][,3])
    var_ocanoAt=crop(var_ocanoAt, b)
    
    rutas_d<-paste(ruta, "results/", dep, sep = "")
    
    cca_maps(var_ocanoAt,names_file, yserie, Estaciones_C, xserie, lead[i], ruta = rutas_d,  a[i], xmin, xmax, ymin, ymax, estaciones_in, prec)
    print(paste("prueba piloto",i, sep=" "))
  }
}else if(prec!="SST"){
  
  ##### Si la region es optimizada correr este for
  
  zone<-read.csv(paste(ruta,"zone.csv",sep=""), header=T, sep=",")
  
  
  zone<-zone[which(zone$prec==prec),c("a", "Coord", dep)]
  print(c("a", "Coord", dep))
  
  lista<-split(zone, zone$a)
  
  lista<-rep(lista, length(a))
  
  for(i in 1:length(a)){
    xserie <- read.csv(paste(ruta_c, "/X_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    yserie <- read.csv(paste(ruta_c,"/Y_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    names_file <- paste(a[i],"_",lead[i],"_", dep,sep="")
    
    
    SST<-read.table(paste(ruta,"/",prec,"/", lead[i],".tsv", sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
    ## Conversión a raster
    SST<-rasterize(SST, prec)
    var_ocanoAt <-SST[[1:length_periodo[i]]]
    b<-extent(lista[[i]][,3])
    var_ocanoAt<-crop(rotate(var_ocanoAt), b)
    
    
    rutas_d<-paste(ruta, "results/", dep, sep = "")
    
    cca_maps(var_ocanoAt,names_file, yserie, Estaciones_C, xserie, lead[i], ruta = rutas_d,  a[i], xmin, xmax, ymin, ymax, estaciones_in, prec)
    
    print(paste("prueba piloto",i, sep=" "))
  }
}
}

#### Declaración de las constantes
lead<- c("DEF_Nov", "DEF_Aug", "DEF_Jun")
a<- rep(12,3)
length_periodo<- rep(31,3)  # Ancho del periodo de estudio para cada trimestre
cbind.data.frame(a, lead, length_periodo)







dep<-"valle"
maps_dep(dep,a,lead,length_periodo)


### Para todas las regiones en caso que tenga 
dep<-list("casanare", "cordoba", "santander", "tolima", "valle")
sapply(dep, maps_dep, a, lead, length_periodo, simplify = T)





###########################################################
###########################################################
###########################################################







lead<- c("DEF_Nov", "DEF_Aug", "DEF_Jun")
a<- rep(12,3)
length_periodo<- rep(31,3)  # Ancho del periodo de estudio para cada trimestre
cbind.data.frame(a, lead, length_periodo)




## Esta función devuelve las gráficas del goodness index, 
## ruta_c ruta donde se encuentran los archivos
## dep_f son los departamentos que se van a gráficar
GoodnessIndex <- function(ruta_c, dep_f, a, lead){

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
      good_index<-cbind(dep_f[j], a[i] ,lead[i],goodnex[dim(goodnex)[1],5:8])
      # Almacene la fila en un data frame
      GoodnessIndex<-rbind(GoodnessIndex,good_index)
    }
  
  # Quite la primera fila ya que es nula
  GoodnessIndex<-GoodnessIndex[-1,]
  # Quite los nombres de las filas
  rownames(GoodnessIndex)=NULL
  # Cambie los nombres de las columnas
  names(GoodnessIndex)=c("Dep","Mes_ini_Trim", "lead", "modos_x", "modos_y", "modos_cca", "GoodnessIndex")

  Trim<-0
  for(i in 1:dim( GoodnessIndex)[1]){
    if(GoodnessIndex$Mes_ini_Trim[i]<11){
      Trim[i] <- paste0(substr(month.abb[GoodnessIndex$Mes_ini_Trim[i]:(GoodnessIndex$Mes_ini_Trim[i]+2)],1,1), collapse = "")
    }else if(GoodnessIndex$Mes_ini_Trim[i]==11){
      Trim[i] <-  paste(paste0(substr(month.abb[11:12],1,1), collapse = ""), substr(month.abb[1],1,1), sep="")
    }else if(GoodnessIndex$Mes_ini_Trim[i]==12){
      Trim[i] <- paste(substr(month.abb[12],1,1), paste0(substr(month.abb[1:2],1,1), collapse = ""), sep="")
    }
  }
  
  
  GoodnessIndex<-cbind.data.frame(Trimestre=Trim, GoodnessIndex)
  
  #######
  
  setwd(paste(ruta, "/results/", sep=""))
  ### Guarde todos los Goodness Index en un archivo .csv
  write.csv(x = GoodnessIndex, file = "GoodnessIndex.csv",sep = ",")
  
 return(GoodnessIndex)}




## Corrida para todos los archivos
ruta_c
table<-GoodnessIndex(ruta_c = ruta_c, dep_f = dep_f, a = a, lead = lead)



ggplot(table, aes(x = Trimestre, y= GoodnessIndex)) + geom_point(aes(colour=Trimestre)) +
  facet_grid(~Dep) + theme_bw() + theme(legend.position = "none")+
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted")

ggsave("summary_GI.png", width = 7, height = 3)















###########################################################
###########################################################
###########################################################

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
  
  datos<-cbind.data.frame(Trimestre=Trim, datos)
  
return(datos)}


summary_ind<-function(dep,  ruta_c,  a, lead){

  
 tipo <- list("Pearsons_correlation", "Spearmans_correlation", "k_2AFC_Score",
               "Hit_Score", "Hit_Skill_Score", "LEPS_score", "Gerrity_Score",
               "k_2AFC_cat", "k_2AFC_cont", "ROC_below", "ROC_above")
  
 lista<-list()  
 for(i in 1:length(tipo)){
   lista[[i]] <- ind(tipo = tipo[[i]], a, lead, dep, ruta_c)
   names(lista)[i] <-tipo[[i]] 
 }
  

 summary<-Reduce(function(x, y) merge(x, y, all=TRUE), lista)
 
 
 setwd(paste(ruta, "results/", dep, sep = ""))
 
 write.csv(x = summary, file = paste("Perfomance_Measures_",dep,".csv", sep=""))
 # Determinación de los limites departamentales y estaciones a dibular en el cap >.<
 if(dep=="casanare"){
   xmin<- -73.5; xmax<- -71; ymin<-  4; ymax<-  6
   estaciones_in=data.frame(name="Yopal", Long=-72.388, Lat = 	5.320)
 }else if(dep=="cordoba"){
   xmin<- -76.6; xmax<- -74.6; ymin<-  7; ymax<-  10
   estaciones_in=data.frame(name=c("Lorica","Cereté"), Long=c(-75.913,-75.802), Lat = c(9.302,8.840))
 }else if(dep=="tolima"){
   xmin<- -76.2; xmax<- -74; ymin<-  2.8; ymax<-  5.5
   estaciones_in=data.frame(name=c("Ibagué","Espinal"), Long=c(-75.148,-74.960), Lat = c(4.430,4.188))
 }else if(dep=="valle"){
   xmin<- -77.5; xmax<- -75.6; ymin<-  3; ymax<-  5
   estaciones_in=data.frame(name="La Unión", Long=-76.062, Lat = 4.531)
 }else if(dep=="santander"){
   xmin<- -75; xmax<- -72; ymin<- 5; ymax<- 8
   estaciones_in=data.frame(name="Villanueva", Long=-73.21, Lat = 6.64)
 }
 
 
 # Corte colombia de acuerdo a las coordenadas asignadas
 col2=extent(xmin, xmax, ymin, ymax) # Coordenadas por departamento
 colombia_1=crop(colombia,col2) #  realizar el coorte
 colombia_1@data$id <- rownames(colombia_1@data) # cree una nueva variable en el shp
 colombia_1@data$id <- as.numeric(colombia_1@data$id) # digale que es de caracter númerico
 colombia2 <- fortify(colombia_1, region="id") # convierta el shp en una tabla de datos
 
 
 
 
 # Realice el gráfico de las correlaciones entre las estaciones y el modo 1 de y 
 p <- ggplot(colombia2, aes(x=long,y=lat)) # gráfique el país
 p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 80")
 p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
 p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.3)
 
 
 
 
 
 for(i in 1:length(a)){
 #############
 datos2<-summary[summary$Mes_ini==a[i] & summary$lead==lead[i],]
 

 # Aqui se ingresan los datos de las estaciones
 pe <- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=Pearsons_correlation),size=2.5)
 pe <- pe + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue","#2166AC", "steelblue2" , "snow", "pink", "violetred4","#B2182B"))(20),limits=c(-1,1))+ 
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                legend.text=element_text(size=8),
                panel.background=element_rect(fill="white",colour="black"),
                axis.text=element_text(colour="black",size=10),
                axis.title=element_text(colour="black",size=10,face="bold"),
                #legend.position = "bottom", 
                legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 pe <- pe + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 pe <- pe + labs(title="Pearsons correlation")
 
 tiff(filename = paste("Pearsons_correlation_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(pe)
 dev.off()
 
 
 sp<- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=Spearmans_correlation),size=2.5)
 sp <- sp + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue","#2166AC", "steelblue2" , "snow", "pink", "violetred4","#B2182B"))(20),limits=c(-1,1))+
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                   legend.text=element_text(size=8),
                   panel.background=element_rect(fill="white",colour="black"),
                   axis.text=element_text(colour="black",size=10),
                   axis.title=element_text(colour="black",size=10,face="bold"),
                   #legend.position = "bottom", 
                   legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 sp <- sp + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 sp <- sp + labs(title="Spearmans correlation") 

 tiff(filename = paste("Spearmans_correlation_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(sp)
 dev.off()
 
 
 
 afc<- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=k_2AFC_Score),size=2.5)
 afc <-  afc + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue", "steelblue2" , "snow", "khaki", "orange1"))(20),limits=c(0,100))+
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                         legend.text=element_text(size=8),
                         panel.background=element_rect(fill="white",colour="black"),
                         axis.text=element_text(colour="black",size=10),
                         axis.title=element_text(colour="black",size=10,face="bold"),
                         #legend.position = "bottom", 
                         legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 afc <-  afc + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 afc <-  afc + labs(title="2AFC Score") 
 
 tiff(filename = paste("2AFC_Score_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(afc)
 dev.off()
 
 
 HS<- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=Hit_Score),size=2.5)
 HS <-  HS + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue", "steelblue2" , "snow", "khaki", "orange1"))(20),limits=c(0,100))+
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                         legend.text=element_text(size=8),
                         panel.background=element_rect(fill="white",colour="black"),
                         axis.text=element_text(colour="black",size=10),
                         axis.title=element_text(colour="black",size=10,face="bold"),
                         #legend.position = "bottom", 
                         legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 HS <-  HS + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 HS <-  HS + labs(title="Hit Score") 
 
 tiff(filename = paste("Hit_Score_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(HS)
 dev.off()
 
 
 
 HSS<- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=Hit_Skill_Score),size=2.5)
 HSS <- HSS + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue","#2166AC", "steelblue2" , "snow", "pink", "violetred4","#B2182B"))(20),limits=c(-100,100))+
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                         legend.text=element_text(size=8),
                         panel.background=element_rect(fill="white",colour="black"),
                         axis.text=element_text(colour="black",size=10),
                         axis.title=element_text(colour="black",size=10,face="bold"),
                         #legend.position = "bottom", 
                         legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 HSS <- HSS + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 HSS <- HSS + labs(title="Hit Skill Score") 
 
 tiff(filename = paste("Hit_Skill_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(HSS)
 dev.off()
 
 
 
 
 be<- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=ROC_below),size=2.5)
 be <-  be + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue", "steelblue2" , "snow", "khaki", "orange1"))(20),limits=c(0,1))+
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                         legend.text=element_text(size=8),
                         panel.background=element_rect(fill="white",colour="black"),
                         axis.text=element_text(colour="black",size=10),
                         axis.title=element_text(colour="black",size=10,face="bold"),
                         #legend.position = "bottom", 
                         legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 be <-  be + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 be <-  be + labs(title="ROC below") 
 

 tiff(filename = paste("ROC_below_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(be)
 dev.off()
 
 
 
 
 ab<- p + geom_point(data=datos2, aes(x=Lon, y=Lat, map_id=Estacion,colour=ROC_above),size=2.5)
 ab<- ab + scale_colour_gradientn(colours = colorRampPalette(c("midnightblue", "steelblue2" , "snow", "khaki", "orange1"))(20),limits=c(0,1))+
   coord_equal() + theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(0.5,"cm"),
                         legend.text=element_text(size=8),
                         panel.background=element_rect(fill="white",colour="black"),
                         axis.text=element_text(colour="black",size=10),
                         axis.title=element_text(colour="black",size=10,face="bold"),
                         #legend.position = "bottom", 
                         legend.title = element_text(size = 10.5)) 
 # Aqui se colocan los nombres de las estaciones
 ab <-  ab + geom_text(data=estaciones_in,aes(label = substring(name,1,9), x = Long, y=Lat-0.05),size=3) 
 ab <-  ab + labs(title="ROC above") 
 
 tiff(filename = paste("ROC_above_", dep, "_", a[i], "_",lead[i], ".tif", sep=""), height=400,width=650,res=100)
 print(ab)
 dev.off()
 
}
 
} 



cbind.data.frame(a,lead)


summary_ind("cordoba",  ruta_c,  a, lead)



### Para todas las regiones en caso que tenga 
dep<-list("casanare", "cordoba", "santander", "tolima", "valle")
sapply(dep, summary_ind, ruta_c,  a, lead, simplify = T)








#################################################################
#################################################################
#################################################################








