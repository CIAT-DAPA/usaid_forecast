### Paquetes Necesarios
library(raster) 
library(ggplot2)
library(grid)
library(rasterVis)
library(maptools)
library(rgeos)
library(gridExtra)
library(cowplot)




region<-"results_graphs_C"  # "results_graphs_C"    "results_graphs_Op"
prec<-"U_wind_250"    # prec= c("U_wind_250","U_wind_850", "rhum_700", "vertical_vel_250")



### Directorio de trabajo
setwd(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp2/", region, "/", prec, "/" ,sep=""))
getwd()



#### Primero se corre para u-wind 250  y u-wind 850 la lectura inicial de la grilla. 

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
  ## Muestra las posiciones de las filas en la tabla que no contienen información relevante para el raster
  #total_row_delete=c(-1,-3,-(which(dates[,1]=="90")-2),-which(dates[,1]=="-90"),-which(dates[,1]==""))
  
  total_row_delete=c(-1,-3,-(which(dates[,1]=="90.0")-2),-which(dates[,1]=="-90.0"),-which(dates[,1]==""))
  dates_1=dates[total_row_delete,-1] # Elimina la información no relevante
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





######################################
######################################
######################################
######################################


## Ruta principal donde se encuentran las carpetas con los archivos  
ruta = paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp2/", region, "/", prec, "/" ,sep="")



ruta_l="C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/"
### Lectura del shp
colombia=shapefile(paste(ruta_l,"/colombia/colombia_depts.shp",sep=""))

shp=shapefile("C:/Users/AESQUIVEL/Google Drive/shp/mapa_mundi.shp")





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
cca_maps<-function(var_ocanoAt, yserie, Estaciones_C, xserie, names_file, ruta,  a, xmin, xmax, ymin, ymax, estaciones_in){
  
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
  Map_x<-gplot(rotate(correl_map)) + geom_tile(aes(fill = value)) + coord_equal() + 
    scale_fill_gradient2(low="#2166AC",mid = "white", high="#B2182B",name = " ",  limits=c(-1,1)) + 
    labs(title=" ",x="Long",y="Lat")  + theme(legend.key.height=unit(0.5,"cm"),legend.key.width=unit(2,"cm"),
                                              legend.text=element_text(size=10),
                                              panel.background=element_rect(fill="white",colour="black"),
                                              axis.text=element_text(colour="black",size=10),
                                              axis.title=element_text(colour="black",size=10,face="bold"),
                                              legend.position = "bottom", 
                                              legend.title = element_text(size = 10.5))
  
  # Corte colombia de acuerdo a las coordenadas asignadas
  shp=crop(shp, extent(rotate(correl_map))) #  realizar el coorte
  shp@data$id <- rownames(shp@data) # cree una nueva variable en el shp
  shp@data$id <- as.numeric(shp@data$id) # digale que es de caracter númerico
  shp2 <- fortify(shp, region="id") # convierta el shp en una tabla de datos
  
  
  Map_x<-Map_x + geom_polygon(data = shp2, aes(long, lat, group = group), 
                              colour = alpha("gray30", 1/3), size = 0.7, fill=NA) 
  
  
  
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
  modos <- modos  +   scale_x_continuous(breaks = seq(1982,2012,3))
  

  #  Guarde los cca_maps de correlaciones
  tiff(paste(ruta,"results/",dep,"/cca_maps_",names_file,".tif",sep=""), height=300,width=1600,res=100,
       compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
  grid.arrange(Map_x,modos,p,ncol=5, layout_matrix = rbind(c(1,1,2,3,3)))
  #grid.arrange(Map_x,p,modos, layout_matrix = rbind(c(1,1),c(2,3)))
  dev.off()
}


dep= "casanare" # variar el departamento

# "casanare"    "cordoba"    "tolima"    "valle" "santander"
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


# Lead times (names)
lead<-c(paste(rep("MAM",3),c("Feb","Nov","Sep"), sep="_"),
        paste(rep("JJA",3),c("May","Feb","Dec"), sep="_"),
        paste(rep("SON",3),c("Aug","May","Mar"), sep="_"),
        paste(rep("DEF",3),c("Nov","Aug","Jun"), sep="_"))

# trimester
a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))

#### Declaración de las constantes
length_periodo=rep(c(32,31,32,31,32,31),c(1,2,2,1,3,3)) # Ancho del periodo de estudio para cada trimestre

# Lectura d eas estaciones para cada departamento.
Estaciones_C <- read.delim(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp2/dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
if(region=="results_graphs_C"){
  # Lea los archivos y las estaciones necesarias para todas las corridas del departamento. 
  for(i in 1:length(lead)){
    xserie <- read.csv(paste(ruta_c, "X_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    yserie <- read.csv(paste(ruta_c,"Y_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    names_file <- paste(a[i],"_",lead[i],"_", dep,sep="")
    
    ##### Cambiar las rutas para leer los archivos

    SST<-read.table(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp2/predictors/", prec, "/",lead[i],".tsv", sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
    ## Conversión a raster
    SST=rasterize(SST, prec)
    var_ocanoAt <-SST[[1:length_periodo[i]]]
    
    cca_maps(var_ocanoAt, yserie, Estaciones_C, xserie, names_file, ruta,  a[i], xmin, xmax, ymin, ymax, estaciones_in)
    print(i)
  }  
}else if(region=="results_graphs_Op"){
  ##### Si la region es optimizada correr este for
  print("En proceso")
}


















