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
prec="U_wind_250"           # prec= c("U_wind_250","U_wind_850", "rhum_700", "vertical_vel_250")




### Directorio de trabajo
setwd(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/", region, "/", prec, "/" ,sep=""))
getwd()




#### Primero se corre para u-wind 250  y u-wind 850 la lectura inicial de la grilla. 

transform_raster=function(x){ 
  # Primero se crea un raster teniendo encuenta la resolución espacial de la tabla .tsv  
  mapa_base=raster(nrows=74, ncols=144,xmn=0,xmx=357.5, ymn=-90,ymx=90) # Dimensiones del raster
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
  year=substr(year_month[-1],1,4) # Substrae el año de las fehcas
  ## Muestra las posiciones de las filas en la tabla que no contienen información relevante para el raster
  #total_row_delete=c(-1,-3,-(which(dates[,1]=="90")-2),-which(dates[,1]=="-90"),-which(dates[,1]==""))
  
  total_row_delete=c(-1,-(which(dates[,1]=="90")-2),-which(dates[,1]==""))
  
  dates_1=dates[total_row_delete,-1] # Elimina la información no relevante
  
  list_dates=split(dates_1,sort(rep(year,74))) # Se divide la tabla de datos por año
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
  return(data_out_final)} # devuelva los datos finales




######################################
######################################
######################################
######################################


## Ruta principal donde se encuentran las carpetas con los archivos  
ruta= paste("C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/Salidas_",region,sep="")

ruta1="C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/"
### Lectura del shp
colombia=shapefile(paste(ruta1,"/colombia/colombia_depts.shp",sep=""))















