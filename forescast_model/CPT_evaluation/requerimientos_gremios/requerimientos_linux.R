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

initial_year<-1981 ; final_year<-2013

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
  year=sort(rep(initial_year:final_year,12)) # cree un vector para los años
  month=rep(1:12,length(initial_year:final_year)) # cree el vector de meses
  data_station=cbind.data.frame(year,month,stations, row.names=NULL) # cree un data frame 
  pos=seq(a,dim(data_station)[1],12) #  posiciones ne las que se encuentra el mes a
  pos_select=sort(c(pos,pos+1,pos+2)) # muestre las posiciones del trimestre
  # Agregue los datos del trimestre y luego sumelos. 
  data_out=aggregate(data_station[pos_select,-1:-2],by=list(sort(rep(1:(length(pos)),3))),sum)
  data_out_final=na.omit(data_out[,-1]) # Elimine los NA
  years_y=na.omit(year[pos+1]) # Elimine los NA
  data_out_final=data.frame(years_y,data_out_final) # Cree un data frame con los datos finales
  return(data_out_final)} # devuelva los datos finales


