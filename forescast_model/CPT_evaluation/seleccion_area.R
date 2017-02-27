### Paquetes Necesarios
library(raster) 
library(ggplot2)
library(grid)
library(rasterVis)


setwd("C:/Users/AESQUIVEL/Google Drive/new_predictor/")
getwd()


prec="U_wind_850"

# prec= c("U_wind_250","U_wind_850")


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
  }
    
  return(layers_crop)
}



########## solo pasa los datos en formato tabla
data_base=function(list_stack){
  
  data=lapply(list_stack,function(x) t(rasterToPoints(x)))
  data_table=lapply(data,"[",c(-1,-2),)
  years=lapply(data_table,function(x) as.numeric(substr(rownames(x), 2, 5)))
  all_output=list(data_table,years)
  return(all_output)
}




#################  genera los trimestres acumulados de las estaciones de precipitacion
quarterly_data=function(sy_month,data){
  
  l=(sy_month)+(0:2)
  if(sum(l>12)>0)l[which(l>12)]=l[which(l>12)]-12
  if(sum(l<1)>0)l[which(l<1)]=l[which(l<1)]+12
  pos_ini=which(data$month==l[1])
  pos_data=sort(c(pos_ini,pos_ini+1,pos_ini+2))
  data_out=data.frame(na.omit(aggregate(data[pos_data,-1:-2],by=list(sort(rep(1:(length(pos_ini)),3))),sum))[,-1])
  year_out=data[pos_ini+1,1][1:dim(data_out)[1]]
  rownames(data_out)=year_out
  
  all_output=list(data_out,year_out)
  names(all_output)=c("data_stations","year_response")
  
  return(all_output)
  }



#################  algoritmo Nipals
nipals<-function(X,modos){
  
  n<-nrow(X)
  p<-ncol(X)
  X0<-scale(X)*(sqrt(n)/sqrt(n-1))
  T<-matrix(NA,n,modos)#Componentes principales
  P<-matrix(NA,p,modos)#Vectores Propios
  valor<-0
  for(h in 1:modos) 
  {
    th<-as.matrix(X0[,1])
    
    for(i in 1:40)
    {  
      ph<-(t(X0)%*%th)/as.numeric(t(th)%*%th)
      nph<-ph/sqrt(sum(ph^2))#Vectores Normalizados
      ph<-nph
      th<-X0%*%ph
    } 
    valor[h]<-t(th)%*%th/(dim(X0)[1])
    T[,h]<-th
    P[,h]<-ph
    
    X1<-X0-th%*%t(ph)
    X0<-X1
  } 
  rownames(T)<-rownames(X) 
  Resul<-list(T,P,valor)
  return(Resul) 
  
}



#################  algoritmo Nipals
selection_area=function(x,y){
  
  loadings_modos=list()
  count=0
  
  if(dim(y)[2]<10){
    k=dim(y)[2]
    y_pca=nipals(y,modos=k)[[1]]
  }else{
    k=10
    y_pca=nipals(y,modos=10)[[1]]
  }
  
  x_pca=nipals(x,modos=10)[[1]]
  
  for(i in 1:10){
    
    for(j in 1:k){
      
      canonico=cancor(x_pca[,1:i],y_pca[,1:j])
      x_center=scale(x_pca[,1:i],scale = F)
      y_center=scale(y_pca[,1:j],scale = F)  
      com_x=x_center%*%canonico$xcoef
      com_y=y_center%*%canonico$ycoef
      mode1=cbind(com_x[,1],com_y[,1])
      loadings_x=cor(x,mode1[,1])
      Loadings_map=data_stack[[1]][[1]]
      pos_cor=!is.na(Loadings_map[])
      Loadings_map[pos_cor]=loadings_x[,1]
      count=count+1
      loadings_modos[[count]]=Loadings_map
      
    }
    
  }
  
  loadings_stack=mean(abs(stack(loadings_modos)))
  
  return(loadings_stack)
}



plots=function(prec, dep, x,y){
  
  if(require(rasterVis)==FALSE){install.packages("rasterVis")}
  library("rasterVis")
  tiff(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/optimizaciones/", dep, "/", prec, "/",y,".tiff",sep=""),compression = 'lzw',height = 5,width = 16,units="in", res=150)
  myThemec <- BuRdTheme()
  myThemec$regions$col=colorRampPalette(c("snow","red4"))
  myThemec$panel.background$col = "gray"
    
  
  shp=shapefile("C:/Users/AESQUIVEL/Google Drive/shp/mapa_mundi.shp")
  
  
  #myThemec$panel.background$col = "gray30"
  e=levelplot(rotate(x)>quantile(x,0.7,na.rm=T), main=y,par.settings=myThemec,margin=F,colorkey=list(space="right"))+ latticeExtra::layer(sp::sp.lines(shp, lwd=0.8, col="gray30"))
  print(e)
  #writeSpatialShape(sur2, "torn")
  dev.off()
}








######## Run ########
#####################


### la primera es la ruta donde estabas los archivos de la tsm
path<-paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/predictors/", prec, sep="")

files_tsm<-list.files(path)
rutes=paste(path,files_tsm,sep = "/")

data_tsm=lapply(rutes,function(x)read.table(x,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999))

data_stack=lapply(data_tsm,rasterize, prec)
data_raw=data_base(data_stack)
data_tsm=data_raw[[1]]
years_predictor=data_raw[[2]]



dep="santander"



precp=read.table(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/optimizaciones/",dep,".csv", sep = ""),header=T,dec=".",sep=",")

month_ini=c(12,6,3,9)

sta_quar=lapply(month_ini,quarterly_data,precp)




data_quartely=unlist(lapply(sta_quar,"[", 1),recursive=FALSE)
years_response=unlist(lapply(sta_quar,"[", 2),recursive=FALSE)



year_model=Map(function(x1,y1) years_model=intersect(x1,y1),years_predictor, years_response)
years_final_res=Map(function(x1,y1) pos_x=x1%in%y1 ,years_response,year_model)
years_final_prec=Map(function(x1,y1) pos_x=x1%in%y1 ,years_predictor, year_model)

data_tsm_final=Map(function(x1,y1) x1[y1,] , data_tsm ,years_final_prec)
data_res_final=Map(function(x1,y1) x1[y1,,drop=FALSE] ,data_quartely,years_final_res)


final=Map(selection_area,data_tsm_final,data_res_final)

Map(plots,prec, dep, final,month_ini)

levelplot(final[[2]]>quantile(final[[1]],0.7,na.rm=T,marge=F))



#prueba<-raster("C:/Users/AESQUIVEL/Google Drive/new_predictor/map.tif")
#plot(final[[1]])
#extent(prueba)<-extent(0,357.5,-30,30)
#plot(prueba, add=T, col="snow")
#s=writeRaster(final[[1]], "C:/Users/AESQUIVEL/Google Drive/new_predictor/optimizaciones/9.tiff")









