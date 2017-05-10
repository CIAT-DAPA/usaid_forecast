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




region<-"results_graphs_Op"  # "results_graphs_C"    "results_graphs_Op"
prec<-"vertical_vel_250"    # prec= c("U_wind_250","U_wind_850", "rhum_700", "vertical_vel_250")




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
ruta <- paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/", region, "/", prec, "/" ,sep="")



ruta_l<-"C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/"
### Lectura del shp
colombia=shapefile(paste(ruta_l,"/colombia/colombia_depts.shp",sep=""))

shp=shapefile("C:/Users/AESQUIVEL/Google Drive/shp/mapa_mundi.shp")




####### Organizar el directorio correctamente
dep_f<-c("casanare",    "cordoba",    "tolima",    "valle", "santander")
if(dir.exists("results")==FALSE){dir.create("results")}

setwd(paste( getwd(),"/results/" ,sep=""))
print(getwd())

if(list.dirs()=="."){lapply(dep_f, dir.create)}

#setwd(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/results_graphs_C/", prec, "/", sep=""))
getwd()





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
cca_maps<-function(var_ocanoAt, yserie, Estaciones_C, xserie, lead, ruta,  a, xmin, xmax, ymin, ymax, estaciones_in, region){
  
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
  if(region=="results_graphs_C"){
    Map_x<-gplot(rotate(correl_map)) + geom_tile(aes(fill = value)) + coord_equal() + 
      scale_fill_gradient2(low="#2166AC",mid = "white", high="#B2182B",name = " ",  limits=c(-1,1)) + 
      labs(title=" ",x="Long",y="Lat")  + theme(legend.key.height=unit(0.5,"cm"),legend.key.width=unit(2,"cm"),
                                                legend.text=element_text(size=10),
                                                panel.background=element_rect(fill="white",colour="black"),
                                                axis.text=element_text(colour="black",size=10),
                                                axis.title=element_text(colour="black",size=10,face="bold"),
                                                legend.position = "bottom", 
                                                legend.title = element_text(size = 10.5))
    
    shp=crop(shp, extent(rotate(correl_map))) #  realizar el coorte
  }else if(region=="results_graphs_Op"){
    Map_x<-gplot(correl_map) + geom_tile(aes(fill = value)) + coord_equal() + 
      scale_fill_gradient2(low="#2166AC",mid = "white", high="#B2182B",name = " ",  limits=c(-1,1)) + 
      labs(title=" ",x="Long",y="Lat")  + theme(legend.key.height=unit(0.5,"cm"),legend.key.width=unit(2,"cm"),
                                                legend.text=element_text(size=10),
                                                panel.background=element_rect(fill="white",colour="black"),
                                                axis.text=element_text(colour="black",size=10),
                                                axis.title=element_text(colour="black",size=10,face="bold"),
                                                legend.position = "bottom", 
                                                legend.title = element_text(size = 10.5))
    
    shp=crop(shp, extent(correl_map)) #  realizar el coorte
  }
  
  # Corte colombia de acuerdo a las coordenadas asignadas
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


ruta_c<-paste(ruta, "Cross_validated/",sep="")

dep<- "valle" # variar el departamento

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


length_periodo=c(rep(32,3), rep(31,1)) # Ancho del periodo de estudio para cada trimestre
# Lead times (nombres)
lead<-c("MAM",	"JJA", "SON",  "DEF")
# Timestre
a<- c(3,6,9,12)

## Para cada estación lea las estaciones de interes. 

# Lectura d eas estaciones para cada departamento.
Estaciones_C <- read.delim(paste(ruta_l,"dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")


if(region=="results_graphs_C"){
  # Lea los archivos y las estaciones necesarias para todas las corridas del departamento. 
  for(i in 1:length(lead)){
    xserie <- read.csv(paste(ruta_c, "X_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    yserie <- read.csv(paste(ruta_c,"Y_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    names_file <- paste(a[i],"_",lead[i],"_", dep,sep="")
  
    SST<-read.table(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/predictors/", prec, "/",lead[i],".tsv", sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
    ## Conversión a raster
    SST=rasterize(SST, prec)
    var_ocanoAt <-SST[[1:length_periodo[i]]]
    
    cca_maps(var_ocanoAt, yserie, Estaciones_C, xserie, names_file, ruta,  a[i], xmin, xmax, ymin, ymax, estaciones_in)
  }  
}else if(region=="results_graphs_Op"){
  ##### Si la region es optimizada correr este for

  zone<-read.csv("C:/Users/AESQUIVEL/Google Drive/new_predictor/optimizaciones/zone.csv", header=T, sep=",")
  
  # Si se quiere leer directo desde excel
  #prueba<-read_excel("C:/Users/AESQUIVEL/Google Drive/new_predictor/optimizaciones/areas.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "",
  #           skip = 0)
  
  zone<-zone[which(zone$prec==prec),c("a", "Coord", dep)]
  print(c("a", "Coord", dep))
  
  lista<-split(zone, zone$a)
  #lista<-rep(lista, each=3)
  
  for(i in 1:length(lead)){
    xserie <- read.csv(paste(ruta_c, "X_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    yserie <- read.csv(paste(ruta_c,"Y_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
    names_file <- paste(a[i],"_",lead[i],"_", dep,sep="")
    
    SST<-read.table(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/predictors/", prec, "/",lead[i],".tsv", sep=""),sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)
    ## Conversión a raster
    SST<-rasterize(SST, prec)
    var_ocanoAt <-SST[[1:length_periodo[i]]]
    b<-extent(lista[[i]][,3])
    var_ocanoAt<-crop(rotate(var_ocanoAt), b)
    
    cca_maps(var_ocanoAt, yserie, Estaciones_C, xserie, lead[i], ruta,  a[i], xmin, xmax, ymin, ymax, estaciones_in, region)
    print(paste("prueba piloto",i, sep=" "))
  }

}






####### Goodnex Index


## Esta función devuelve las gráficas del goodness index, 
## ruta_c ruta donde se encuentran los archivos
## dep_f son los departamentos que se van a gráficar
GoodnessIndex <- function(ruta_c,dep_f){
  #### Lead times para cada trimestre
  lead<-c("MAM", "JJA", "SON",  "DEF")
  ### Trimestres
  a<- c(rep(3,1),rep(6,1),rep(9,1),rep(12,1))
  
  good_index=0 # inicialice el vector
  GoodnessIndex=NA # inicialice el data frame
  for(j in 1:length(dep_f)) # realice esto para todos los departamentos
    for(i in 1:length(lead)){ # barra para todos los periodos con sus respectivos lead time
      # Lea la tabla 
      goodnex<-read.table(paste(ruta_c,"GoodnessIndex_",a[i],"_",lead[i],"_precip_",dep_f[j],".txt", sep=""),  skip=6, colClasses=c(rep("numeric",3),"character",rep("numeric",4)))
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
  names(GoodnessIndex)=c("dep","a", "lead", "modos_x", "modos_y", "modos_cca", "GoodnessIndex")
  #GoodnessIndex <- cbind(GoodnessIndex,lead_time=rep(rep(c(0,1,3),4),length(dep_f))  )
  GoodnessIndex <- cbind(GoodnessIndex,lead_time=rep(rep(0,4),length(dep_f))  )  
  ### Guarde todos los Goodness Index en un archivo .csv
  write.csv(x = GoodnessIndex, file = "GoodnessIndex.csv",sep = ",")
  
  
  GoodnessIndex$a[GoodnessIndex$a==12]=0 # Cambiarle el número para que diciembre aparezca primero
  # Realice el gráfico de cajas para todos los departamentos.
  for(i in 1:length(dep_f)){
    dep_inf<-  GoodnessIndex[GoodnessIndex$dep==dep_f[i],]
    to_string  <- as_labeller(c(`0`="DEF",`3` = "MAM", `6` = "JJA", `9` = "SON"))
    graph_dep  <- ggplot(dep_inf, aes(x = factor(a), y = GoodnessIndex)) 
    graph_dep  <- graph_dep  + geom_bar(stat = 'identity', fill="gray60") + ylim(-0.05,0.5)
    #graph_dep  <- graph_dep  + facet_wrap(~a, nrow = 1, labeller = to_string )
    graph_dep  <- graph_dep  + scale_x_discrete(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON")) +  
      labs(x="Trimester", y=paste("Goodness Index", sep=""))
      #scale_x_discrete(breaks=c(0, 1, 3),labels=c("Sim", "0", "3")) 
    graph_dep  <-graph_dep  + theme_bw() + theme(legend.position = "none") 
    
      
    tiff(paste(ruta,"results/",dep_f[i],"/GoodnessIndex_dep_",dep_f[i],".tif",sep=""), height=720,width=1280,res=200,
         pointsize=2,compression="lzw")
    print(graph_dep  )
    dev.off()
    
  }
  
  
  
  ### Grafico de Linea puede hacerse solopara el lead simultaneo o para los maximos
  ### modificando la función 
  Sim=GoodnessIndex[GoodnessIndex$lead_time==0,]
  names(Sim)[1]="Departamento"
  levels(Sim$Departamento)<-c("Casanare", "Cordoba", "Tolima",  "Valle", "Santander")
  graph_line  <- ggplot(Sim, aes(x =a, y = GoodnessIndex, color=Departamento))
  graph_line  <- graph_line + geom_line(aes(linetype=Departamento), size=1) + ylim(-0.05,0.55)
  graph_line  <- graph_line + geom_point(aes(shape=Departamento), size=2)
  graph_line  <- graph_line + theme_bw()  + labs(x="", y="Goodness Index") 
  graph_line  <- graph_line +  scale_x_continuous(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON"))
  graph_line  <- graph_line + theme(legend.title = element_text(size = 10.5),
                                    legend.key.height=unit(0.5,"cm"),
                                    legend.key.width=unit(0.8,"cm"))
  
  
  # Guarde el graph
  tiff(paste(ruta,"results/GoodnessIndex_line.tif",sep=""), height=720,width=1280,res=200,
       pointsize=2,compression="lzw")
  print(graph_line)
  dev.off()
  
  return(GoodnessIndex)}

## Corrida para todos los archivos
dep_f<-c("casanare",    "cordoba",    "tolima",    "valle", "santander")
table<-GoodnessIndex(ruta_c = ruta_c, dep_f = dep_f)








# Boxplot comparando Indicadores por departamento 
# ruta # ruta donde se almacenan las salidas
#### Ruta_c = es la ruta donde se encuentran los archivos del indicador
#### tipo = es el indicador que se va a gráficar (tiene que tener el mismo nombre que lso archivos)
#### Esta función permite realizar el gráfico de caja y alambre de un indicador de CPT
#### Tambien devuelve la trama de datos, además depende de la ruta de salidas que debe llamarse ruta
grahs_indicador <- function(ruta_c, tipo){
  mes_inicio=seq(3,12,3) # Meses de inicio de los trimestres 
  lead_0=c("MAM", "JJA", "SON", "DEF") # trimestres simultaneos 
  
  
  
  names=list() # cree una lista con los nombres
  for(i in 1:length(mes_inicio)){ # itere el número de trimestres y liste los archivos
    names[[i]]=list.files(ruta_c,pattern=paste(tipo,"_", mes_inicio[i], "_", lead_0[i], sep=""))  
  }
  
  names=unlist(names) # convierta los nombres de los archivos de lista a vector
  lead_1=c(rep("MAM",5), rep("JJA",5), rep("SON",5), rep("DEF",5)) # trimestres 
  numeral=c(rep(3,5), rep(6,5), rep(9,5), rep(1,5)) # trimestres 
  #(el número de repeticiones depende del número de departamentos)
  dep=rep(c("casanare",    "cordoba",   "santander",   "tolima",   "valle"),4) # Esto se tiene que modificar de acuerdo al número de departamentos
  #(el número de repeticiones depende del número de trimestres)
  datos<-NA # datos
  ## Cree la tabla variando por departamento
  for(i in 1:length(dep)){ # lea los archivos como tablas y luego almacenelos en una base global
    data_p=read.table(paste(ruta_c,names[i],sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric") )
    data_p=data.frame(rep(numeral[i],dim(data_p)[1]),rep(lead_1[i],dim(data_p)[1]),rep(dep[i],dim(data_p)[1]), data_p)
    datos=rbind(datos, data_p) # Lectura de los datos y creación de la tabla global 
  }
  
  # Depuración de los datos
  datos=datos[-1,] # Elimine la primera fila NA
  names(datos)=c("Trimestre","Trimestre_l", "Departamento","Estacion","Lat","Long","Indicador") # Asignarle nombres a las columnas
  datos$Indicador=round(datos$Indicador,3) # Redondear el número de decimales de la curva
  datos2=datos[datos$Indicador!=-999,] # Eliminar los datos faltantes
  
  levels(datos2$Departamento) <- c("Casanare", "Cordoba", "Santander", "Tolima",  "Valle")
  
  ### Condicionante que depende del tipo de indicador que se esta gráficando. 
  if(tipo=="Pearsons_correlation"){
    tit="Pearsons correlation"
    lim=c(-0.62,1)
  }else if(tipo=="k_2AFC_Score"){
    tit="2AFC Score"
    lim=c(20,80)
  }else if(tipo=="Hit_Skill_Score"){
    tit="Hit Skill Score"
    lim=c(-25,60)
  }else if(tipo=="ROC_below"){
    tit="ROC below"
    lim=c(0,1)
  }else if(tipo=="ROC_above"){
    tit="ROC above"
    lim=c(0,1)
  }
  
  
  # Realice el gráfico de caja y alambres
  to_string  <- as_labeller(c(`1`="DEF",`3` = "MAM", `6` = "JJA", `9` = "SON"))
  box <- ggplot(datos2, aes(Departamento, Indicador, fill = Departamento))
  box <- box + geom_boxplot() + ylim(lim)
  box <- box + facet_wrap(~ Trimestre, nrow = 1, labeller = to_string )
  box <- box + labs(y=tit, xlab="")
  box <- box + theme_bw() + theme(axis.text.x=element_blank(), 
                                  axis.ticks.x=element_blank(), 
                                  axis.line = element_line(colour = "blue", size = 2),    
                                  legend.title = element_text(size = 10.5), 
                                  legend.key.height=unit(0.8,"cm"),legend.key.width=unit(0.8,"cm"))
  
  
  
  # Guarde el gráfico en la ruta de salidas (ruta)
  tiff(paste(ruta,"/results/",tipo,"_sim",".tif",sep=""), height=600,width=1280,res=200,
       pointsize=2,compression="lzw") # height=720,
  print(box)
  dev.off()
  return(datos2)} # la función devuelve la trama de datos en caso de ser necesaria


tipo <- c("Pearsons_correlation","k_2AFC_Score",
          "Hit_Skill_Score","ROC_below","ROC_above")

for(i in  1:length(tipo)){
  indicador<-grahs_indicador(ruta_c = ruta_c, tipo = tipo[i])  
}









### Gráfico Indicadores 

### Este gráfico realiza un resumen de un indicador por sitos
### Gráficando todos los lead times y trimestres de estudio. 
### Tipo = indicador que se desea gráficar
### ruta_c <- ruta donde se encuentran los datos de los indicadores. 
summary_ind<-function(ruta_c, tipo){
  # lead time (nombres de los archivos)
  lead<-c("MAM", "JJA",	"SON", "DEF")
  lead_num<-rep(c("sim"),4) # número de los archivos
  a<- c(rep(3,1),rep(6,1),rep(9,1),rep(12,1)) # trimestres de estudio
  
  
  
  # nombre de los sitios d eestudios (como apareceran en el gráfico)
  estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima"
                ,"Turipana", "StaIsabel")
  
  
  datos<-NA # Creación de una trama de datos
  
  # lea los datos y apilelos en una tabla de datos
  for(j in 1:length(lead)){
    names_below<-list.files(ruta_c, pattern = paste(tipo, "_", a[j], "_", lead[j], sep=""))
    for(i in 1:5){
      # Ciclo for que barre el número de departamentos que existan 
      # En caso de desear más departamentos modificar este argumento
      data_p<-read.table(paste(ruta_c,names_below[i],sep=""), sep="",  skip=3,
                         colClasses=c("character","numeric","numeric","numeric") )
      data_p<-data.frame(Trimestre=rep(a[j], dim(data_p)[1]),  lead_num=rep(lead_num[j],
                                                                            dim(data_p)[1]), data_p)
      datos<-rbind(datos, data_p) # Lectura de los datos y creación de la tabla global
    }
  } 
  
  
  
  
  # Depuración de la base de datos 
  datos<-datos[-1,] # Elimine la primera fila NA
  datos<-datos[,-4:-5]
  names(datos)<-c("Trimestre", "lead_num", "Estacion","ROC_B") # Asignarle nombres a las columnas
  datos$ROC_B<-round(datos$ROC_B,3) # Redondear el número de decimales de la curva
  datos<-datos[datos$ROC_B!=-999,] # Eliminar los datos faltantes
  datos<-rbind(datos[datos$Trimestre==12, ], datos[datos$Trimestre!=12, ])
  datos$Trimestre<-factor(datos$Trimestre)
  levels(datos$Trimestre)<-c("TR2", "TR3", "TR4", "TR1")
  
  
  ## Ahora depure toda la información solo para los sitios de estudio
  res.shape2<-NA
  for(i in 1:7){  #numero de estaciones
    datos_t<-datos[datos$Estacion == estaciones[i],]
    res.shape2<-rbind(datos_t,res.shape2)
  }
  res.shape2<-data.frame(res.shape2, row.names = NULL)
  res.shape2<-res.shape2[-dim(res.shape2)[1],]
  
  
  # Shape
  # Cree una nueva variable 
  res.shape2 <- cbind(res.shape2,f1=rep(1:28,each=1)) # 28 equivale a # de trimestre * sitios
  
  ##  Cree los limites del gráfico y el titulo del eje de acuerdo al indicador
  if(tipo=="Pearsons_correlation"){
    tit="Pearsons correlation"
    lim=c(-0.62,1)
  }else if(tipo=="k_2AFC_Score"){
    tit="2AFC Score"
    lim=c(20,80)
  }else if(tipo=="Hit_Skill_Score"){
    tit="Hit Skill Score"
    lim=c(-25,60)
  }else if(tipo=="ROC_below"){
    tit="ROC below"
    lim=c(0,1)
  }else if(tipo=="ROC_above"){
    tit="ROC above"
    lim=c(0,1)
  }
  
  
  tiff(paste(ruta,"/results/a_",tipo,".tif",sep=""), height=800,width=1800,res=150,
       compression="lzw")
  
  # Cree el gráfico condicionando por la nueva variable
  plot(ROC_B~f1,data=res.shape2,type="n",xlab=" ",
       ylab=tit,xaxt="n",bty="n", ylim=lim)#
  
  #abline(h=0.95,col=2,lty=2)
  
  axis(side=1,labels=rep(c("DEF","MAM","JJA","SON"),7),at=1:28,las=2) # dibuje el eje x
  
  abline(v=seq(4.5,28,4),lty=2)# grafique las lineas que dividan lso sitios
  
  #sitios = unique(res.shape2$Estacion)
  sitios<-c("Cereté","Espinal","La Unión","Ibagué","Yopal","Lorica", "Villanueva") # Orden en que se grafican lso sitios
  # Ponga los titulos
  text(sitios[7],y=lim[2],x=2.5)
  text(sitios[1],y=lim[2],x=6.5)
  text(sitios[2],y=lim[2],x=10.4)
  text(sitios[3],y=lim[2],x=14.5)
  text(sitios[4],y=lim[2],x=18.5)
  text(sitios[5],y=lim[2],x=22.5)
  text(sitios[6],y=lim[2],x=26.5)
  
  # Gráfique los puntos para cada lead time
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="sim",pch=19,col=2)

  
  
  ## Cree la leyenda por separado 
  #plot(1, type = "n", axes = FALSE, ann = FALSE)
  #legend("center", "(x,y)",unique(res.shape2$lead_num),
  #pch=c(1,20,1), col=c(2,1,1),title="Lead-Time")
  
  dev.off()
} # Termine la función 


# Shape
tipo <- c("Pearsons_correlation","k_2AFC_Score",
          "Hit_Skill_Score","ROC_below","ROC_above")
# Corra la función para todos los indicadores
for(i in  1:length(tipo)){
  summary_ind(ruta_c = ruta_c, tipo = tipo[i])  
}





###################################################################################

########################### Analisis Retrospectivos 



ruta_r<-paste(ruta, "retroactive/",sep="")
  
  
  
  



# Boxplot comparando Indicadores por departamento 
# ruta # ruta donde se almacenan las salidas
#### Ruta_c = es la ruta donde se encuentran los archivos del indicador
#### tipo = es el indicador que se va a gráficar (tiene que tener el mismo nombre que lso archivos)
#### Esta función permite realizar el gráfico de caja y alambre de un indicador de CPT
#### Tambien devuelve la trama de datos, además depende de la ruta de salidas que debe llamarse ruta
grahs_indicador_r <- function(ruta_r, tipo){
  mes_inicio=seq(3,12,3) # Meses de inicio de los trimestres 
  lead_0=c("MAM", "JJA", "SON", "DEF") # trimestres simultaneos 
  
  names=list() # cree una lista con los nombres
  for(i in 1:length(mes_inicio)){ # itere el número de trimestres y liste los archivos
    names[[i]]=list.files(ruta_r,pattern=paste(tipo,"_", mes_inicio[i], "_", lead_0[i], sep=""))  
  }
  
  names=unlist(names) # convierta los nombres de los archivos de lista a vector
  
  
  lead_1=c(rep("MAM",5), rep("JJA",5), rep("SON",5), rep("DEF",5)) # trimestres 
  numeral=c(rep(3,5), rep(6,5), rep(9,5), rep(1,5)) # trimestres 
  #(el número de repeticiones depende del número de departamentos)
  dep=rep(c("casanare",    "cordoba",   "santander",   "tolima",   "valle"),4) # Esto se tiene que modificar de acuerdo al número de departamentos
  #(el número de repeticiones depende del número de trimestres)
  datos<-NA # datos
  
  for(i in 1:length(dep)){ # lea los archivos como tablas y luego almacenelos en una base global
    data_p=read.table(paste(ruta_r,names[i],sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric") )
    data_p=data.frame(rep(numeral[i],dim(data_p)[1]),rep(lead_1[i],dim(data_p)[1]),rep(dep[i],dim(data_p)[1]), data_p)
    datos=rbind(datos, data_p) # Lectura de los datos y creación de la tabla global 
  }
  
  
  datos=datos[-1,] # Elimine la primera fila NA
  names(datos)=c("Trimestre","Trimestre_l", "Departamento","Estacion","Lat","Long","Indicador") # Asignarle nombres a las columnas
  datos$Indicador=round(datos$Indicador,3) # Redondear el número de decimales de la curva
  datos2=datos[datos$Indicador!=-999,] # Eliminar los datos faltantes
  
  levels(datos2$Departamento) <- c("Casanare", "Cordoba", "Santander", "Tolima",  "Valle")
  
  
  if(tipo=="Pearsons_correlation"){
    tit="Pearsons correlation"
    lim=c(-0.62,1)
  }else if(tipo=="Hit_Skill_Score"){
    tit="Hit Skill Score"
    lim=c(-25,60)
  }else if(tipo=="ROC_below"){
    tit="ROC below"
    lim=c(0,1)
  }else if(tipo=="ROC_above"){
    tit="ROC above"
    lim=c(0,1)
  }
  
  
  # Realice el gráfico de caja y alambres
  to_string  <- as_labeller(c(`1`="DEF",`3` = "MAM", `6` = "JJA", `9` = "SON"))
  box <- ggplot(datos2, aes(Departamento, Indicador, fill = Departamento))
  box <- box + geom_boxplot() + ylim(lim)
  box <- box + facet_wrap(~ Trimestre, nrow = 1, labeller = to_string )
  box <- box + labs(y=tit, xlab="")
  box <- box + theme_bw() + theme(axis.text.x=element_blank(), 
                                  axis.ticks.x=element_blank(), 
                                  axis.line = element_line(colour = "blue", size = 2),    
                                  legend.title = element_text(size = 10.5), 
                                  legend.key.height=unit(0.8,"cm"),legend.key.width=unit(0.8,"cm"))
  
  
  
  # Guarde el gráfico en la ruta de salidas (ruta)
  tiff(paste(ruta,"/results/retro_",tipo,"_sim",".tif",sep=""), height=600,width=1280,res=200,
       pointsize=2,compression="lzw") # height=720,
  print(box)
  dev.off()
  return(datos2)} # la función devuelve la trama de datos en caso de ser necesaria

tipo <- c("Pearsons_correlation",
          "Hit_Skill_Score","ROC_below","ROC_above")

for(i in  1:length(tipo)){
  indicador_r <-grahs_indicador_r (ruta_r = ruta_r, tipo = tipo[i])  
}







### Gráfico Indicadores 


summary_ind_r<-function(ruta_r, tipo){
  
  # lead time (nombres de los archivos)
  lead<-c("MAM", "JJA",	"SON", "DEF")
  lead_num<-rep(c("sim"),4) # número de los archivos
  a<- c(rep(3,1),rep(6,1),rep(9,1),rep(12,1)) # trimestres de estudio
  
  
  
  # nombre de los sitios d eestudios (como apareceran en el gráfico)
  estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima"
                ,"Turipana", "StaIsabel")
  
  
  datos<-NA # Creación de una trama de datos
  
  for(j in 1:length(lead)){
    names_below<-list.files(ruta_r, pattern = paste(tipo, "_", a[j], "_", lead[j], sep=""))
    for(i in 1:5){
      # Ciclo for que barre el número de departamentos que existan 
      # En caso de desear más departamentos modificar este argumento
      data_p<-read.table(paste(ruta_r,names_below[i],sep=""), sep="",  skip=3,colClasses=c("character","numeric","numeric","numeric") )
      data_p<-data.frame(Trimestre=rep(a[j], dim(data_p)[1]),  lead_num=rep(lead_num[j], dim(data_p)[1]), data_p)
      datos<-rbind(datos, data_p) # Lectura de los datos y creación de la tabla global
    }
  } 
  
  
  
  
  
  datos<-datos[-1,] # Elimine la primera fila NA
  datos<-datos[,-4:-5]
  names(datos)<-c("Trimestre", "lead_num", "Estacion","ROC_B") # Asignarle nombres a las columnas
  datos$ROC_B<-round(datos$ROC_B,3) # Redondear el número de decimales de la curva
  datos<-datos[datos$ROC_B!=-999,] # Eliminar los datos faltantes
  datos<-rbind(datos[datos$Trimestre==12, ], datos[datos$Trimestre!=12, ])
  datos$Trimestre<-factor(datos$Trimestre)
  levels(datos$Trimestre)<-c("TR2", "TR3", "TR4", "TR1")
  
  
  
  res.shape2<-NA
  for(i in 1:7){
    datos_t<-datos[datos$Estacion == estaciones[i],]
    res.shape2<-rbind(datos_t,res.shape2)
  }
  res.shape2<-data.frame(res.shape2, row.names = NULL)
  res.shape2<-res.shape2[-dim(res.shape2)[1],]
  
  
  # Shape
  
  res.shape2 <- cbind(res.shape2,f1=rep(1:28,each=1))
  
  if(tipo=="Pearsons_correlation"){
    tit="Pearsons correlation"
    lim=c(-0.62,1)
  }else if(tipo=="Hit_Skill_Score"){
    tit="Hit Skill Score"
    lim=c(-25,60)
  }else if(tipo=="ROC_below"){
    tit="ROC below"
    lim=c(0,1)
  }else if(tipo=="ROC_above"){
    tit="ROC above"
    lim=c(0,1)
  }
  
  
  tiff(paste(ruta,"/results/retroa_",tipo,".tif",sep=""), height=800,width=1800,res=150,
       compression="lzw")
  # layout(matrix(c(1,1,1,1,1,1,1,1,2), 1,9, byrow = TRUE))
  plot(ROC_B~f1,data=res.shape2,type="n",xlab=" ",
       ylab=tit,xaxt="n",bty="n", ylim=lim)#
  
  #abline(h=0.95,col=2,lty=2)
  
  axis(side=1,labels=rep(c("DEF","MAM","JJA","SON"),7),at=1:28,las=2) # dibuje el eje x
  
  abline(v=seq(4.5,28,4),lty=2)# grafique las lineas que dividan lso sitios
  
  #sitios = unique(res.shape2$Estacion)
  sitios<-c("Cereté","Espinal","La Unión","Ibagué","Yopal","Lorica", "Villanueva") # Orden en que se grafican lso sitios
  # Ponga los titulos
  text(sitios[7],y=lim[2],x=2.5)
  text(sitios[1],y=lim[2],x=6.5)
  text(sitios[2],y=lim[2],x=10.4)
  text(sitios[3],y=lim[2],x=14.5)
  text(sitios[4],y=lim[2],x=18.5)
  text(sitios[5],y=lim[2],x=22.5)
  text(sitios[6],y=lim[2],x=26.5)
  
  
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="sim",pch=19,col=2)
 
  #plot(1, type = "n", axes = FALSE, ann = FALSE)
  #legend("center", "(x,y)",unique(res.shape2$lead_num),
  #pch=c(1,20,1), col=c(2,1,1),title="Lead-Time")
  
  dev.off()
}

# Shape

tipo <- c("Pearsons_correlation",
          "Hit_Skill_Score","ROC_below","ROC_above")

for(i in  1:length(tipo)){
  summary_ind_r(ruta_r, tipo = tipo[i])  
}










############## Gráficos de Intervalos 

### ruta_r = ruta donde se encuentran los archivos retrospectivos
### estacion = estación a la cual se le va a realizar el gráfico
### dep = departamento en el que se encuentra la estación
### lead = nombre del lead time ("MAM")
### lead_num = número del lead time (ejemplo 0 (simultaneo),1 ó 3)
### length_periodo = tamaño del periodo de tiempo 
### a = mes de incio, b= segundo mes, c= tercer mes
### Esta función realiza el gráfico de intervalos para una estación, en el lead time deseado
### con los prónosticos deterministicos retroespectivos
# Para esta función se necesita la información de todas las estaciones completas (Estaciones_C)


retrospectiva<-function(ruta_r, estacion,sitios, dep, lead, a){
  # Extraiga trimestralmente la información de todas las estaciones
  data<-data_trim(Estaciones_C, a)  
  
  if(a == 12){
    year=2012
    # Lea los limites de los pronosticos deterministicos y extraigalos solo para la estación
    lower<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 10)
    lower<-lower[-1:-2,estacion]
    upper<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=15, nrows = 10)
    upper<-upper[-1:-2,estacion]
  } else if(a != 12){
    year=2013
    # Lea los limites de los pronosticos deterministicos y extraigalos solo para la estación
    lower<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 11)
    lower<-lower[-1:-2,estacion]
    upper<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=16, nrows = 11)
    upper<-upper[-1:-2,estacion]
  }
  # solo deje los datos observados para el periodo de interes 
  position<-which(data$years_y>2004&data$years_y<=year)
  obs<-data[position,estacion]
  # lea la información del pronosticos y elimine las coordenadas de la estación
  predictions<-read.table(paste(ruta_r,"Retroactive_Predictions_",a,"_",lead,"_precip_",dep,".txt", sep=""),  skip=2)
  predictions<-predictions[-1:-2,estacion]
  
  ## Cree un data frame para el periodo de retrospectivo y la estación deseada
  estacion_data <- data.frame(year=2005:year, predictions = predictions, lower= lower, upper=upper, obs = obs)
  
  # Haga el gráfico de intervalos 
  retro <-  ggplot(data=estacion_data,aes(x=year,y=predictions))
  retro <-  retro +  geom_point(aes(y=predictions,fill="steelblue4"),shape=21, size=3) 
  retro <-  retro +  geom_errorbar(aes(ymin=lower,ymax=upper), colour="steelblue4")
  retro <-  retro +  geom_point(aes(y=obs, fill="red"), colour="black",shape=21, size=3)
  retro <-  retro +  theme_bw() + labs(x="",y="Precipitación (mm)")
  retro <-  retro +  theme(legend.title = element_text(size = 10.5), 
                           legend.position = "none",
                           legend.key.height=unit(0.5,"cm"),
                           legend.key.width=unit(0.8,"cm"),  axis.text.x = element_text(angle = 90, hjust = 1))
  retro <- retro + scale_fill_manual(values=c("red","blue"), 
                                     labels=c("Observación","Predicción"), name="")
  retro <- retro +   scale_x_continuous(breaks = seq(2005,year,1))
  retro
  # Almacene la imagen 
  tiff(paste(ruta,"/results/",dep,"/retrospectivo_",a,"_",lead,"_", sitios,"_",dep,".tif",sep=""), height=720,width=1280,res=200,
       pointsize=2,compression="lzw")
  print(retro)
  dev.off()
} # retorne el gráfico 



estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima","Turipana", "StaIsabel")
sitios<-c("Lorica","Yopal","Ibagué","LaUnion","Espinal","Cereté")
lead<-c("MAM", "JJA", "SON", "DEF")
a<- c(rep(3,1),rep(6,1),rep(9,1),rep(12,1))
dep<-c("cordoba", "casanare", "tolima", "valle","tolima", "cordoba", "santander")

for(i in 1:7){
  Estaciones_C <- read.delim(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/","dep/precip_",dep[i],".txt",sep=""),skip =3, header=T, sep="")
  for(j in 1:length(a)){
    retrospectiva(ruta_r, estaciones[i], sitios[i], dep[i], lead[j], a[j])
  }
}










####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

#### Calculo de indicadores con las categorías para un departamento

categorias_dep<-function(Estaciones_C,ruta_r,lead,dep,a){
  # Lea los datos de las estaciones
  data<-data_trim(Estaciones_C, a)  
  
  por_cat_est<-matrix(1:2,ncol=2, nrow = (length(data)-1) )
  
  
  if(a == 12){
    ## Si a = 12
    for(j in 1:(length(data)-1)){
      ### Organizarlo puede ser a partir de un condicionante para que 
      # lea la tabla de datos de below (pronosticada)
      probabilities_1<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 10)
      probabilities_1e<-probabilities_1[-1:-2,j]
      # lea la tabla de datos de normal (pronosticada)
      probabilities_2<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=15, nrows = 10)
      probabilities_2e<-probabilities_2[-1:-2, j]
      # lea la tabla de datos de above (pronosticada)
      probabilities_3<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=27, nrows = 10)
      probabilities_3e<-probabilities_3[-1:-2, j]
      # cree un data frame con la información filtada solo para la estación deseada
      categorias<-data.frame(year=2006:2013, below=probabilities_1e, normal=probabilities_2e,above=probabilities_3e)
      
      # Encuentre los quantiles observados 
      
      cat_pron<-0
      
      for (i in 1:length(2006:2013)) { # repita esto por el número de años 
        # cree el vector de categorías pronosticadas
        # tomando como base la categoría más probable 
        if(categorias$below[i]>categorias$normal[i]&categorias$below[i]>categorias$above[i]){
          cat_pron[i] <-1 
        } else if (categorias$above[i]>categorias$normal[i]&categorias$above[i]>categorias$below[i]) {
          cat_pron[i] <-3
        } else cat_pron[i] <-2
      }
      #cree un data frame con todos los datos 
      categorias<-data.frame(categorias, cat_pron=cat_pron)
      # retorne el porcentaje y la trama de datos 
      
      data<-data[data$years_y!=1981&data$years_y!=1982,]
      obs<-data[data$years_y>2005&data$years_y<=2013,j+1]
      
      quantiles<-matrix(1:16, byrow = TRUE, ncol=2)  
      cat_obs<-0
      for(i in 1:length(2006:2013)){
        quantiles[i,]<-quantile(data[data$years_y<(i+2005),j+1],  probs = c(0.33,0.66))
        cbind(data$years_y[data$years_y<(i+2005)],  data[data$years_y<(i+2005),j+1])
        
        if(obs[i]<quantiles[i,1]){
          cat_obs[i]<-1
        } else if(obs[i]>quantiles[i,2]){
          cat_obs[i]<-3
        } else  cat_obs[i]<-2
      }  
      
      observaciones<-data.frame(years = 2006:2013, quantile_33=quantiles[,1], quantile_66=quantiles[,2], obs = obs, cat_obs)
      Cat_estacion<-data.frame(observaciones, categorias)
      
      por_cat_est[j,1]<-sum(Cat_estacion$cat_obs==Cat_estacion$cat_pron)/length(2006:2013) *100
      por_cat_est[j,2]<-sum(Cat_estacion$cat_obs==Cat_estacion$cat_pron)
    }
  } else  if(a != 12){
    ### Si a != 12
    for(j in 1:(length(data)-1)){
      ### Organizarlo puede ser a partir de un condicionante para que 
      # lea la tabla de datos de below (pronosticada)
      probabilities_1<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 11)
      probabilities_1e<-probabilities_1[-1:-2,j]
      # lea la tabla de datos de normal (pronosticada)
      probabilities_2<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=16, nrows = 11)
      probabilities_2e<-probabilities_2[-1:-2,j]
      # lea la tabla de datos de above (pronosticada)
      probabilities_3<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=29, nrows = 11)
      probabilities_3e<-probabilities_3[-1:-2,j]
      # cree un data frame con la información filtada solo para la estación deseada
      categorias<-data.frame(year=2005:2013, below=probabilities_1e, normal=probabilities_2e,above=probabilities_3e)
      
      # Encuentre los quantiles observados 
      
      cat_pron<-0
      
      for (i in 1:length(2005:2013)) { # repita esto por el número de años 
        # cree el vector de categorías pronosticadas
        # tomando como base la categoría más probable 
        if(categorias$below[i]>categorias$normal[i]&categorias$below[i]>categorias$above[i]){
          cat_pron[i] <-1 
        } else if (categorias$above[i]>categorias$normal[i]&categorias$above[i]>categorias$below[i]) {
          cat_pron[i] <-3
        } else cat_pron[i] <-2
      }
      #cree un data frame con todos los datos 
      categorias<-data.frame(categorias, cat_pron=cat_pron)
      # retorne el porcentaje y la trama de datos 
      
      
      
      data<-data[data$years_y!=1981,]
      
      
      obs<-data[data$years_y>2004&data$years_y<=2013,j+1]
      quantiles<-matrix(1:18, byrow = TRUE, ncol=2)  
      cat_obs<-0
      for(i in 1:length(2005:2013)){
        quantiles[i,]<-quantile(data[data$years_y<(i+2004),j+1],  probs = c(0.33,0.66))
        cbind(data$years_y[data$years_y<(i+2004)],  data[data$years_y<(i+2004),j+1])
        
        if(obs[i]<quantiles[i,1]){
          cat_obs[i]<-1
        } else if(obs[i]>quantiles[i,2]){
          cat_obs[i]<-3
        } else  cat_obs[i]<-2
      }  
      
      observaciones<-data.frame(years = 2005:2013, quantile_33=quantiles[,1], quantile_66=quantiles[,2], obs = obs, cat_obs)
      Cat_estacion<-data.frame(observaciones, categorias)
      
      por_cat_est[j,1]<-sum(Cat_estacion$cat_obs==Cat_estacion$cat_pron)/length(2005:2013) *100
      por_cat_est[j,2]<-sum(Cat_estacion$cat_obs==Cat_estacion$cat_pron)
    }
  }  
  dimnames(por_cat_est) <- list(names(data)[-1], c("porc","numero"))
  
  return(por_cat_est)}



##################### Función para calcular el porcentaje de aciertos deterministicos por 
#### Departamento 



# Esta función grafica el porcentaje de aciertos deterministicos (es decir si la observación)
# cae en el intervalo de predición
# ruta_r = ruta donde se encuentran los archivos del retrospectivos
# a = mes de inicio del trimestre
# Estaciones_C = estaciones del departamento que se va a estudiar
# lead = lead time a pronosticar (nombre del archivo)
dep_aciertosD <- function(ruta_r, a, Estaciones_C, lead){
  data<-data_trim(Estaciones_C, a)  # convierta los datos en trimestrales
  porcentaje<-0 # incialice el vector de porcentaje
  RMSE<-0 # incialice el vector para el RMSE 
  
  
  for(i in 1:(length(data)-1)){ # Repita el proceso para todas las estaciones del archivo de datos
    if(a == 12){ # Condicione esto dependiendo del mes de incio del trimestre
      year=2012 # year cambia de acuerdo a la condición de a
      # Lea los limites de los pronosticos deterministicos y extraigalos solo para la estación
      lower<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 10)
      lower<-lower[-1:-2,i]
      upper<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=15, nrows = 10)
      upper<-upper[-1:-2,i]
    } else if(a != 12){
      year=2013
      # Lea los limites de los pronosticos deterministicos y extraigalos solo para la estación
      lower<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 11)
      lower<-lower[-1:-2,i]
      upper<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=16, nrows = 11)
      upper<-upper[-1:-2,i]
    }
    # solo deje los datos observados para el periodo de interes 
    position<-which(data$years_y>2004&data$years_y<=year)
    obs_1<-data[position,]
    obs<-obs_1[,(i+1)]
    # lea la información del pronosticos y elimine las coordenadas de la estación
    predictions<-read.table(paste(ruta_r,"Retroactive_Predictions_",a,"_",lead,"_precip_",dep,".txt", sep=""),  skip=2)
    predictions<-predictions[-1:-2,i]
    
    ## Cree un data frame para el periodo de retrospectivo y la estación deseada
    estacion_data <- data.frame(year=2005:year, predictions = predictions, lower= lower, upper=upper, obs = obs)
    
    # Identifique si se cumple que la observación cae en el intervalo
    # si cumple la condición asignele un valor de 1
    # en caso contrario asignele el valor de cero
    cumple = ifelse(estacion_data$lower<=estacion_data$obs & estacion_data$upper>=estacion_data$obs, 1,0)
    estacion_data<-data.frame(estacion_data,cumple) # guadelo en data frame 
    
    
    porcentaje[i] <- sum(estacion_data$cumple) # Sume cuantas veces se cumple esto
    ## Calcule el RMSE para cada estación y almacenelo. 
    RMSE[i] <- sqrt(sum((estacion_data$predictions-estacion_data$obs)^2)/dim(estacion_data)[1])
  }
  
  aciertos<-list(por_deter=porcentaje, RMSE=RMSE) # Almacene los dos indicadores. 
  #, RMSE=RMSE)
  return(aciertos)}



#dep=c("casanare",    "cordoba",    "tolima",    "valle", "santander")
lead<-c("MAM", "JJA", "SON", "DEF")
a<- c(rep(3,1),rep(6,1),rep(9,1),rep(12,1))
lead_num<-rep(c("sim"),4)


dep="valle"
Estaciones_C <- read.delim(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/","dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
data<-NA
for(i in 1:length(lead)){
  cat_num<-categorias_dep(Estaciones_C,ruta_r,lead[i],dep,a[i])[,2]
  cat_prob<-categorias_dep(Estaciones_C,ruta_r,lead[i],dep,a[i])[,1]
  nombres<-names(cat_num)
  data_i<-data.frame(rep(a[i],length(cat_num)) , rep(lead_num[i],length(cat_num)),nombres, cat_num,cat_prob, row.names = NULL)
  data<-rbind(data, data_i)
}

data<-data[-1,]
names(data)<-c("Trimestre", "Lead", "Estacion", "num_cat_ac", "prob_cat_ac")

### Corra la función con toda la información 
dato<-NA
for(i in 1:length(lead)){
  num<-dep_aciertosD(ruta_r, a[i], Estaciones_C, lead[i])$por_deter
  RMSE<-dep_aciertosD(ruta_r, a[i], Estaciones_C, lead[i])$RMSE
  nombres<-names(Estaciones_C)
  dato_i<-data.frame(rep(a[i],length(num)) , rep(lead_num[i],length(num)),nombres, num,RMSE, row.names = NULL)
  dato<-rbind(dato, dato_i)
}

#Depure la base de datos 
dato<-dato[-1,]
names(dato)<-c("Trimestre", "Lead", "Estacion", "num_det_ac", "RMSE")

getwd()

# Ahora una la base de datos para el porcentaje de aciertos categoricos y el deterministico
total<-data.frame(dato, num_cat_a=data$num_cat_ac,num_cat_ac=data$prob_cat_ac, row.names = NULL)
# Ahora almacene esta trama de datos en un rachivo .csv
write.csv(x = total, file = paste("Tabla_", dep, ".csv", sep=""))






#############################################################################
#############################################################################
#############################################################################
#############################################################################



#### Creación del goodneex index retrospectivo

dep_f=c("casanare","cordoba","tolima","valle", "santander")
lead<-c("MAM", "JJA",	"SON", "DEF")
a<- c(rep(3,1),rep(6,1),rep(9,1),rep(12,1))


good_index=0 # inicialice el vector
GoodnessIndex=NA # inicialice el data frame
for(j in 1:length(dep_f)){
  for(i in 1:length(lead)){
    file= paste(ruta_r,"Retroactive_GoodnessIndex_",a[i],"_",lead[i],"_precip_",dep_f[j],".txt", sep="")
    data=read.table(file,dec=".",skip =2,fill=TRUE,na.strings =-999)
    index<-data[which(data[,1]=="Training")-1,8]
    index<-as.numeric(as.character(index))
    ind<-mean(index)
    
    
    good_index<-cbind.data.frame(dep_f[j], a[i] ,lead[i],ind)
    GoodnessIndex<-rbind(GoodnessIndex,good_index)
  }
}

GoodnessIndex<-GoodnessIndex[-1,]
# Quite los nombres de las filas
rownames(GoodnessIndex)=NULL
# Cambie los nombres de las columnas
names(GoodnessIndex)=c("dep","a", "lead", "GoodnessIndex")


setwd(paste(ruta, "results/",sep="" ))
### Guarde todos los Goodness Index en un archivo .csv
write.csv(x = GoodnessIndex, file = "GoodnessIndex_ret_Op.csv")






#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################




data<-read.table("clipboard",header = T)


data$a[data$a==12]=0 # Cambiarle el número para que diciembre aparezca primero
labels_d<-as_labeller(c("casanare"="Casanare","cordoba"="Cordoba","tolima"="Tolima", "valle"="Valle del Cauca", "santander"="Santander"))

ggplot(data, aes(x=a, y=GI, color=Predictor)) + 
  geom_line(aes(linetype=Predictor), size=1) + geom_point(aes(shape=Predictor)) +
  scale_x_continuous(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON"))+
  facet_wrap(~dep, nrow=1, labeller = labeller(dep = labels_d)) +
  labs(x="Trimestre", y=paste("Goodness Index", sep=""))+ theme_bw() +
  geom_hline(yintercept = c(0, 0.3), colour = "black", linetype = "dotted")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))



setwd("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/results_graphs_Op/")
getwd()

ggsave("models.png",width =8 ,height =3.5,dpi=200)







data<-read.table("clipboard",header = T)

data$a[data$a==12]=0 # Cambiarle el número para que diciembre aparezca primero
labels_d<-as_labeller(c("casanare"="Casanare","cordoba"="Cordoba","tolima"="Tolima", "valle"="Valle del Cauca", "santander"="Santander"))


regT<-data[data$Region=="Teo",]


T<-ggplot(regT, aes(x=a, y=GI, color=Predictor)) + ylim(min(data$GI), max(data$GI)) +
  geom_line(aes(linetype=Predictor), size=1) + geom_point(aes(shape=Predictor)) +
  scale_x_continuous(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON"))+
  facet_wrap(~dep, nrow=1, labeller = labeller(dep = labels_d)) +
  labs(x="Trimestre", y=paste("Goodness Index", sep=""))+ theme_bw() +
  geom_hline(yintercept = c(0, 0.3), colour = "black", linetype = "dotted")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5), legend.position = "bottom") +
  ggtitle("Theoretical region", subtitle = NULL)

regOp<-data[data$Region=="Op",]


Op<-ggplot(regOp, aes(x=a, y=GI, color=Predictor)) + ylim(min(data$GI), max(data$GI)) +
  geom_line(aes(linetype=Predictor), size=1) + geom_point(aes(shape=Predictor)) +
  scale_x_continuous(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON"))+
  facet_wrap(~dep, nrow=1, labeller = labeller(dep = labels_d)) +
  labs(x="Trimestre", y=paste("Goodness Index", sep=""))+ theme_bw() +
  geom_hline(yintercept = c(0, 0.3), colour = "black", linetype = "dotted")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5), legend.position = "bottom")+
  ggtitle("Optimized region", subtitle = NULL)




grid.arrange(T,Op)



setwd("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1")
tiff(paste(getwd(),"/cc.tif",sep=""), height=350,width=1000,res=80,
     compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
print(grid.arrange(T,Op, ncol=2))
dev.off()


##########################################################################



#region<-"results_graphs_C" # "results_graphs_C"    "results_graphs_Op"
#prec<-"vertical_vel_250"    # prec= c("U_wind_250","U_wind_850", "rhum_700", "vertical_vel_250")
### Directorio de trabajo
setwd(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/",sep=""))
getwd()


data<-read.table("clipboard",header = T)


data$a[data$a==12]=0 # Cambiarle el número para que diciembre aparezca primero

labels<-as_labeller(c("0"="DEF","3"="MAM","6"="JJA", "9"="SON"))
labels_d<-as_labeller(c("casanare"="Casanare","cordoba"="Cordoba","tolima"="Tolima", "valle"="Valle del Cauca", "santander"="Santander"))
#x11()
ggplot(data,aes(x=cv,y=rt,shape=Predictor,color=Region,size=0.2))+
  geom_point() +
  facet_grid(a~dep, labeller = labeller(a = labels, dep=labels_d))+theme_bw()+
  scale_size(guide=F)+scale_shape(name="Lead Time")+ scale_color_discrete(name="Region", labels=c("Optimized", "Theoretical")) + 
  ylab("Goodness Index - Retroactive")+xlab("Goodness Index - Cross Validated")+
  theme(strip.text.x = element_text(size = 11)) + 
  geom_vline(xintercept = 0.3, colour = "black", linetype = "dotted") + geom_hline(yintercept = 0.3, colour = "black", linetype = "dotted")


ggsave("best_model.png",width =12 ,height =6,dpi=200 )











#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################


### Directorio de trabajo
setwd(paste("C:/Users/AESQUIVEL/Google Drive/new_predictor/Exp1/",sep=""))
getwd()




###### Boxplot


prueba<-read.table("clipboard",header = T)

prueba$a[prueba$a==12]=0 # Cambiarle el número para que diciembre aparezca primero



labels_d<-as_labeller(c("casanare"="Casanare","cordoba"="Cordoba","tolima"="Tolima", "valle"="Valle del Cauca", "santander"="Santander"))
labels_R<-as_labeller(c("Op"="Optimized", "Teo"= "Theoretical"))


library("RColorBrewer")



ggplot(prueba, aes(x=as.factor(a) , y=GI)) + 
  geom_boxplot(aes(fill = Predictor)) +  scale_x_discrete(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON")) +  
  scale_fill_brewer(palette="Set3") +
  facet_wrap(~dep, ncol=5, labeller = labeller(dep=labels_d))+
  theme_bw() + ylab("Goodness Index") +xlab("") +
  geom_hline(yintercept = c(0,0.3), colour = "black", linetype = "dotted")


ggsave("Box_D.png",width =11 ,height =4,dpi=100 )



ggplot(prueba, aes(x=as.factor(a) , y=GI)) + 
  geom_boxplot(aes(fill = Predictor)) +  scale_x_discrete(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON")) +  
  scale_fill_brewer(palette="Set3") +
  theme_bw() + ylab("Goodness Index") +xlab("") +
  geom_hline(yintercept = c(0,0.3), colour = "black", linetype = "dotted")


ggsave("Box_T.png",width =8 ,height =4,dpi=100 )









#####################################################################
prueba<-read.table("clipboard",header = T)

### Graphs densidades

labels_Val<-as_labeller(c("Cv"="Cv", "retro"="Rv"))
labels_R<-as_labeller(c("Op"="Optimized", "Teo"= "Theoretical"))

ggplot(prueba, aes(GI, fill = Predictor))+
  geom_density(alpha = 0.4)+ facet_grid(Val~Region, labeller = labeller(Val=labels_Val, Region=labels_R))+  theme_bw() + xlab("Goodness Index") + 
  geom_vline(xintercept = c(0,0.3), color="gray30", linetype="dashed", size=1)


ggsave("densityR.png",width =8 ,height =5,dpi=200 )




ggplot(prueba, aes(GI, fill = Predictor))+
  geom_density(alpha = 0.4)+ facet_wrap(~Val, labeller = labeller(Val=labels_Val))+  theme_bw() + xlab("Goodness Index") + 
  geom_vline(xintercept = c(0,0.3), color="gray30", linetype="dashed", size=1)

ggsave("density.png",width =6 ,height =3,dpi=200 )





########################################################################


##### Head Map



data<-read.table("clipboard",header = T)
col <- colorRampPalette(c("snow","#fee08b","#e6f598","#abdda4","#ddf1da","#d53e4f","#f46d43","#fdae61"))
labels<-as_labeller(c("0"="DEF","3"="MAM","6"="JJA", "9"="SON"))



data$a[data$a==12]=0 # Cambiarle el número para que diciembre aparezca primero



p <- ggplot(data, aes(Region, Predictor)) + geom_tile(aes(fill = GI),colour = "white") + 
  facet_grid(a~dep, labeller = labeller(a = labels, dep=labels_d)) + theme_bw() #+ theme(legend.position="top")
p <- p  + geom_text(aes(label = round(GI, 2)), size=3) + xlab("Región") + ylab("Predictor")
p <-p +  scale_fill_gradientn(colours = col(10)) 



tiff(paste(getwd(),"/head_map_retro.tif",sep=""), height=500,width=800,res=80,
     compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
print(p)
dev.off()





#########################################################
#########################################################
################ Box Graphs   ###########################
#########################################################
#########################################################

data<-read.table("clipboard",header = T)
data$a[data$a==12]=0 # Cambiarle el número para que diciembre aparezca primero




cfsv2<-ggplot(data, aes(x=Predictor , y=GI)) + 
  geom_boxplot(aes(fill = as.factor(lead))) +scale_fill_brewer(palette="Set2") + 
  theme_bw() + ylab("Goodness Index") +xlab("") + labs(fill="Lead Time")+
  geom_hline(yintercept = c(0,0.3), colour = "black", linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


datos2<-read.table("clipboard",header = T)
datos2$a[datos2$a==12]=0 # Cambiarle el número para que diciembre aparezca primero


re<-ggplot(data, aes(x=Predictor , y=GI)) + 
  geom_boxplot(fill="gray") +scale_fill_brewer(palette="Set2") + 
  theme_bw() + ylab("Goodness Index") +xlab("") + 
  geom_hline(yintercept = c(0,0.3), colour = "black", linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


grid.arrange(re,cfsv2, layout_matrix=matrix(c(1,2,2),ncol=3))

