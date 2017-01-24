### Paquetes Necesarios
library(raster) 
library(ggplot2)
library(grid)
library(rasterVis)
library(maptools)
library(rgeos)
library(gridExtra)
library(cowplot)

### Directorio de trabajo
setwd("C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/Salidas_tropico")
getwd()



### Transformar un archivo .tsv(CPT) en raster 

# Se crea una función que permita crear un raster a partir de los valores 
# de la tabla depurados, (esta función esta programada para los datos de la TSM observada)
transform_raster=function(x){ 
  # Primero se crea un raster teniendo encuenta la resolución espacial de la tabla .tsv  
  mapa_base=raster(nrows=88, ncols=180,xmn=0,xmx=358, ymn=-88,ymx=88) # Dimensiones del raster
  val=c(as.matrix(t(x),ncol=1,byrow = T)) 
  val=as.numeric(val)
  val[val==-999.000]=NA
  values(mapa_base)=val
  return(mapa_base)
}

# Con esta función se depura y rasteriza la tabla .tsv
rasterize=function(dates) { 
  
  if(require(raster)==FALSE){install.packages("raster")}
  library("raster")
  pos_years=!is.na(dates[1,]) # Muestra en que lugares de la fila 1 hay información
  year_month=dates[1,][pos_years] # Muestra la información d ela fila
  year=substr(year_month[-1],1,4) # Substrae el año de las fehcas
  ## Muestra las posiciones de las filas en la tabla que no contienen información relevante para el raster
  total_row_delete=c(-1,-3,-(which(dates[,1]=="88.0")-2),-which(dates[,1]=="-88.0"),-which(dates[,1]==""))
  dates=dates[total_row_delete,-1] # Elimina la información no relevante
  
  list_dates=split(dates,sort(rep(year,88))) # Se divide la tabla de datos por año
  all_raster=lapply(list_dates,transform_raster) ## Transforma las tablas de datos en rasters
  layers=stack(all_raster) # Crea un stack
  layers_crop=crop(layers,extent(0, 358, -30, 30)) ## Realiza el corte para que solo tome el tropico
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






## Ruta principal donde se encuentran las carpetas con los archivos  
ruta="C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/Salidas_tropico"

ruta1="C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/"
### Lectura del shp
colombia=shapefile(paste(ruta1,"/colombia/colombia_depts.shp",sep=""))





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
  Map_x=gplot(correl_map) + geom_tile(aes(fill = value)) + coord_equal() + 
    scale_fill_gradient2(low="#2166AC",mid = "white", high="#B2182B",name = " ",  limits=c(-1,1)) + 
    labs(title=" ",x="Long",y="Lat")  + theme(legend.key.height=unit(0.5,"cm"),legend.key.width=unit(2,"cm"),
          legend.text=element_text(size=10),
          panel.background=element_rect(fill="white",colour="black"),
          axis.text=element_text(colour="black",size=10),
          axis.title=element_text(colour="black",size=10,face="bold"),
          legend.position = "bottom", 
          legend.title = element_text(size = 10.5))

  
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
  tiff(paste(ruta,"/santander/cca_maps_",names_file,".tif",sep=""), height=300,width=1600,res=100,
       compression="lzw") # height=1280, width=2048, pointsize=2, res=200,
  grid.arrange(Map_x,modos,p,ncol=5, layout_matrix = rbind(c(1,1,2,3,3)))
  #grid.arrange(Map_x,p,modos, layout_matrix = rbind(c(1,1),c(2,3)))
  dev.off()
}



ruta_c<-paste(ruta, "/Cross_validated/",sep="")

dep= "santander" # variar el departamento



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


length_periodo=c(rep(32,9), rep(31,3)) # Ancho del periodo de estudio para cada trimestre
# Lead times (nombres)
lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
# Timestre
a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))

## Para cada estación lea las estaciones de interes. 


# Lectura d eas estaciones para cada departamento.
Estaciones_C <- read.delim(paste(ruta1,"dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")

# Lea los archivos y las estaciones necesarias para todas las corridas del departamento. 
for(i in 1:12){
  xserie <- read.csv(paste(ruta_c, "X_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
  yserie <- read.csv(paste(ruta_c,"Y_CCA_Map_Series_",a[i],"_",lead[i],"_precip_",dep,".txt",sep=""),skip =2, header=T, sep="")
  names_file <- paste(a[i],"_",lead[i],"_", dep,sep="")
  
  
  SST<-read.table(paste(ruta1,"ERSST_CPT/",lead[i],".tsv",sep=""),sep="\t",dec=".",skip =3,fill=TRUE,na.strings =-999)
  ## Conversión a raster
  SST=rasterize(SST)
  var_ocanoAt <-SST[[1:length_periodo[i]]]
  
  cca_maps(var_ocanoAt, yserie, Estaciones_C, xserie, names_file, ruta,  a[i], xmin, xmax, ymin, ymax, estaciones_in)
}


















####### Goodnex Index


## Esta función devuelve las gráficas del goodness index, 
## ruta_c ruta donde se encuentran los archivos
## dep_f son los departamentos que se van a gráficar
GoodnessIndex <- function(ruta_c,dep_f){
  #### Lead times para cada trimestre
  lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
  ### Trimestres
  a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))
  
  good_index=0 # inicialice el vector
  GoodnessIndex=NA # inicialice el data frame
  for(j in 1:length(dep_f)) # realice esto para todos los departamentos
    for(i in 1:12){ # barra para todos los periodos con sus respectivos lead time
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
  GoodnessIndex <- cbind(GoodnessIndex,lead_time=rep(rep(c(0,1,3),4),length(dep_f))  )
  
  ### Guarde todos los Goodness Index en un archivo .csv
  write.csv(x = GoodnessIndex, file = "GoodnessIndex.csv",sep = ",")
  
  
  GoodnessIndex$a[GoodnessIndex$a==12]=0 # Cambiarle el número para que diciembre aparezca primero
  # Realice el gráfico de cajas para todos los departamentos.
  for(i in 1:length(dep_f)){
    dep_inf<-  GoodnessIndex[GoodnessIndex$dep==dep_f[i],]
    to_string  <- as_labeller(c(`0`="DEF",`3` = "MAM", `6` = "JJA", `9` = "SON"))
    graph_dep  <- ggplot(dep_inf, aes(x = factor(lead_time), y = GoodnessIndex)) 
    graph_dep  <- graph_dep  + geom_bar(stat = 'identity', fill="gray60") + ylim(-0.02,0.5)
    graph_dep  <- graph_dep  + facet_wrap(~a, nrow = 1, labeller = to_string )
    graph_dep  <- graph_dep  + scale_x_discrete(breaks=c(0, 1, 3),
                                                labels=c("Sim", "0", "3")) +  labs(x="Lead Time", y=paste("Goodness Index", sep=""))
    graph_dep  <-graph_dep  + theme_bw() + theme(legend.position = "none") 
    
    
    tiff(paste(ruta,"/santander/GoodnessIndex_dep_",dep_f[i],".tif",sep=""), height=720,width=1280,res=200,
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
  graph_line  <- graph_line + geom_line(aes(linetype=Departamento), size=1) + ylim(-0.01,0.5)
  graph_line  <- graph_line + geom_point(aes(shape=Departamento), size=2)
  graph_line  <- graph_line + theme_bw()  + labs(x="", y="Goodness Index") 
  graph_line  <- graph_line +  scale_x_continuous(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON"))
  graph_line  <- graph_line + theme(legend.title = element_text(size = 10.5),
                                    legend.key.height=unit(0.5,"cm"),
                                    legend.key.width=unit(0.8,"cm"))
  

# Guarde el graph
  tiff(paste(ruta,"/santander/GoodnessIndex_line.tif",sep=""), height=720,width=1280,res=200,
       pointsize=2,compression="lzw")
  print(graph_line)
  dev.off()
  
return(GoodnessIndex)}



## Corrida para todos los archivos
dep_f<-c("casanare",    "cordoba",    "tolima",    "valle", "santander")
table<-GoodnessIndex(ruta_c = ruta_c, dep_f = dep_f)

max_G<-aggregate(table$GoodnessIndex,list(table$dep, table$a), FUN = "max")


### Grafico de Linea puede para los maximos
### modificando la función 
names(max_G)[1]="Departamento"
levels(max_G$Departamento)<-c("Casanare", "Cordoba", "Tolima",  "Valle", "Santander")
graph_line  <- ggplot(max_G, aes(x = Group.2 , y = x, color=Departamento))
graph_line  <- graph_line + geom_line(aes(linetype=Departamento), size=1) + ylim(-0.01,0.5)
graph_line  <- graph_line + geom_point(aes(shape=Departamento), size=2)
graph_line  <- graph_line + theme_bw()  + labs(x="", y="Goodness Index") 
graph_line  <- graph_line +  scale_x_continuous(breaks = c(0,3,6,9), labels = c("DEF","MAM", "JJA", "SON"))
graph_line  <- graph_line + theme(legend.title = element_text(size = 10.5),
                                  legend.key.height=unit(0.5,"cm"),
                                  legend.key.width=unit(0.8,"cm"))


# Guarde el graph
tiff(paste(ruta,"/santander/Max_line.tif",sep=""), height=720,width=1280,res=200,
     pointsize=2,compression="lzw")
print(graph_line)
dev.off()









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
  for(i in 1:4){ # itere el número de trimestres y liste los archivos
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
  tiff(paste(ruta,"/santander/",tipo,"_sim",".tif",sep=""), height=600,width=1280,res=200,
       pointsize=2,compression="lzw") # height=720,
  print(box)
  dev.off()
  return(datos2)} # la función devuelve la trama de datos en caso de ser necesaria


ruta_c<-paste(ruta, "/Cross_validated/",sep="")

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
  lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
  lead_num<-rep(c("sim",0,3),4) # número de los archivos
  a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3)) # trimestres de estudio
  
  
  
  # nombre de los sitios d eestudios (como apareceran en el gráfico)
  estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima"
                ,"Turipana", "StaIsabel")
  

  datos<-NA # Creación de una trama de datos
  
  # lea los datos y apilelos en una tabla de datos
  for(j in 1:12){
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
  res.shape2 <- cbind(res.shape2,f1=rep(1:28,each=3)) # 28 equivale a # de trimestre * sitios
  
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

 
  tiff(paste(ruta,"/santander/a_",tipo,".tif",sep=""), height=800,width=1800,res=150,
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
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="sim",pch=1,col=2)
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="0",pch=20)
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="3", col=1)
  

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



ruta_r="C:/Users/AESQUIVEL/Google Drive/Experimento_1/Salidas_corrida_DEF/salidas/Salidas_tropico/retroactive/"


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
  for(i in 1:4){ # itere el número de trimestres y liste los archivos
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
  tiff(paste(ruta,"/santander/retro_",tipo,"_sim",".tif",sep=""), height=600,width=1280,res=200,
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
  
  lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
  lead_num<-rep(c("sim",0,3),4)
  a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))
  # nombre de los sitios d eestudios (como apareceran en el gráfico)
  estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima"
                ,"Turipana", "StaIsabel")
  
  
  datos<-NA # Creación de una trama de datos
  
  for(j in 1:12){
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
  
  res.shape2 <- cbind(res.shape2,f1=rep(1:28,each=3))
  
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
  
  
  tiff(paste(ruta,"/santander/retroa_",tipo,".tif",sep=""), height=800,width=1800,res=150,
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
  
  
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="sim",pch=1,col=2)
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="0",pch=20)
  points(ROC_B~f1,data=res.shape2,subset= lead_num=="3", col=1)
  
  
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



dep=c("casanare",    "cordoba",    "tolima",    "valle")
lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))
lead_num<-rep(c("sim",0,3),4)


  dep="santander"
  Estaciones_C <- read.delim(paste(ruta1,"dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
  
  data<-NA
  for(i in 1:12){
    cat_num<-categorias_dep(Estaciones_C,ruta_r,lead[i],dep[j],a[i])[,2]
    cat_prob<-categorias_dep(Estaciones_C,ruta_r,lead[i],dep[j],a[i])[,1]
    nombres<-names(cat_num)
    data_i<-data.frame(rep(a[i],length(cat_num)) , rep(lead_num[i],length(cat_num)),nombres, cat_num,cat_prob, row.names = NULL)
    data<-rbind(data, data_i)
 }


  data<-data[-1,]
  names(data)<-c("Trimestre", "Lead", "Estacion", "num_cat_ac", "prob_cat_ac")
  
  
  

#if(a ==12){
#  cat_prob_p<-(sum(cat_prob[,2])/(length(2006:2012)*length(Estaciones_C)))*100  
#} else  cat_prob_p<-(sum(cat_prob[,2])/(length(2006:2013)*length(Estaciones_C)))*100 
#cat_prob_p










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
  
  #length(data)
  
  
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




### Corra la función con toda la información 


#dep=c("casanare",    "cordoba",    "tolima",    "valle")
lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))
lead_num<-rep(c("sim",0,3),4)


dep="santander"
Estaciones_C <- read.delim(paste(ruta1,"/dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")


dato<-NA
for(i in 1:12){
  num<-dep_aciertosD(ruta_r, a[i], Estaciones_C, lead[i])$por_deter
  RMSE<-dep_aciertosD(ruta_r, a[i], Estaciones_C, lead[i])$RMSE
  nombres<-names(Estaciones_C)
  dato_i<-data.frame(rep(a[i],length(num)) , rep(lead_num[i],length(num)),nombres, num,RMSE, row.names = NULL)
  dato<-rbind(dato, dato_i)
}

#Depure la base de datos 
dato<-dato[-1,]
names(dato)<-c("Trimestre", "Lead", "Estacion", "num_det_ac", "RMSE")


# Ahora una la base de datos para el porcentaje de aciertos categoricos y el deterministico
total<-data.frame(dato, num_cat_a=data$num_cat_ac,num_cat_ac=data$prob_cat_ac, row.names = NULL)
# Ahora almacene esta trama de datos en un rachivo .csv
write.csv(x = total, file = paste("Tabla_", dep, ".csv", sep=""))








#### Calculo de indicadores con las categorías para una estación en especifico



# Estaciones_C = archivo que contiene todas las estaciones de x departamento
# ruta_r = ruta donde se encuentran los archivos retrospectivos
# lead = lead time (ejemplo "MAM")
# dep = departamento en el cual se encuentran las estaciones
# estacion = estación que se va pronosticar
# a= mes de inicio del periodo pronosticado
## La función categorias entrega el porcentaje de veces que la 
## categoria observada concuerda con la pronosticada y la trama de datos
#categorias<-function(Estaciones_C,ruta_r,lead,dep, estacion,a){
  # Lea los datos de las estaciones
#  data<-data_trim(Estaciones_C, a)  
  
#  if(a == 12){
    ## Si a = 12
    
    ### Organizarlo puede ser a partir de un condicionante para que 
    # lea la tabla de datos de below (pronosticada)
#    probabilities_1<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 9)
#    probabilities_1e<-probabilities_1[-1:-2,estacion]
    # lea la tabla de datos de normal (pronosticada)
#    probabilities_2<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=14, nrows = 9)
#    probabilities_2e<-probabilities_2[-1:-2, estacion]
    # lea la tabla de datos de above (pronosticada)
#    probabilities_3<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=25, nrows = 9)
#    probabilities_3e<-probabilities_3[-1:-2, estacion]
    # cree un data frame con la información filtada solo para la estación deseada
#    categorias<-data.frame(year=2006:2012, below=probabilities_1e, normal=probabilities_2e,above=probabilities_3e)
    
    # Encuentre los quantiles observados 
    
#    cat_pron<-0
    
#    for (i in 1:length(2006:2012)) { # repita esto por el número de años 
      # cree el vector de categorías pronosticadas
      # tomando como base la categoría más probable 
#      if(categorias$below[i]>categorias$normal[i]&categorias$below[i]>categorias$above[i]){
#        cat_pron[i] <-1 
#      } else if (categorias$above[i]>categorias$normal[i]&categorias$above[i]>categorias$below[i]) {
#        cat_pron[i] <-3
#      } else cat_pron[i] <-2
#    }
    #cree un data frame con todos los datos 
#    categorias<-data.frame(categorias, cat_pron=cat_pron)
    # retorne el porcentaje y la trama de datos 
    
    
    
#    data<-data[data$years_y!=1981&data$years_y!=1982,]
    
    
    
#    obs<-data[data$years_y>2005&data$years_y<=2012,estacion]
    
#    quantiles<-matrix(1:14, byrow = TRUE, ncol=2)  
#    cat_obs<-0
#    for(i in 1:length(2006:2012)){
#    quantiles[i,]<-quantile(data[data$years_y<(i+2005),estacion],  probs = c(0.33,0.66))
#    cbind(data$years_y[data$years_y<(i+2005)],  data[data$years_y<(i+2005),estacion])
      
#      if(obs[i]<quantiles[i,1]){
#        cat_obs[i]<-1
#      } else if(obs[i]>quantiles[i,2]){
#        cat_obs[i]<-3
#      } else  cat_obs[i]<-2
#      print(i)
#      print(max(data$years_y[data$years_y<(i+2005)]))
#    }  
    
#    observaciones<-data.frame(years = 2006:2012, quantile_33=quantiles[,1], quantile_66=quantiles[,2], obs = obs, cat_obs)
    
    
#    Cat_estacion<-data.frame(observaciones, categorias)
    
#    por_cat_est<-sum(Cat_estacion$cat_obs==Cat_estacion$cat_pron)/length(2006:2012) *100
    
#    aciertos<-list(Cat_estacion,por_cat_est) 
#    names(aciertos)<-c("data", "por_cat_est")
#  } else  if(a != 12){
    ### Si a != 12
    
    ### Organizarlo puede ser a partir de un condicionante para que 
    # lea la tabla de datos de below (pronosticada)
#    probabilities_1<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 10)
#    probabilities_1e<-probabilities_1[-1:-2,estacion ]
    # lea la tabla de datos de normal (pronosticada)
#    probabilities_2<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=15, nrows = 10)
#    probabilities_2e<-probabilities_2[-1:-2, estacion]
    # lea la tabla de datos de above (pronosticada)
#    probabilities_3<-read.table(paste(ruta_r,"Retroactive_Forecast_Probabilities_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=27, nrows = 10)
#    probabilities_3e<-probabilities_3[-1:-2, estacion]
    # cree un data frame con la información filtada solo para la estación deseada
#    categorias<-data.frame(year=2006:2013, below=probabilities_1e, normal=probabilities_2e,above=probabilities_3e)
    
    # Encuentre los quantiles observados 
    
#    cat_pron<-0
    
#    for (i in 1:length(2006:2013)) { # repita esto por el número de años 
      # cree el vector de categorías pronosticadas
      # tomando como base la categoría más probable 
#      if(categorias$below[i]>categorias$normal[i]&categorias$below[i]>categorias$above[i]){
#        cat_pron[i] <-1 
#      } else if (categorias$above[i]>categorias$normal[i]&categorias$above[i]>categorias$below[i]) {
#        cat_pron[i] <-3
#      } else cat_pron[i] <-2
#    }
    #cree un data frame con todos los datos 
#    categorias<-data.frame(categorias, cat_pron=cat_pron)
    # retorne el porcentaje y la trama de datos 
    
    
    
#    data<-data[data$years_y!=1981,]
    
    
#    obs<-data[data$years_y>2005&data$years_y<=2013,estacion]
#    quantiles<-matrix(1:16, byrow = TRUE, ncol=2)  
#    cat_obs<-0
#    for(i in 1:length(2006:2013)){
#    quantiles[i,]<-quantile(data[data$years_y<(i+2005),estacion],  probs = c(0.33,0.66))
#    cbind(data$years_y[data$years_y<(i+2005)],  data[data$years_y<(i+2005),estacion])
      
#      if(obs[i]<quantiles[i,1]){
#        cat_obs[i]<-1
#      } else if(obs[i]>quantiles[i,2]){
#        cat_obs[i]<-3
#      } else  cat_obs[i]<-2
#      print(i)
#      print(max(data$years_y[data$years_y<(i+2005)]))
#    }  
    
#    observaciones<-data.frame(years = 2006:2013, quantile_33=quantiles[,1], quantile_66=quantiles[,2], obs = obs, cat_obs)
    
#    Cat_estacion<-data.frame(observaciones, categorias)
    
#    por_cat_est<-sum(Cat_estacion$cat_obs==Cat_estacion$cat_pron)/length(2006:2013) *100
    
#    aciertos<-list(Cat_estacion,por_cat_est) 
#    names(aciertos)<-c("data", "por_cat_est")
#  }  
  
 return(aciertos)#}


#dep="santander"
#a=12
#Estaciones_C <- read.delim(paste(ruta1,"dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
#estacion="StaIsabel"
#lead="DEF"

#categorias(Estaciones_C,ruta_r,lead,dep, estacion,a) 
#categorias(Estaciones_C,ruta_r,"SON",dep, estacion,9) 








































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
  tiff(paste(ruta,"/santander/retrospectivo_",a,"_",lead,"_", sitios,"_",dep,".tif",sep=""), height=720,width=1280,res=200,
       pointsize=2,compression="lzw")
  print(retro)
  dev.off()
} # retorne el gráfico 






estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima","Turipana")
sitios<-c("Lorica","Yopal","Ibagué","LaUnion","Espinal","Cereté")
lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))
dep<-c("cordoba", "casanare", "tolima", "valle","tolima", "cordoba")

for(i in 1:6){
  Estaciones_C <- read.delim(paste(ruta,"dep/precip_",dep[i],".txt",sep=""),skip =3, header=T, sep="")
  
  for(j in 1:12){
    retrospectiva(ruta_r, estaciones[i], sitios[i], dep[i], lead[j], a[j])
   # retrospectiva(ruta_r, "StaIsabel", "Villanueva", "santander", lead[j], a[j])
  }
}








############## Calculo del porcentaje de aciertos determitisticos y RMSE por estación 


ruta_r="C:/Users/AESQUIVEL/Desktop/Salidas_corrida_DEF/salidas/retroactive/"
dep="valle"
a=9
Estaciones_C <- read.delim(paste(ruta,"dep/precip_",dep,".txt",sep=""),skip =3, header=T, sep="")
lead="Aug"
estacion<-"CentAdmoLaUnion"


RMSE_aciertosD <- function(ruta_r, a, Estaciones_C, lead, estacion){
  data<-data_trim(Estaciones_C, a) 
  if(a == 12){
    year=2012
    # Lea los limites de los pronosticos deterministicos y extraigalos solo para la estación
    lower<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 9)
    lower<-lower[-1:-2,estacion]
    upper<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=14, nrows = 9)
    upper<-upper[-1:-2,estacion]
  } else if(a != 12){
    year=2013
    # Lea los limites de los pronosticos deterministicos y extraigalos solo para la estación
    lower<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=3, nrows = 10)
    lower<-lower[-1:-2,estacion]
    upper<-read.table(paste(ruta_r,"Retroactive_Prediction_Limits_",a,"_",lead,"_precip_",dep,".txt", sep=""),  sep="",skip=15, nrows = 10)
    upper<-upper[-1:-2,estacion]
  }
  # solo deje los datos observados para el periodo de interes 
  position<-which(data$years_y>2005&data$years_y<=year)
  obs_1<-data[position,]
  obs<-obs_1[,estacion]
  # lea la información del pronosticos y elimine las coordenadas de la estación
  predictions<-read.table(paste(ruta_r,"Retroactive_Predictions_",a,"_",lead,"_precip_",dep,".txt", sep=""),  skip=2)
  predictions<-predictions[-1:-2,estacion]
  
  ## Cree un data frame para el periodo de retrospectivo y la estación deseada
  estacion_data <- data.frame(year=2006:year, predictions = predictions, lower= lower, upper=upper, obs = obs)
  
  
  porcentaje<-0
  
  cumple = ifelse(estacion_data$lower<=estacion_data$obs & estacion_data$upper>=estacion_data$obs, 1,0)
  estacion_data<-data.frame(estacion_data,cumple)
  porcentaje <- sum(estacion_data$cumple)/dim(estacion_data)[1]*100
  
  
  
  RMSE <- sqrt(sum((estacion_data$predictions-estacion_data$obs)^2)/dim(estacion_data)[1])
  
  aciertos<-list(datos=estacion_data, por_deter=porcentaje, RMSE=RMSE)
return(aciertos)}




estaciones<-c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima","Turipana")
sitios<-c("Lorica","Yopal","Ibagué","LaUnion","Espinal","Cereté")
lead<-c("MAM",	"Feb", "Nov", "JJA",	"May",	"Feb", "SON", "Aug",	"May", "DEF",	"Nov",	"Aug")
a<- c(rep(3,3),rep(6,3),rep(9,3),rep(12,3))
lead_num<- rep(c("sim",0,3),4)
dep<-c("cordoba", "casanare", "tolima", "valle","tolima", "cordoba")



RMSE<-0

for(j in 1:6){
  Estaciones_C <- read.delim(paste(ruta,"dep/precip_",dep[j],".txt",sep=""),skip =3, header=T, sep="")
  for(i in 1:12){
    RMSE[i]<-RMSE_aciertosD(ruta_r, a[i], Estaciones_C, lead[i],  estaciones[j])$RMSE
  }
  datos_p=data.frame(a, lead_num, rep(estaciones[j],12), RMSE)
  datos=rbind(datos, datos_p)
}




levels(datos$Trimestre)<-c("TR2", "TR3", "TR4", "TR1")



































































##Script para correr climatologías
#Ruta donde se encuentran los archivos
library(reshape2)
library(ggplot2)

ruta = "C:/Users/AESQUIVEL/Desktop/Salidas_corrida_DEF/salidas/datos_puros/"
stations = list.files(ruta)
dptos = c("Casanare", "Córdoba", "Tolima", "Valle")
month.esp = c("Ene", "Feb", "Mar", "Abr","May", "Jun", "Jul", "Ago", "Sep","Oct","Nov","Dec")

Datos <- lapply(paste(ruta,"/",stations,sep=""),function(x){read.table(x,header=T,sep=",")})
x = seq(as.Date("1981/1/1"), as.Date("2013/12/31"), "month") #Definir periodo que se desea analizar
month = format(x,"%m")

data_clim = lapply(Datos, function(x) aggregate(x[-1],list(month),mean))
data_clim_dpto = lapply(data_clim, function(x) apply(x[-1], 1, mean))
data_clim_dpto_all = as.data.frame(do.call("cbind",data_clim_dpto))
data_clim_dpto_all = cbind("month" = month.esp, data_clim_dpto_all)
names(data_clim_dpto_all) = c("month",dptos)

clim = melt(data_clim_dpto_all)
clim$month = factor(clim$month,levels= month.esp)

ggplot(clim,aes(x=month,y=value))+geom_bar(stat = "identity")+facet_wrap(~variable,scales = "free_x")+
  ylab("Precipitación (mm)")+xlab(" ")+
  theme_bw()+theme(strip.text = element_text(size=10))

ggsave("clim.tiff",  height =5, width =10,dpi=200 )

station_only = read.table(file = "retrospectivo/Stations_r/station.txt",header = T, sep="\t")
station_only_clim = aggregate(station_only[-1],list(month),mean)
station_only_clim$Group.1 = month.esp

station_clim = melt(station_only_clim)
station_clim$Group.1 = factor(station_clim$Group.1,levels= month.esp)
station_clim$dpto = c(rep(c("Casanare","Valle"),each=12), rep(c("Tolima","Córdoba"),each=24))

ggplot(station_clim,aes(x=Group.1,y=value,fill=dpto))+geom_bar(stat = "identity")+facet_wrap(~variable,scales = "free_x",ncol=2)+
  ylab("Precipitación (mm)")+xlab(" ")+
  theme_bw()+theme(strip.text = element_text(size=10))

ggsave("clim_stations.tiff",  height =5, width =10,dpi=200 )


# data_sd_dpto = lapply(data_clim, function(x) apply(x[-1], 1, sd))
# data_sd_dpto_all = as.data.frame(do.call("cbind",data_sd_dpto))
# data_sd_dpto_all = cbind("month" = month.esp, data_sd_dpto_all)
# names(data_sd_dpto_all) = c("month",dptos)




































# Selección de modelos



tabla=read.table("clipboard",header = T)

aggregate(tabla[,4:6],list(tabla$Lead,tabla$region,tabla$Trimestre,tabla$Dpto),mean)
aggregate(tabla$num_cat_ac,list(tabla$Lead,tabla$Trimestre,tabla$Dpto),mean)
aggregate(tabla$num_det_ac2,list(tabla$Lead,tabla$Trimestre,tabla$Dpto),mean)


##############################
tabla=read.table("clipboard",header = T)

tabla$trim = factor(tabla$trim,levels=c("DEF","MAM","JJA","SON"))
tabla$lt = factor(tabla$lt,levels = c("sim","0","3"))

#trim = c("12"="DEF", "3"="MAM","6"="JJA","9"="SON")

library(ggplot2)
x11()
ggplot(tabla,aes(x=GI,y=num_cat_ac,shape=lt,color=Region,size=0.2))+geom_point()+facet_grid(trim~estacion)+theme_bw()+
  scale_size(guide=F)+scale_shape(name="Lead Time")+ylab("% Aciertos categóricos")+xlab("Goodness Index")+
  theme(strip.text.x = element_text(size = 11))

ggsave("best_model.png",width =12 ,height =6,dpi=200 )

stations=c("DoctrinaLa","AptoYopal","AptoPerales","CentAdmoLaUnion","Nataima","Turipana")
tabla_st=tabla[which(tabla$Estacion %in% stations),]

aggregate(tabla_st[,4:6],list(tabla_st$Lead,tabla_st$region,tabla_st$Trimestre,tabla_st$Estacion),mean)
x11()





