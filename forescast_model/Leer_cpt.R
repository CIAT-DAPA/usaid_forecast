#Lizeth Llanos
sy_date=Sys.Date()

download.cpt=function(sy_date){ ### En la funcion se ingresa la fecha del sistema Sys.Date()

  data=list()### Objecto donde se guardan los output
  
  for(i in 0:5){
    
    month=as.numeric(substr(sy_date, 6, 7))### Se extrae el mes de la fecha actual  
    temp <- tempfile()### Crea un archivo temporal 
    #route1=paste("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/S/%280000%201%20",month.abb[first_month],"%20",first_year,"-",last_year,"%29VALUES/L/%280.5%29VALUES/M/1/24/RANGE%5BM%5Daverage/-999/setmissing_value/%5BX/Y%5D%5BS/L/add%5Dcptv10.tsv.gz",sep="")
    route=paste("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/S/%280000%201%20",month.abb[month-1],"%201982-2016%29VALUES/L/",i,".5/",i+2,".5/RANGE%5BL%5D//keepgrids/average/M/1/24/RANGE%5BM%5Daverage/-999/setmissing_value/%5BX/Y%5D%5BS/L/add%5Dcptv10.tsv.gz",sep="")### Ruta de descarga de datos 
    download.file(route, temp)### Realiza la descarga en el archivo temporal 
    gzfile(temp, 'rt')### Descomprime el archivo
    data[[i+1]] <- read.table(temp,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999)### Lee los datos del archivo temporal 
    unlink(temp)### Elimina el archivo temporal 

    }
  
return(data)  
  
}


transform_raster=function(x){
    mapa_base=raster()
    val=c(as.matrix(t(x),ncol=1,byrow = T))
    val[val==-999.000]=NA
    values(mapa_base)=as.numeric(val)
  return(mapa_base)
}


rasterize=function(dates) { 
  
  #install.packages("raster")
  library("raster")
  pos_years=!is.na(dates[1,])
  year_month=dates[1,][pos_years]
  year=substr(year_month[-1],1,4) 
  total_row_delete=c(-1,-3,-(which(dates[,1]=="90.0")-2),-which(dates[,1]=="-90.0"),-which(dates[,1]==""))
  dates=dates[total_row_delete,-1]
  
  list_dates=split(dates,sort(rep(year,180)))
  all_raster=lapply(list_dates,transform_raster)
  layers=stack(all_raster)
  
  return(layers)
  }




sy_date=Sys.Date()
sy_month=as.numeric(substr(sy_date, 6, 7))

x=download.cpt(sy_date)
r=lapply(x,rasterize)
data_tsm=lapply(r,function(x) t(rasterToPoints(x)))
predictor_tsm=lapply(data_tsm,"[",c(-1,-2),)
years_predictor=as.numeric(substr(rownames(predictor_tsm[[1]]), 2, 5))


#### cargan datos de estaciones ######
Response_prec=read.table("clipboard",dec=".",header = T)

list_response=split(Response_prec,Response_prec$month)
years_response=list_response[[2]]$Year

coordinates_response=t(list_response$`0`[,c(-1,-2)])

response_select=list_response[sy_month+1:6]


library("CCA")

years_model=intersect(years_predictor,years_response)



cci=cc(as.matrix(predictor_tsm[[1]][years_predictor%in%years_model,1:100]),as.matrix(list_response[[9]][years_response%in%years_model,-1:-2]))

View(as.matrix(predictor_tsm[[1]][years_predictor%in%years_model,1:100]))
View(as.matrix(list_response[[9]][years_response%in%years_model,-1:-2]))
     
View(predictor_tsm[[1]])
