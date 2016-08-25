library(raster)
#chirps = raster(x = "D:/Tobackup/CIAT/Projects/USAID/Data_clima/data_filling/stack_chirps.grd")
chirps_all = stack("D:/Tobackup/CIAT/Projects/USAID/Data_clima/data_filling/stack_chirps.grd")

setwd("D:\\Tobackup\\CIAT\\Projects\\USAID\\Data_clima\\data_filling\\")
dpto="casanare"
dir.create(dpto)
  
station_data = read.csv(file = "casanare_precip_filter2.csv",header=T)
station_coord = read.csv(file = "casanare_catalog.csv",header=T)

station_chirps.b = raster::extract(x=chirps_all, y=station_coord[-1], method = 'bilinear')
station_chirps.b = as.data.frame(t(station_chirps.b))[1:nrow(station_data),]
names(station_chirps.b)=names(station_data)

dates=seq(as.Date("1981/01/01"),as.Date("2013/12/31"),"month")
months=months.Date(dates)
names_st=names(station_chirps.b)

dir.create(paste0(dpto,"/all"))
dir.create(paste0(dpto,"/monthly"))
setwd(paste0("D:\\Tobackup\\CIAT\\Projects\\USAID\\Data_clima\\data_filling\\",dpto,"/all/"))

add_legend <- function(...) {
       opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
               mar=c(0, 0, 0, 0), new=TRUE)
              on.exit(par(opar))
              plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
              legend(...)
   }

for (i in 1:ncol(station_chirps.b)){
  model = lm(station_data[,i]~station_chirps.b[,i])
  rmse <- round(sqrt(mean(resid(model)^2)), 2)
  coefs <- coef(model)
  b0 <- round(coefs[1], 2)
  b1 <- round(coefs[2],2)
  r2 <- round(summary(model)$r.squared, 2)
  
  eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                  R^2 == .(r2) * "," ~~ RMSE == .(rmse))
  
   
  data_model = predict(model,as.data.frame(station_chirps.b[,i]))
  data_model[data_model<0] = 0
  tiff(paste0(names_st[i],".tiff"),compression = 'lzw',height = 10,width = 10,units="in", res=200)
  par(mfrow=c(2,1))
  
  plot(dates,station_data[,i],lwd=1.5,type="l",xlab="",ylab="Precipitation (mm)",main=names_st[i])
  lines(dates,station_chirps.b[,i],col="red",lty=2,lwd=1)
  lines(dates,data_model,col="blue",lty=2)
  
    
  plot(station_data[,i],station_chirps.b[,i],xlab="Observed_stations",ylab="CHIRPS")
  abline(model,col="red")
  legend('bottomright', legend = eqn, bty = 'n')
  
  add_legend("topright",c("Observed","CHIRPS","Model"),
             horiz=T, bty='n', cex=0.9,lty=c(1,2,2),lwd=c(1.5,1,1),col=c("black","blue","red")) 
  
  dev.off()
  
  pos.na = which(is.na(station_data[,i]))
  station_data[pos.na,i] = as.numeric(data_model[pos.na])
  
}

write.csv(cbind("date" = format(dates,"%Y-%m"),station_data),paste0(dpto,"_fill.csv"),row.names = F)

# setwd(paste0("D:\\Tobackup\\CIAT\\Projects\\USAID\\Data_clima\\data_filling\\",dpto,"/monthly/"))
# for (i in 1:ncol(station_chirps.b)){
#   for(j in 1:12){
#   pos = which(months==month.name[j])
#   y=station_data[pos,i]
#   x=station_chirps.b[pos,i]
#   model = lm(y~x)
#   rmse <- round(sqrt(mean(resid(model)^2)), 2)
#   coefs <- coef(model)
#   b0 <- round(coefs[1], 2)
#   b1 <- round(coefs[2],2)
#   r2 <- round(summary(model)$r.squared, 2)
#   
#   eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
#                   R^2 == .(r2) * "," ~~ RMSE == .(rmse))
#   
#   
#   dir.create(names_st[i])
#   tiff(paste0(names_st[i],"/",names_st[i],"_",month.name[j],".tiff"),compression = 'lzw',height = 10,width = 10,units="in", res=200)
#   par(mfrow=c(2,1))
#   
#   plot(dates[pos],station_data[pos,i],lwd=1.8,type="l",xlab="",ylab="Precipitation (mm)",main=paste(names_st[i],month.name[j],sep="_"))
#   lines(dates[pos],station_chirps.b[pos,i],col="red",lty=2,lwd=1)
#   lines(dates[pos],predict(model, x),col="blue",lty=3,lwd=1)
#   
#   plot(station_data[pos,i],station_chirps.b[pos,i],xlab="Observed_stations",ylab="CHIRPS")
#   abline(model,col="red")
#   legend('bottomright', legend = eqn, bty = 'n')
#   dev.off()
#   }
# }

