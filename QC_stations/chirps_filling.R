library(raster)
chirps = raster(x = "stack_chirps.grd")
chirps_all = stack("stack_chirps.grd")

station_data = read.table("clipboard",header=T)
station_coord = read.table("clipboard",header=T)

station_chirps = raster::extract(x=chirps_all, y=station_coord[-1])
station_chirps = as.data.frame(t(station_chirps))[1:nrow(station_data),]
names(station_chirps)=names(station_data)[-1]

station_chirps.b = raster::extract(x=chirps_all, y=station_coord[-1], method = 'bilinear')
station_chirps.b = as.data.frame(t(station_chirps.b))[1:nrow(station_data),]
names(station_chirps.b)=names(station_data)[-1]

dates=seq(as.Date("1981/01/01"),as.Date("2014/12/31"),"month")
names_st=names(station_chirps.b)

for (i in 1:ncol(station_chirps.b)){
  model = lm(station_data[,i+1]~station_chirps[,i])
  rmse <- round(sqrt(mean(resid(model)^2)), 2)
  coefs <- coef(model)
  b0 <- round(coefs[1], 2)
  b1 <- round(coefs[2],2)
  r2 <- round(summary(model)$r.squared, 2)
  
  eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                  R^2 == .(r2) * "," ~~ RMSE == .(rmse))
  
   
  
  tiff(paste0(names_st[i],".tiff"),compression = 'lzw',height = 10,width = 10,units="in", res=200)
  par(mfrow=c(2,1))
  
  plot(dates,station_data[,i+1],lwd=1.5,type="l",xlab="",ylab="Precipitation (mm)",main=names_st[i])
  #lines(dates,station_chirps[,i],col="red",lty=2,lwd=2)
  lines(dates,station_chirps.b[,i],col="red",lty=2,lwd=1)
  
  plot(station_data[,i+1],station_chirps.b[,i],xlab="Observed_stations",ylab="CHIRPS")
  abline(model,col="red")
  legend('bottomright', legend = eqn, bty = 'n')
  dev.off()
}

station_chirps[,i]-station_chirps.b[,i]

model=lm(station_data[,i+1]~station_chirps[,i])
plot(xx,type="l")
lines(turipana,col="red")

plot(xx[1:408],turipana[,1])
mean(xx[1:408]-turipana[,1],na.rm=T)
sd(xx[1:408]-turipana[,1],na.rm=T)
plot(abs(xx[1:408]-turipana[,1]),type="l")

turipana=read.table("clipboard",header=F)

cor(xx[1:408],turipana[,1],use="complete.obs")
x11()
plot(xy)
xy
extract()
View(xy[])
min(xy[],na.rm=T)
which(xy[]<0)
