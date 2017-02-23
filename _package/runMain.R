# Librerias y prerequisitos: 
#   . gunzip
#   . R librarys: fun, lubridate, reshape, string
library(funr)
library(lubridate)
library(reshape)
library(stringr)
library(trend)
library(data.table)

#dirCurrent <- paste(get_script_path(), "/", sep = "", collapse = NULL)
dirCurrent <- "D:/ToBackup/2017/USAID/usaid_forecast/_package/"

## RUN paquete forecast
dirForecast <- paste(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)
dir_save <- paste(dirForecast, "descarga", sep = "", collapse = NULL)
dir_response <- paste(dirForecast, "estacionesMensuales", sep = "", collapse = NULL)
dir_stations <- paste(dirForecast, "dailyData", sep = "", collapse = NULL)
path_prob <- paste(dirForecast, "probForecast", sep = "", collapse = NULL)
path_output <- paste(dirForecast, "resampling", sep = "", collapse = NULL)

if (!file.exists(file.path(dir_save))){
  cat ('\n... directorio "descarga" creado\n\n\n')
  dir.create(file.path(dir_save))
}

if (!file.exists(file.path(dir_probForecast))){
  cat ('... directorio "probForecast" creado\n\n\n')
  dir.create(file.path(dir_probForecast))
}

if (!file.exists(file.path(path_output))){
  cat ('... directorio "path_output" creado\n\n\n')
  dir.create(file.path(path_output))
}

runPrediccion <- source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL))

runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

