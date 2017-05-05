# Ciclo para cada localidad
workspace="/home/aesquivel/requerimientos/CPT.15.5.10/15.5.10"
conf_file=$1
echo $conf_file
for localidad in `ls /home/aesquivel/requerimientos/CPT.15.5.10/15.5.10/predictando`
do
	echo "Localidad: $localidad"
  
  # Ciclo para leer variables de predictoras
  i=1
  mes_numero=
  mes_nombre=
  north=
  south=
  west=
  east=
  length=
  start_year= 
  predictora=  
  declare -a mes_archivos=('' '' '');  
  for line in $(cat $conf_file); 
  do 
     
     if  [ $i = 1 ]
     then
	       mes_numero=$line      
     elif  [ $i = 2 ]
     then
	       mes_nombre=$line
     elif  [ $i = 3 ]
     then
	       north=$line
     elif  [ $i = 4 ]
     then
	       south=$line
     elif  [ $i = 5 ]
     then
	       west=$line
     elif  [ $i = 6 ]
     then
	       east=$line
     elif  [ $i = 7 ]
     then
	       length=$line
     elif  [ $i = 8 ]
     then
	       start_year=$line
     else
         mes_archivos[$i-9]=$line
     fi
     i=$(($i+1))
  done
  echo "Corridad: $mes_numero $mes_nombre"
  #echo  ${mes_archivos[@]}
  
  # Ciclo para las variables predictoras
  for p in ${mes_archivos[@]}
  do
    predictora=$p

    echo "Mes: $predictora"
    echo "Coordenadas: $north $south $west $east"
    
    YFile="$workspace/predictando/$localidad"
    echo "Archivo Y $YFile"
    XFile="$workspace/predictores/$predictora.tsv"
    echo "Archivo X $XFile"
    salidas="$workspace/salidas/Cross_validated"
    GoodnessIndex="$salidas/GoodnessIndex_"$mes_numero"_"$predictora"_$localidad"
    Pearsons_correlation="$salidas/Pearsons_correlation_"$mes_numero"_"$predictora"_$localidad"
    Spearmans_correlation="$salidas/Spearmans_correlation_"$mes_numero"_"$predictora"_$localidad"
    k_2AFC_Score="$salidas/k_2AFC_Score_"$mes_numero"_"$predictora"_$localidad"
    Hit_Score="$salidas/Hit_Score_"$mes_numero"_"$predictora"_$localidad"
    Hit_Skill_Score="$salidas/Hit_Skill_Score_"$mes_numero"_"$predictora"_$localidad"
    LEPS_score="$salidas/LEPS_score_"$mes_numero"_"$predictora"_$localidad"
    Gerrity_Score="$salidas/Gerrity_Score_"$mes_numero"_"$predictora"_$localidad"
    k_2AFC_cat="$salidas/k_2AFC_cat_"$mes_numero"_"$predictora"_$localidad"
    k_2AFC_cont="$salidas/k_2AFC_cont_"$mes_numero"_"$predictora"_$localidad"
    ROC_below="$salidas/ROC_below_"$mes_numero"_"$predictora"_$localidad"
    ROC_above="$salidas/ROC_above_"$mes_numero"_"$predictora"_$localidad"
    
    CrossValidated_predictions="$salidas/CrossValidated_predictions"$mes_numero"_"$predictora"_$localidad"
    y_input_data="$salidas/y_input_data_"$mes_numero"_"$predictora"_$localidad"
    y_categories_data="$salidas/y_input_data_"$mes_numero"_"$predictora"_$localidad"
    CanonicalCorrelations="$salidas/CanonicalCorrelations_"$mes_numero"_"$predictora"_$localidad"
    X_CCA_Map_Loadings="$salidas/X_CCA_Map_Loadings_"$mes_numero"_"$predictora"_$localidad"
    X_CCA_Map_Series="$salidas/X_CCA_Map_Series_"$mes_numero"_"$predictora"_$localidad"
    Y_CCA_Map_Loadings="$salidas/Y_CCA_Map_Loadings_"$mes_numero"_"$predictora"_$localidad"
    Y_CCA_Map_Series="$salidas/Y_CCA_Map_Series_"$mes_numero"_"$predictora"_$localidad"
    ForecastProbabilities="$salidas/ForecastProbabilities_"$mes_numero"_"$predictora"_$localidad"
    Deter_forecasts="$salidas/Deter_forecasts_"$mes_numero"_"$predictora"_$localidad"
    Deter_forecast_ensembles="$salidas/Deter_forecast_ensembles_"$mes_numero"_"$predictora"_$localidad"
    Deter_Errors="$salidas/Deter_Errors_"$mes_numero"_"$predictora"_$localidad"
    PredictionLimits="$salidas/PredictionLimits_"$mes_numero"_"$predictora"_$localidad"
    Predictor_Time_Scores="$salidas/Predictor_Time_Scores_"$mes_numero"_"$predictora"_$localidad"
    ClimatologyThresholds="$salidas/ClimatologyThresholds_"$mes_numero"_"$predictora"_$localidad"
    ClimatologyAverages="$salidas/ClimatologyAverages_"$mes_numero"_"$predictora"_$localidad"

    # Abrir cpt
./CPT.x <<EOF
611

1       #Open X file
$XFile
   
# X Domain
$north     #North
$south     #South
$west      #West
$east      #East
# X Mode Options
1       #Minimum modes (X)
5      #Maximum modes (X)
		
2       #Open Y file
$YFile
    
13      #North
-4      #South
-79     #West
-67     #East
1       #Minimum modes (Y)
5       #Maximum modes (Y)
       	
# CCA Options
1       #Minimum modes (CCA)
3       #Maximum modes (CCA)


213 # Target Season
$mes_numero # Mes de inicio del trimestre # Pendiente
   
3 #Length of season to forecast
3 #Length of SPI
    
#Other options
7       #length of training period (defaults to 1982-2013)
$length #fixed 32-year period
9       #Number of forecasts
1       #1 years
6       #Forecast period settings
$start_year        #Start year
554     #Transformation settings
2       #Gamma distributions
###

### Revisar si se estan prendiendo o apagando cuando se llaman
#545   #Allow synchronous predictors (turn on)
541    #Transform Y data
542    #Zero-bound
###


#Cross-validation
112     #Save goodness index
$GoodnessIndex
311     #Run cross-validation analysis
#Run pronosticos
451     #Forecast series
452     #Forecast ensembles
454     #Forecast values
455     #Forecast probabilities


#####   Save results of cross-validation --- skill maps
413     #Cross-validated skill maps
1       #Pearson's correlation
$Pearsons_correlation
413     #Cross-validated skill maps
2       #Spearman's correlation
$Spearmans_correlation
413     #Cross-validated skill maps
3       #2AFC Score
$k_2AFC_Score
413     #Cross-validated skill maps
4       # Hit Score
$Hit_Score
413     #Cross-validated skill maps
5      #Hit Skill Score
$Hit_Skill_Score
413     #Cross-validated skill maps
6       #LEPS score
$LEPS_score
413     #Cross-validated skill maps
7       #Gerrity Score
$Gerrity_Score
413     #Cross-validated skill maps
8      #2AFC (categorical)
$k_2AFC_cat
413     #Cross-validated skill maps
9      #2AFC (continuous)
$k_2AFC_cont
413     #Cross-validated skill maps
10      #ROC below
$ROC_below
413     #Cross-validated skill maps
11      #ROC above
$ROC_above



#Save forecast (and other) results
111	#Output results
# Historical Predictions - Cross-validation
201       #Save cross-validated predictions
$CrossValidated_predictions
102 #  Y Input Data
$y_input_data
101 # Y Categories Data
$y_categories_data
## CCA Results
401 #Save canonical correlations
$CanonicalCorrelations
##   X homogeneous covariance maps
411 # X CCA Map Loadings
$X_CCA_Map_Loadings
412 #  X CCA Map Series
$X_CCA_Map_Series
#  Y homogeneous covariance maps
421 # Y CCA Map Loadings
$Y_CCA_Map_Loadings
422 #  Y CCA Map Series
$Y_CCA_Map_Series

#Forecast 
501	#Save forecast probabilities
$ForecastProbabilities

# Deterministic forecasts
511	#Save deterministic forecasts
$Deter_forecasts
512	#Save deterministic forecast ensembles
$Deter_forecast_ensembles
514	#Save deterministic prediction error variances
$Deter_Errors
## Confidence limits
513	#Save prediction limits
$PredictionLimits

##  Predictors
531  # Predictor Time Scores
$Predictor_Time_Scores.

## Climatology
601	# thresholds
$ClimatologyThresholds
602	# averages
$ClimatologyAverages
EOF
  done
  
done
