# Ciclo para cada localidad
workspace="/home/aesquivel/CPT_obs/15.3.7"
conf_file=$1
echo $conf_file
for localidad in `ls /home/aesquivel/CPT_obs/15.3.7/predictando`
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
     else
         mes_archivos[$i-8]=$line
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
    salidas="$workspace/salidas/retroactive" 
    Retroactive_GoodnessIndex="$salidas/Retroactive_GoodnessIndex_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Pearsons_correlation="$salidas/Retroactive_Pearsons_correlation_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Spearmans_correlation="$salidas/Retroactive_Spearmans_correlation_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_2AFC_Score="$salidas/Retroactive_2AFC_Score_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Hit_Score="$salidas/Retroactive_Hit_Score_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Hit_Skill_Score="$salidas/Retroactive_Hit_Skill_Score_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_LEPS_score="$salidas/Retroactive_LEPS_score_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Gerrity_Score="$salidas/Retroactive_Gerrity_Score_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_2AFC_cat="$salidas/Retroactive_2AFC_cat_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_2AFC_cont="$salidas/Retroactive_2AFC_cont_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_ROC_below="$salidas/Retroactive_ROC_below_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_ROC_above="$salidas/Retroactive_ROC_above_"$mes_numero"_"$predictora"_$localidad"
    
    Reliability_Results_p="$salidas/verification/Reliability_Results_"$mes_numero"_"$predictora"_$localidad"
    ROC_Diagram="$salidas/verification/ROC_Diagram_"$mes_numero"_"$predictora"_$localidad"
    Ranked_Hits_Diagram="$salidas/verification/Ranked_Hits_Diagram_"$mes_numero"_"$predictora"_$localidad"

    #$Y_Input_Data="$salidas/Y_Input_Data_"$mes_numero"_"$predictora"_$localidad"
    #$Categories_Data="$salidas/Categories_Data_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Predictions="$salidas/Retroactive_Predictions_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Forecast_Probabilities="$salidas/Retroactive_Forecast_Probabilities_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Prediction_Limits="$salidas/Retroactive_Prediction_Limits_"$mes_numero"_"$predictora"_$localidad" 
    Retroactive_CanonicalCorrelations="$salidas/Retroactive_CanonicalCorrelations_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_X_CCA_Map_Loadings="$salidas/Retroactive_X_CCA_Map_Loadings_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_X_CCA_Map_Series="$salidas/Retroactive_X_CCA_Map_Series_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Y_CCA_Map_Loadings="$salidas/Retroactive_Y_CCA_Map_Loadings_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_Y_CCA_Map_Series="$salidas/Retroactive_Y_CCA_Map_Series_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_ClimatologyThresholds="$salidas/Retroactive_ClimatologyThresholds_"$mes_numero"_"$predictora"_$localidad"
    Retroactive_ClimatologyAverages="$salidas/Retroactive_ClimatologyAverages_"$mes_numero"_"$predictora"_$localidad"
    # Abrir cpt
./CPT.x <<EOF
1

### Revisar si se estan prendiendo o apagando cuando se llaman
545     #Allow synchronous predictors
541     #Transform Y data
542    #Zero-bound
###
1       #Open X file
$XFile
   
# X Domain
$north #North
$south     #South
$west       #West
$east     #East
# X Mode Options
1       #Minimum modes (X)
5       #Maximum modes (X) 
		
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
3      0#Maximum modes (CCA)
    
#Other options
7       #length of training period (defaults to 1982-2013)
31     #fixed 32-year period
9       #Number of forecasts
1       #1 years
6       #Forecast period settings
2014    #Start year
554     #Transformation settings
2       #Gamma distributions
###


#
112     #Save goodness index
$Retroactive_GoodnessIndex
312 # Run retroactive analysis
# Retroactive forecasts
23  # Length of initial training period 
1   # Training period update interval


		#####   Save results of Retroactive --- skill maps
423 # Retroactive skill maps
1       #Pearson's correlation
$Retroactive_Pearsons_correlation
423 # Retroactive skill maps
2       #Spearman's correlation
$Retroactive_Spearmans_correlation
423 # Retroactive skill maps
3       #2AFC Score
$Retroactive_2AFC_Score
423 # Retroactive skill maps
4       # Hit Score
$Retroactive_Hit_Score
423 # Retroactive skill maps
5      #Hit Skill Score
$Retroactive_Hit_Skill_Score
423 # Retroactive skill maps
6       #LEPS score
$Retroactive_LEPS_score
423 # Retroactive skill maps
7       #Gerrity Score
$Retroactive_Gerrity_Score
423 # Retroactive skill maps
8      #2AFC (categorical)
$Retroactive_2AFC_cat
423 # Retroactive skill maps
9      #2AFC (continuous)
$Retroactive_2AFC_cont
423 # Retroactive skill maps
10      #ROC below
$Retroactive_ROC_below
423 # Retroactive skill maps
11      #ROC above
$Retroactive_ROC_above



431  # Attributes Diagram
y
$Reliability_Results_p
432 #  ROC Diagram
y
$ROC_Diagram
436 # Ranked-Hits Diagram
y
$Ranked_Hits_Diagram



#Save forecast (and other) results
111	#Output results
#3 # Y Input Data
#$Y_Input_Data
#4 # Y Categories Data
#$Categories_Data
# Historical Predictions - Retroactive Predictions
7   #Retroactive Predictions
$Retroactive_Predictions
8   # Retroactive Forecast Probabilities
$Retroactive_Forecast_Probabilities
9   # Retroactive Prediction Limits
$Retroactive_Prediction_Limits


# CCA Results
16 #Save canonical correlations
$Retroactive_CanonicalCorrelations
#   X homogeneous covariance maps
17 # X CCA Map Loadings
$Retroactive_X_CCA_Map_Loadings
18 #  X CCA Map Series
$Retroactive_X_CCA_Map_Series
#  Y homogeneous covariance maps
19 # Y CCA Map Loadings
$Retroactive_Y_CCA_Map_Loadings
20 #  Y CCA Map Series
$Retroactive_Y_CCA_Map_Series


# Climatology
40	# thresholds
$Retroactive_ClimatologyThresholds
41	# averages
$Retroactive_ClimatologyAverages
EOF
  done
  
done
