#declare -a conf_a=('/home/aesquivel/CPT/15.3.7/config/2-conf');

declare -a conf_a=('/home/aesquivel/CPT/15.3.7/config/1-conf' '/home/aesquivel/CPT/15.3.7/config/2-conf' '/home/aesquivel/CPT/15.3.7/config/3-conf' '/home/aesquivel/CPT/15.3.7/config/4-conf' '/home/aesquivel/CPT/15.3.7/config/5-conf' '/home/aesquivel/CPT/15.3.7/config/6-conf');


#sh test.sh

for conf in "${conf_a[@]}"
do  
   echo "Ciclo normal"
   sh cpt-ciclo.sh $conf
   echo "Ciclo normal retro"
   sh cpt-retoactive.sh $conf

done
