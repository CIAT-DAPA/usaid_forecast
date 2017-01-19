declare -a conf_a=('/home/aesquivel/CPT_obs/15.3.7/config/1-conf' '/home/aesquivel/CPT_obs/15.3.7/config/2-conf' '/home/aesquivel/CPT_obs/15.3.7/config/3-conf' '/home/aesquivel/CPT_obs/15.3.7/config/4-conf');


#declare -a conf_a=('/home/aesquivel/CPT/15.3.7/config/1-conf');

for conf in "${conf_a[@]}"
do  
  echo "Ciclo normal"
  sh cpt-ciclo.sh $conf
  echo "Ciclo simultaneo"
  sh cpt-ciclo-sim.sh $conf-sim
  echo "Ciclo normal retro"
  sh cpt-retoactive.sh $conf
  echo "Ciclo simultaneo retro"
  sh cpt-retoactive-sim.sh $conf-sim


done