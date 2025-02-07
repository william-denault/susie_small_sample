
source("C:/Document/Serieux/Travail/Package/susie_small_sample/scripts/sim_script/unfitlered_susie.R" )
res=  run_susie_sim (N=30,h=0.25)

if(file.exists( "C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/unfilter_n30h25_susie.RData")){
  load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/unfilter_n30h25_susie.RData")

}


for ( i in 1:2000){

  tres =run_susie_sim (N=30,h=0.25,
                       L_sim=1,
                       L_susie=1)


  res= rbind(tres,res)
  save( res, file="C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/unfilter_n30h25_susie.RData")

}
