
source("/project2/mstephens/wdenault/susie_small_sample/scripts/sim_script/unfitlered_susie_small.R")
res=  run_susie_sim (N=30,h=0.25)

if(file.exists( "/project2/mstephens/wdenault/susie_small_sample/simulations/unfilter_n30h25_susie_small.RData")){
  load("/project2/mstephens/wdenault/susie_small_sample/simulations/unfilter_n30h25_susie_small.RData")

}

for ( i in 1:100){

  tres =run_susie_sim (N=30,h=0.25,
                       L_sim=1,
                       L_susie=10)

  res= rbind(tres,res)
  save( res, file="/project2/mstephens/wdenault/susie_small_sample/simulations/unfilter_n30h25_susie_small.RData")

}
