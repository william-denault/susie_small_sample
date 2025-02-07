source("C:/Document/Serieux/Travail/Package/susie_small_sample/scripts/sim_script/ideal_susie_sim.R" )
res=  run_susie_sim (N=30,
                     sd_res = 1,
                     sd_prior = 0.5)




for ( i in 1:2000){

  tres =run_susie_sim (N=30,
                       sd_res = 1,
                       sd_prior = 0.5)

  res= rbind(tres,res)
  save( res, file="C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/n30.RData")

}
