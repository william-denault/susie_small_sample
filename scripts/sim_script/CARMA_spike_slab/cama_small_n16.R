library(susieR
)

rm(list=ls())
source("/home/wdenault/susie_small_sample/scripts/sim_script/CARMA_sample_sim_spike_slab.R")

for ( o in 1:20000){


  temp0 <- run_CARMA_sim(N=75, h=0.50, n_sim=10 )

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA_spike_slab75_h50.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA_spike_slab75_h50.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA_spike_slab75_h50.RData")




}
