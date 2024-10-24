library(susieR
)

rm(list=ls())
source("/home/wdenault/susie_small_sample/scripts/sim_script/CARMA_sample_sim_spike_slab.R")

for ( o in 1:20000){

  temp0 <- run_CARMA_sim(N=20, h=0.75, n_sim=10 )

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA_spike_slab20_h75.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA_spike_slab20_h75.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA_spike_slab20_h75.RData")



}
