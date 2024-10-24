library(susieR
)

rm(list=ls())
source("/home/wdenault/susie_small_sample/scripts/sim_script/CARMA_sample_sim.R")

for ( o in 1:10000){



  temp0 <- run_CARMA_sim(N=20, h=0.75, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA20_h75.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA20_h75.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA20_h75.RData")

}
