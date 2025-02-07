library(susieR
)

rm(list=ls())
source("/home/wdenault/susie_small_sample/scripts/sim_script/CARMA_sample_sim.R")

for ( o in 1:75000){

  temp0 <- run_CARMA_sim(N=75, h=0.25, n_sim=10)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h25.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h25.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }
  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h25.RData")


  temp0 <- run_CARMA_sim(N=75, h=0.50, n_sim=10)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h50.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h50.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h50.RData")


  temp0 <- run_CARMA_sim(N=75, h=0.30, n_sim=10)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h30.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h30.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h30.RData")



  temp0 <- run_CARMA_sim(N=75, h=0.75, n_sim=10)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h75.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h75.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_CARMA75_h75.RData")

}
