library(susieR
)

rm(list=ls())
source("/home/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims.R")
for ( o in 1:10000){


  temp0 <- run_susie_sim(N=75, h=0.75, n_sim=1000)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie75_h75.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_susie75_h75.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie75_h75.RData")



}
