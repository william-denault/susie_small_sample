library(susieR
)

rm(list=ls())
source("~/susie_small_sample/scripts/sim_script/small_sample_sims_fixed.R")
for ( o in 1:10000){
  temp0 <- run_susie_sim(N=100, h=0.25, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h25fixed.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h25fixed.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h25fixed.RData")




  temp0 <- run_susie_sim(N=100, h=0.50, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h50fixed.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h50fixed.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h50fixed.RData")



  temp0 <- run_susie_sim(N=100, h=0.30, n_sim=100)


  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h30fixed.RData")){
    load("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h30fixed.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h30fixed.RData")





}
