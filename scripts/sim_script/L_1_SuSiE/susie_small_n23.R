library(susieR
)

rm(list=ls())
source("/home/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims.R")
for ( o in 1:10000){

  temp0 <- run_susie_sim(N=100, h=0.30, n_sim=100, L_sim = 1, L_susie = 1)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/L_1_small_sample_susie100_h30.RData")){
    load("/home/wdenault/susie_small_sample/simulations/L_1_small_sample_susie100_h30.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/L_1_small_sample_susie100_h30.RData")




}
