library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims.R")

for (o in 1:100000){

  L = sample (1:4, size=1)
  temp0 <- run_susie_sim(N=10, h=0.25,L_susie=L, L_sim=L, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie10_h25_L1.RData")){

   load("/home/wdenault/susie_small_sample/simulations/small_sample_susie10_h25_L1.RData")

    if(!is.null(temp)){

      temp <- rbind(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save (temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie10_h25_L1.RData")

}

