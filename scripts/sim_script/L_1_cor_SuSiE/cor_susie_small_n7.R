library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/scripts/sim_script/cor_sample_sim.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=20, h=0.1,L_susie=1, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/cor_small_sample_susie20_h10_L1.RData")){

    load("/home/wdenault/susie_small_sample/simulations/cor_small_sample_susie20_h10_L1.RData")

    if(!is.null(temp)){

      temp <- rbind(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save (temp, file="/home/wdenault/susie_small_sample/simulations/cor_small_sample_susie20_h10_L1.RData")

}

