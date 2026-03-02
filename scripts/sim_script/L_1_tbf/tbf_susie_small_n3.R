library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/scripts/sim_script/tbf_sample_sim.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=10, h=0.25,L_susie=1, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/tbf_small_sample_susie10_h25_L1.RData")){

    load("/home/wdenault/susie_small_sample/simulations/tbf_small_sample_susie10_h25_L1.RData")

    if(!is.null(temp)){

      temp <- rbind(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save (temp, file="/home/wdenault/susie_small_sample/simulations/tbf_small_sample_susie10_h25_L1.RData")

}

