library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=100, h=0.1, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h10.RData")){

   temp= readRDS("/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h10.RData")

    if(!is.null(temp)){

      temp <- c(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save (temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie100_h10.RData")

}

