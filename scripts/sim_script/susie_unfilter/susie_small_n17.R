library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/sim_script/small_sample_sims_unfilter.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=30, h=0.5, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie30_h50_unfilter.RData")){

 temp=    read ("/home/wdenault/susie_small_sample/simulations/small_sample_susie30_h50_unfilter.RData")

    if(!is.null(temp)){

      temp <- c(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save (temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie30_h50_unfilter.RData")

}

