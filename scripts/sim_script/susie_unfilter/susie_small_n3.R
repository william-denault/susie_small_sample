library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/sim_script/small_sample_sims_unfilter.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=10, h=0.25, n_sim=100)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/small_sample_susie10_h25_unfilter.RData")){

 load ("/home/wdenault/susie_small_sample/simulations/small_sample_susie10_h25_unfilter.RData")

    if(!is.null(temp)){

      temp <- c(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save (temp, file="/home/wdenault/susie_small_sample/simulations/small_sample_susie10_h25_unfilter.RData")

}

