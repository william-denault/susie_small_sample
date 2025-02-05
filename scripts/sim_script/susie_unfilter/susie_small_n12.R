library(susieR)

rm(list=ls())

source("/project2/mstephens/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims_unfilter.R")

for (o in 1:100){

  temp0 <- run_susie_sim(N=30, h=0.75, n_sim=100)

  if(file.exists("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie30_h75_unfilter.RData")){

    load("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie30_h75_unfilter.RData")

    if(!is.null(temp)){

      temp <- c(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  save(temp, file="/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie30_h75_unfilter.RData")

}

