library(susieR)

rm(list=ls())

source("/project2/mstephens/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims_unfilter.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=70, L_sim=1, L_susie=1, h=0.75, n_sim=100)

  if(file.exists("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie70_h75_unfilter_L1.rds")){

 temp=    readRDS("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie70_h75_unfilter_L1.rds")

    if(!is.null(temp)){

      temp <- c(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  saveRDS(temp, file="/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie70_h75_unfilter_L1.rds")

}

