library(susieR)

rm(list=ls())

source("/project2/mstephens/wdenault/susie_small_sample/scripts/sim_script/cor_sample_sim_unfilter.R")

for (o in 1:100000){

  temp0 <- run_susie_sim(N=20, L_sim=1, L_susie=1, h=0.5, n_sim=100)

  if(file.exists("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie20_h50_unfilter_L1.rds")){

   temp= readRDS("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie20_h50_unfilter_L1.rds")

    if(!is.null(temp)){

      temp <- c(temp, temp0)

    } else {

      temp <- temp0

    }

  } else {

    temp <- temp0

  }

  saveRDS(temp, file="/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie20_h50_unfilter_L1.rds")

}

