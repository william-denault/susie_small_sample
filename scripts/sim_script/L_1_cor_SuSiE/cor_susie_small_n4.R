library(susieR
)


rm(list=ls())
source("~/susie_small_sample/scripts/sim_script/cor_sample_sim.R")
for ( o in 1:10000){
  temp0 <- run_susie_sim(N=50, h=0.25, n_sim=100, L_sim = 1, L_susie = 1)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h25.RData")){
    load("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h25.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h25.RData")




  temp0 <- run_susie_sim(N=50, h=0.50, n_sim=100, L_sim = 1, L_susie = 1)

  if(file.exists("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h50.RData")){
    load("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h50.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h50.RData")



  temp0 <- run_susie_sim(N=50, h=0.30, n_sim=100, L_sim = 1, L_susie = 1)


  if(file.exists("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h30.RData")){
    load("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h30.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h30.RData")


  temp0 <- run_susie_sim(N=50, h=0.75, n_sim=100, L_sim = 1, L_susie = 1)


  if(file.exists("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h75.RData")){
    load("/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h75.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="/home/wdenault/susie_small_sample/simulations/L_1_cor_small_sample_susie50_h75.RData")




}
