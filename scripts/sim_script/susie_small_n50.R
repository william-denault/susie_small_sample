library(susieR
)

rm(list=ls())
source("C:/Document/Serieux/Travail/Data_analysis_and_papers/susie_small_sample/scripts/sim_script/small_sample_sims.R")
for ( o in 1:10000){
  temp0 <- run_susie_sim(N=50, h=0.25, n_sim=100)

  if(file.exists("./simulations/small_sample_susie50_h25.RData")){
    load("./simulations/small_sample_susie50_h25.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="./simulations/small_sample_susie50_h25.RData")




  temp0 <- run_susie_sim(N=50, h=0.50, n_sim=100)

  if(file.exists("./simulations/small_sample_susie50_h50.RData")){
    load("./simulations/small_sample_susie50_h50.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="./simulations/small_sample_susie50_h50.RData")



  temp0 <- run_susie_sim(N=50, h=0.30, n_sim=100)


  if(file.exists("./simulations/small_sample_susie50_h30.RData")){
    load("./simulations/small_sample_susie50_h30.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }
  }else{
    temp <- temp0
  }

  save(temp, file="./simulations/small_sample_susie50_h30.RData")





}
