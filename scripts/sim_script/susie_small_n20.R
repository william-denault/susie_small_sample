library(susieR
)


source("C:/Document/Serieux/Travail/Data_analysis_and_papers/susie_small_sample/scripts/sim_script/small_sample_sims.R")


for ( o in 1:100000){

  temp0 <- run_susie_sim(N=25, h=0.25, n_sim=10000)

  if(file.exists("small_sample_susie20_h25.RData")){
    load("small_sample_susie20_h25.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }


  }else{
    temp <- temp0
  }

  save(temp, file="small_sample_susie20_h25.RData")

  temp0 <- run_susie_sim(N=25, h=0.50, n_sim=10000)

  if(file.exists("small_sample_susie20_h50.RData")){
    load("small_sample_susie20_h50.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="small_sample_susie20_h50.RData")

  temp0 <- run_susie_sim(N=25, h=0.3, n_sim=10000)
  save(temp, file="small_sample_susie20_h30.RData")
  if(file.exists("small_sample_susie20_h30.RData")){
    load("small_sample_susie20_h30.RData")
    if(!is.null(temp)){
      temp <- rbind (temp, temp0)
    }else{
      temp <- temp0
    }



  }else{
    temp <- temp0
  }

  save(temp, file="small_sample_susie20_h30.RData")
}
