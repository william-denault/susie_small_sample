library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/scripts/sim_script/small_sample_sims.R")

for (o in 1:100){

  temp0 <- run_susie_sim(N=10, h=0.1, n_sim=100,save_pip=TRUE)

}
