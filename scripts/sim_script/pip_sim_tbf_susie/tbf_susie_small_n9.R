library(susieR)

rm(list=ls())

source("/home/wdenault/susie_small_sample/scripts/sim_script/tbf_sample_sim_unfilter.R")

for (o in 1:100){

  temp0 <- run_susie_sim(N=20, h=0.25, n_sim=100,save_pip=TRUE)

}
