library(susieR
)


set.seed(123)
source("C:/Document/Serieux/Travail/Data_analysis_and_papers/susie_small_sample/script/sim_script/small_sample_sims.R")

temp <- run_susie_sim(N=20, h=0.25, n_sim=20000)
save(temp, file="small_sample_susie20_h25.RData") 


temp <- run_susie_sim(N=20, h=0.50, n_sim=20000) 
save(temp, file="small_sample_susie20_h50.RData") 

temp <- run_susie_sim(N=20, h=0.30, n_sim=20000) 
save(temp, file="small_sample_susie20_h30.RData") 
