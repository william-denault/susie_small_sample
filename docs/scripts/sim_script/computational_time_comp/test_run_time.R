library(susieR
)

rm(list=ls())
library(CARMA)
source("~/susie_small_sample/scripts/sim_script/run_time_comp.R")

for ( o in 1:10000){


  if(file.exists("/home/wdenault/susie_small_sample/simulations/run_time.RData")){
    load("/home/wdenault/susie_small_sample/simulations/run_time.RData")



    tt= run_time()

    res=rbind(res,tt)
  }else{
    tt= run_time()

    res=tt


  }



  save( res, file="/home/wdenault/susie_small_sample/simulations/run_time.RData")


print(o)

}
