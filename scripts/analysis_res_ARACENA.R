lf= list.files("/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/")


tl=list()
for( i in 1: length(lf)){


  load(paste0( "/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/", lf[i]))

  tl[[i]]  = c(  length(out$res_susie$sets$cs),
                 length(out$res_susie_small$sets$cs))

}


tl
