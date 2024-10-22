lf= list.files("/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/")


tl=list()
for( i in 1: length(lf)){


  load(paste0( "/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/", lf[i]))

   tl[[i]]  = c(  length(out$res_susie$sets$cs),
                  length(out$res_susie_small$sets$cs),
                  length(out$res_susie_perm$sets$cs),
                  length(out$res_susie_small_perm$sets$cs))

}


tl=do.call(rbind,tl)
colnames(tl) = c("SuSiE", "SuSiE_SS", "SuSiE_perm", "SuSiE_perm_SS")
apply(tl, 2 ,sum)

dim(tl)


table (tl[,1], tl[,2])




108/1018
3/390
