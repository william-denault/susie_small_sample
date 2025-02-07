####
###merging results  ------
#####

merge=FALSE

if (merge){
  lf= list.files("/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/")


  tl=list()
  for( i in 1: length(lf)){


    load(paste0( "/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/", lf[i]))

    tt = c(  length(out$res_susie$sets$cs), 0, 0,
             length(out$res_susie_small$sets$cs), 0, 0,
             length(out$res_susie_perm$sets$cs), 0, 0,
             length(out$res_susie_small_perm$sets$cs), 0, 0 )

    names(tt) = c("SuSiE","SuSiE_cs_size","SuSiE_cs_purity",
                  "SuSiE_SS","SuSiE_SS_cs_size","SuSiE_SS_cs_purity",
                  "SuSiE_perm","SuSiE_perm_cs_size","SuSiE_perm_cs_purity",
                  "SuSiE_perm_SS","SuSiE_perm_SS_cs_size","SuSiE_perm_SS_cs_purity"  )


    if(length(out$res_susie$sets$cs)>0 ){
      tt[2]=mean(lengths(out$res_susie$sets$cs))
      tt[3]= mean(out$res_susie$sets$purity[,2])
    }
    if(length(out$res_susie_small$sets$cs)>0 ){
      tt[5]=mean(lengths(out$res_susie_small$sets$cs))
      tt[6]= mean(out$res_susie_small$sets$purity[,2])
    }
    if(length(out$res_susie_perm$sets$cs)>0 ){
      tt[8]=mean(lengths(out$res_susie_perm$sets$cs))
      tt[9]= mean(out$res_susie_perm$sets$purity[,2])
    }
    if(length(out$res_susie_small_perm$sets$cs)>0 ){
      tt[11]=mean(lengths(out$res_susie_small_perm$sets$cs))
      tt[12]= mean(out$res_susie_small_perm$sets$purity[,2])
    }

    tl[[i]]  = tt


    print(i)
  }


  tl=do.call(rbind,tl)
  #colnames(tl) = c("SuSiE", "SuSiE_SS", "SuSiE_perm", "SuSiE_perm_SS")
  #apply(tl, 2 ,sum)

  #dim(tl)

  res= tl

  save(res, file="permutation_Aracena.RData")
}
load("../case_study/permutation_Aracena.RData")

# corresponds Gaussian Ser vs SS Ser on non permuated data
table (res[,1], res[,4])

res= data.frame(res)

median (res$SuSiE_cs_size[which(res$SuSiE >0)])
median (res$SuSiE_cs_purity[which(res$SuSiE >0)])

median (res$SuSiE_SS_cs_size[which(res$SuSiE_SS >0)])


mean (res$SuSiE_cs_purity[which(res$SuSiE >0)])
mean (res$SuSiE_SS_cs_purity[which(res$SuSiE_SS >0)])



mean (res$SuSiE_cs_size[which(res$SuSiE >0 & res$SuSiE_SS >0)])



# corresponds Gaussian Ser vs SS Ser on  permuated data
table (res$SuSiE_perm, res$SuSiE_perm_SS)
