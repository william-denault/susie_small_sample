library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

colors <- c( "#D41159","#1A85FF"  ,"darkgreen")
cal_sum_stat= function(temp,
                       method="Default SER",
                       n=10,
                       h=25,
                        type="Fixed"){
  obs_cov <- rep( NA, 1)

  obs_purity <- rep( NA, 1)

  obs_power <- rep( NA, 1)




 i=1
      obs_cov[i] <- sum( temp$n_true_cs[which(temp$n_effect == (i )& temp$is.dummy==0)  ] )/sum(temp$n_cs[which(temp$n_effect == (i )& temp$is.dummy==0)  ] )

      obs_purity[i] <- mean (  temp$purity[which(temp$n_effect == (i ) & temp$is.dummy==0) ] )
      obs_power[i] <-  sum( temp$n_true_cs[which(temp$n_effect == (i ) ) ] )/  sum( temp$n_effect[which(temp$n_effect == (i ) ) ] )





  out= data.frame(obs_cov=obs_cov,
                  obs_purity=obs_purity,
                  obs_power=obs_power,
                  Method=factor( rep( method, length(obs_cov) )),
                  N= rep( n, length(obs_cov)),
                  H= rep(h, length(obs_cov)),
                  Effect= factor(c("1")),
                  Type= type
  )

}

# -----------------------------
# exact
# -----------------------------

n_values <- c(10,  20, 30, 50, 70, 100)
h_values <- c(10 ,  15,  25,  30 ,  50 ,  75)

# Paths where your simulation outputs are saved
 df_out=list()
o=1
for ( n in n_values){
  for( h in h_values){
    file_susie=paste0(results_path_susie, "small_sample_susie",n,"_h",h,"_L1.RData")

    load(file_susie )
    df_out[[o]]= cal_sum_stat( temp,
                               method="Default SER",
                               n=n,
                               h=h,
                               type= "Fixed")
    o= o+1
    file_susie=paste0(results_path_susie, "small_sample_susie",n,"_h",h,".RData")

    load(file_susie )
    df_out[[o]]= cal_sum_stat( temp,
                               method="Default SER",
                               n=n,
                               h=h,
                               type= "Estimated")
    o= o+1
    file_csusie=paste0(results_path_susie, "cor_small_sample_susie",n,"_h",h,"_L1.RData")

    load(file_csusie)
    df_out[[o]]= cal_sum_stat( temp,
                               method="Servin & Stephens SER",
                               n=n,
                               h=h,
                               type= "Fixed")
    o= o+1
    file_csusie=paste0(results_path_susie, "cor_small_sample_susie",n,"_h",h,".RData")

    load(file_csusie)
    df_out[[o]]= cal_sum_stat( temp,
                               method="Servin & Stephens SER",
                               n=n,
                               h=h,
                               type=  "Estimated")
    o= o+1
    file_tbf=paste0(results_path_susie, "tbf_small_sample_susie",n,"_h",h,"_L1.RData")

    load( file_tbf)
    df_out[[o]]= cal_sum_stat( temp,
                               method="TBF",
                               n=n,
                               h=h,
                               type= "Fixed")
    o= o+1
    file_tbf=paste0(results_path_susie, "tbf_sample_susie",n,"_h",h,".RData")

    load( file_tbf)
    df_out[[o]]= cal_sum_stat( temp,
                               method="TBF",
                               n=n,
                               h=h,
                               type= "Estimated")
    o= o+1
  }

}

df_out = do.call(rbind, df_out)
df_out


library(ggplot2)

ggplot(df_out, aes( x=Method, col=Method,shape = Type, y=obs_cov))+
  geom_point(position_dodge=.2)+
  facet_grid(N~H)



ggplot(df_out, aes(x = Method,
                   y = obs_cov,
                   colour = Method,
                   shape = Type,
                   group = Type)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) +
  facet_grid(N ~ H)+

  geom_hline(yintercept = .95)+
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(size=10)
  )+
scale_color_manual(values =colors)


