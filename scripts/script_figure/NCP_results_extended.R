library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(gridExtra)
library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)

colors <- c( "#D41159","#1A85FF" )
create_NCP_table= function(temp,
                           method="Default SER"
){


  if ( length(which(temp$is.dummy==0))>0){

    temp=temp[which(temp$is.dummy==0),]

  }else{

  }

  out=list()
  o=1
  for ( i in 1: nrow(temp)){
    if ( temp$n_cs[i]==1){#case 1 cs
      out[[o]]= c( temp$n_true_cs[i], temp$n_cs[i], temp[i, ncol(temp)])
      o=o+1
    }else{

      if (temp$n_true_cs[i]== temp$n_cs[i]){# case only true cs
        for (  k in 1:temp$n_true_cs[i]){
          out[[o]]= c( 1, 1, temp[i, ncol(temp)])
          o=o+1
        }
      }else{
        for (  k in 1:temp$n_true_cs[i]){# case only has false cs
          out[[o]]= c( 1, 1, temp[i, ncol(temp)])
          o=o+1
        }
        nfalse=temp$n_cs[i]- temp$n_true_cs[i]
        for (  k in 1:nfalse){
          out[[o]]= c( 0, 1, temp[i, ncol(temp)])
          o=o+1
        }


      }

    }


  }

  out = do.call(rbind,out)

  out= data.frame(response= out[,1],
                  NCP=  out[,3],
                  Method=factor(rep(method, nrow(out))))
  return(out)

}


# -----------------------------
# PARAMETERS
# -----------------------------

n_values <- c(10,  20, 30, 50, 70, 100)
h_values <- c(10 ,  15,  25,  30 ,  50 ,  75)

# Paths where your simulation outputs are saved
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
df_out=list()
o=1
for ( n in n_values){
  for( h in h_values){
    file_susie=paste0(results_path_susie, "small_sample_susie",n,"_h",h,".RData")

    load(file_susie )
    df_out[[o]]= create_NCP_table( temp,
                                   method="Default SER"
    )
    o= o+1
    file_csusie=paste0(results_path_susie, "cor_small_sample_susie",n,"_h",h,".RData")

    load(file_csusie)
    df_out[[o]]= create_NCP_table( temp,
                                   method="Servin & Stephens SER"
    )
    o= o+1
  }

}



df_out= do.call(rbind, df_out)
P1= ggplot( df_out, aes(x=(NCP), y=response, colour = Method))+
  # geom_point(alpha=.5) +
  scale_x_log10( ) +
  geom_hline(yintercept = .95)+
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  theme_bw(base_size = 13) +
  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  ) +
  scale_color_manual(values =colors)+
  ggtitle("Probability of CS containning a causal SNP vs NCP \n
          L estimated number of effect from 1 to 10")



create_NCP_table= function(temp,
                           method="Default SER"
){


  if ( length(which(temp$is.dummy==0))>0){

    temp=temp[which(temp$is.dummy==0 &temp$n_effect==1 ),]

  }else{

  }

  out=list()
  o=1
  for ( i in 1: nrow(temp)){
    if ( temp$n_cs[i]==1){#case 1 cs
      out[[o]]= c( temp$n_true_cs[i], temp$n_cs[i], temp[i, ncol(temp)])
      o=o+1
    }else{

      if (temp$n_true_cs[i]== temp$n_cs[i]){# case only true cs
        for (  k in 1:temp$n_true_cs[i]){
          out[[o]]= c( 1, 1, temp[i, ncol(temp)])
          o=o+1
        }
      }else{
        for (  k in 1:temp$n_true_cs[i]){# case only has false cs
          out[[o]]= c( 1, 1, temp[i, ncol(temp)])
          o=o+1
        }
        nfalse=temp$n_cs[i]- temp$n_true_cs[i]
        for (  k in 1:nfalse){
          out[[o]]= c( 0, 1, temp[i, ncol(temp)])
          o=o+1
        }


      }

    }


  }

  out = do.call(rbind,out)

  out= data.frame(response= out[,1],
                  NCP=  out[,3],
                  Method=factor(rep(method, nrow(out))))
  return(out)

}


# -----------------------------
# PARAMETERS
# -----------------------------

n_values <- c(10,  20, 30, 50, 70, 100)
h_values <- c(10 ,  15,  25,  30 ,  50 ,  75)

# Paths where your simulation outputs are saved
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
df_out=list()
o=1
for ( n in n_values){
  for( h in h_values){
    file_susie=paste0(results_path_susie, "small_sample_susie",n,"_h",h,".RData")

    load(file_susie )
    df_out[[o]]= create_NCP_table( temp,
                                   method="Default SER"
    )
    o= o+1
    file_csusie=paste0(results_path_susie, "cor_small_sample_susie",n,"_h",h,".RData")

    load(file_csusie)
    df_out[[o]]= create_NCP_table( temp,
                                   method="Servin & Stephens SER"
    )
    o= o+1
  }

}



df_out= do.call(rbind, df_out)
P2= ggplot( df_out, aes(x=(NCP), y=response, colour = Method))+
  # geom_point(alpha=.5) +
  scale_x_log10( ) +
  geom_hline(yintercept = .95)+
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  theme_bw(base_size = 13) +
  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  ) +
  scale_color_manual(values =colors)+
  ggtitle("Probability of CS containning a causal SNP vs NCP \n
          L estimated, number of effect 1")





create_NCP_table= function(temp,
                           method="Default SER"
){


  if ( length(which(temp$is.dummy==0))>0){

    temp=temp[which(temp$is.dummy==0),]

  }else{

  }

  out=list()
  o=1
  for ( i in 1: nrow(temp)){
    if ( temp$n_cs[i]==1){#case 1 cs
      out[[o]]= c( temp$n_true_cs[i], temp$n_cs[i], temp[i, ncol(temp)])
      o=o+1
    }else{

      if (temp$n_true_cs[i]== temp$n_cs[i]){# case only true cs
        for (  k in 1:temp$n_true_cs[i]){
          out[[o]]= c( 1, 1, temp[i, ncol(temp)])
          o=o+1
        }
      }else{
        for (  k in 1:temp$n_true_cs[i]){# case only has false cs
          out[[o]]= c( 1, 1, temp[i, ncol(temp)])
          o=o+1
        }
        nfalse=temp$n_cs[i]- temp$n_true_cs[i]
        for (  k in 1:nfalse){
          out[[o]]= c( 0, 1, temp[i, ncol(temp)])
          o=o+1
        }


      }

    }


  }

  out = do.call(rbind,out)

  out= data.frame(response= out[,1],
                  NCP=  out[,3],
                  Method=factor(rep(method, nrow(out))))
  return(out)

}


# -----------------------------
# PARAMETERS
# -----------------------------

n_values <- c(10,  20, 30, 50, 70, 100)
h_values <- c(10 ,  15,  25,  30 ,  50 ,  75)

# Paths where your simulation outputs are saved
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
df_out=list()
o=1
for ( n in n_values){
  for( h in h_values){
    file_susie=paste0(results_path_susie, "small_sample_susie",n,"_h",h,"_L1.RData")

    load(file_susie )
    df_out[[o]]= create_NCP_table( temp,
                                   method="Default SER"
    )
    o= o+1
    file_csusie=paste0(results_path_susie, "cor_small_sample_susie",n,"_h",h,"_L1.RData")

    load(file_csusie)
    df_out[[o]]= create_NCP_table( temp,
                                   method="Servin & Stephens SER"
    )
    o= o+1
  }

}



df_out= do.call(rbind, df_out)
P3 =ggplot( df_out, aes(x=(NCP), y=response, colour = Method))+
  # geom_point(alpha=.5) +
  scale_x_log10( ) +
  geom_hline(yintercept = .95)+
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  theme_bw(base_size = 13) +

  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  ) +
  scale_color_manual(values =colors)+
  ggtitle("Probability of CS containning a causal SNP vs NCP \n
          L fixed =1 , number of effect 1")

grid.arrange(P1,P2,P3, ncol=3)

