library(ggplot2)
library(cowplot)
library(dplyr)

# Function to load data and calculate observed coverage
load_and_calculate_cov_and_cs <- function(path, num_reps = 1 ) {
  load(path)

  if (length(which(temp$is.dummy==1) )>0){
    temp= temp[- which(temp$is.dummy==1),]
  }


  obs_cov <- sapply(1:num_reps, function(i) {
    sum(temp[which(temp[,3] == i), 1]) / sum(temp[which(temp[,3] == i), 2])
  })

  # Calculate credible set size (assuming it's in column 4 of the dataset)
  cs_size <- sapply(1:num_reps, function(i) {
    median(temp[which(temp[,3] == i), 4])
  })

  power <- sapply(1:num_reps,  function(i) {
    sum(temp$n_true_cs[which( temp$is.dummy==0 & temp$n_effect==i )])/ sum(temp$n_effect[which( temp$n_effect==i )])
  })


  return(list(obs_cov = obs_cov, cs_size = cs_size, power= power))
}

# Function to calculate error bars for each observation
calculate_error <- function(est_cov, n = 10) {
  1.96 * sqrt((est_cov * (1 - est_cov)) / n)
}

# Load and process all datasets
h2_values <- c(25, 30, 50, 75)
n_values <- c(10, 20, 30, 50, 75, 100)
bf_labels <- c("SER Gaus", "SER SS" )

data_list <- list()

for (n in n_values) {
  for (h2 in h2_values) {

    susie_data <- load_and_calculate_cov_and_cs(paste0("~/susie_small_sample/simulations/L_1_small_sample_susie", n, "_h", h2, ".RData"))
    cor_data <- load_and_calculate_cov_and_cs(paste0("~/susie_small_sample/simulations/L_1_cor_small_sample_susie", n, "_h", h2, ".RData"))
    #  carma_data <- load_and_calculate_cov_and_cs(paste0("~/susie_small_sample/simulations/small_sample_CARMA", n, "_h", h2, ".RData"))

    # Combine data into a single data frame for this combination of n and h2
    df <- data.frame(
      obs_cov = c(susie_data$obs_cov, cor_data$obs_cov ),
      cs_size = c(susie_data$cs_size, cor_data$cs_size ),
      power= c(susie_data$power, cor_data$power),
      BF = factor(  bf_labels  ),
      L = rep(1, 2),
      n = rep(n,2),
      h2 = rep(h2,2)
    )

    my_n = rep(NA, nrow(df))
    for ( i in 1:2){
      for ( l in 1:10){
        if(i ==i){
          load(paste0("~/susie_small_sample/simulations/small_sample_susie", n, "_h", h2, ".RData"))
          if(length(table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ])>0){
            my_n[which(df $BF =="SER Gaus" & df$L==l )]=table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ]
          }

        }
        if(i ==2){
          load(paste0("~/susie_small_sample/simulations/cor_small_sample_susie", n, "_h", h2, ".RData"))
          if(length(table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ])>0){
            my_n[which(df $BF =="SER SS" & df$L==l )]=table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ]
          }
        }

      }



    }


    # Calculate error bars
    df$error <- calculate_error(df$obs_cov, n =   my_n)

    data_list[[paste0("n", n, "_h2_", h2)]] <- df
  }
}

# Combine all data into a single dataframe
combined_data_L1 <- bind_rows(data_list)

save(combined_data_L1 , file="/home/wdenault/susie_small_sample/simulations/summary_L1.RData")


P_L1_cov_L1 = ggplot(combined_data_L1  , aes( x= BF , y=obs_cov,
                                                       color= BF ))+
  facet_grid(h2~n)+
  geom_point()+ theme_minimal()+
  geom_hline(yintercept = 0.95)+
  ggtitle("Observed coverage for SER (L=1 fixed) ")
print(P_cov)


# Proper custom labeller for LaTeX-style labels
custom_labeller <- labeller(
  n = label_both,
  h2 = function(h2) {
    paste0("h2 = ", h2, "%")
  }
)


P_L1_cov_L1 =ggplot(combined_data_L1 , aes( x= BF , y=obs_cov,
                                                       color= BF ))+
  facet_grid(h2~n,labeller = custom_labeller)+
  geom_point()+ theme_minimal()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+
  ylab("Observed coverage")+
  xlab("")+ ggtitle("Observed coverage for SER (L=1 fixed) ")

ggsave("/home/wdenault/susie_small_sample/plots/P_L_1_cov_L1.pdf",
       plot = P_L1_cov_L1 ,
       width = 320,
       height = 210,
       units = "mm")
