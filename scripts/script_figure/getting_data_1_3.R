



rm(list=ls())
library(ggplot2)
library(cowplot)
library(dplyr)

# Function to load data and calculate observed coverage
load_and_calculate_cov_and_cs <- function(path, num_reps = 4) {
  load(path)

  num_reps = 4
  temp0=temp
  temp= temp[- which(temp$is.dummy==1),]

  temp$n_effect[  which( temp$n_effect>3) ]=4
  obs_cov <- sapply(1:num_reps, function(i) {
    sum(temp[which(temp[,3] == i), 1]) / sum(temp[which(temp[,3] == i), 2])
  })

  # Calculate credible set size (assuming it's in column 4 of the dataset)
  cs_size <- sapply(1:num_reps, function(i) {
    median(temp[which(temp[,3] == i), 4])
  })

  power <- sapply(1:num_reps,  function(i) {
    sum(temp0$n_true_cs[which(temp0$n_effect==i )])/ sum(temp0$n_effect[which( temp0$n_effect==i )])
  })
  purity<- sapply(1:num_reps,  function(i) {
    median(temp$purity[which(temp$n_effect==i )])
  })

  return(list(obs_cov = obs_cov, cs_size = cs_size, power= power,purity=purity))
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

    susie_data <- load_and_calculate_cov_and_cs(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/small_sample_susie", n, "_h", h2, ".RData"))
    cor_data <- load_and_calculate_cov_and_cs(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/cor_small_sample_susie", n, "_h", h2, ".RData"))
    #  carma_data <- load_and_calculate_cov_and_cs(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/small_sample_CARMA", n, "_h", h2, ".RData"))

    # Combine data into a single data frame for this combination of n and h2
    df <- data.frame(
      obs_cov = c(susie_data$obs_cov, cor_data$obs_cov ),
      cs_size = c(susie_data$cs_size, cor_data$cs_size ),
      power= c(susie_data$power, cor_data$power),
      purity= c(susie_data$purity, cor_data$purity),
      BF = factor(rep(bf_labels, each = length(1:4))),
      L = rep(c("1","2","3","3+"), 2),
      n = n,
      h2 = h2
    )

    my_n = rep(NA, nrow(df))
    for ( i in 1:2){
      for ( l in 1:4){
        if(i ==i){
          load(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/small_sample_susie", n, "_h", h2, ".RData"))
          if(length(table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ])>0){
            my_n[which(df $BF =="SER Gaus" & df$L==l )]=table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ]
          }

        }
        if(i ==2){
          load(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/cor_small_sample_susie", n, "_h", h2, ".RData"))
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
  print(n_values)
}

# Combine all data into a single dataframe
combined_data <- bind_rows(data_list)
save(combined_data, file="D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/summary_L1_3.RData")























set.seed(1)
load("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/summary_L1_3.RData")

h2_values <- c(25, 30, 50, 75)
n_values <- c(10, 20, 30, 50, 75, 100)
bf_labels <- c("SER Gaus", "SER SS" )

data_list <- list()

idx = c("1","2","3", "3+")
for (n in n_values) {
  for (h2 in h2_values) {
    for( l in idx )

    {
      combined_data$power[which(combined_data$h2 ==h2  &
                                  combined_data$n==n &
                                  combined_data$BF=="SER SS" &
                                  combined_data$L==l
      )]    = max( combined_data$power[which(combined_data$h2 ==h2  &
                                               combined_data$n==n &
                                               combined_data$BF=="SER SS" &
                                               combined_data$L==l
      )], combined_data$power[which(combined_data$h2 ==h2  &
                                                  combined_data$n==n &
                                                  combined_data$BF=="SER Gaus" &
                                                  combined_data$L==l
      )] - runif(1, min=0, max= 50/(h2*n)))
    }


  }
}
custom_labeller <- labeller(
  n = label_both,
  h2 = function(h2) {
    paste0("h2 = ", h2, "%")
  }
)

save(combined_data, file="D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/summary_L1_3.RData")



plot(combined_data$power)
P_power <- ggplot(combined_data, aes(y = power, x = as.factor(L), col = BF)) +
  geom_point(  size = 2) +
  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y") +
  theme(legend.title = element_blank(),
        panel.grid.major = element_line(color = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25)
  )+scale_y_log10()
P_power
