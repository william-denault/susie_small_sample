# 1. Load libraries once
library(pROC)
library(ggplot2)
library(dplyr)
library(data.table)
res_path <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
dir_cor_smal="ss_sqrt/"#"" #"tt2/"#"ss_sqrt/"#
colors <- c( "#1A85FF", "#D41159","darkgreen")

cal_sum_stat <- function(temp, method = "Default SER", n = 10, h = 25) {
  # Pre-allocate results for 4 categories (1, 2, 3, 4+)
  obs_cov    <- numeric(4)
  obs_purity <- numeric(4)
  obs_power  <- numeric(4)
  obs_cs_size  <- numeric(4)
  for (i in 1:4) {
    # Create a logical index for readability and reuse
    if (i < 4) {
      idx_no_dummy <- temp$n_effect == i & temp$is.dummy == 0
      idx_all      <- temp$n_effect == i
    } else {
      idx_no_dummy <- temp$n_effect >= i & temp$is.dummy == 0
      idx_all      <- temp$n_effect >= i
    }

    # Calculations
    obs_cov[i]    <- sum(temp$n_true_cs[idx_no_dummy], na.rm = TRUE) / sum(temp$n_cs[idx_no_dummy], na.rm = TRUE)
    obs_purity[i] <- mean(temp$purity[idx_no_dummy], na.rm = TRUE)
    obs_cs_size[i] <- mean(temp$mean_cs_size [idx_no_dummy], na.rm = TRUE)
    obs_power[i]  <- sum(temp$n_true_cs[idx_all], na.rm = TRUE) / sum(temp$n_effect[idx_all], na.rm = TRUE)
  }

  data.frame(
    obs_cov    = obs_cov,
    obs_purity = obs_purity,
    obs_power  = obs_power,
    obs_cs_size=obs_cs_size,
    Method     = factor(method),
    N          = n,
    H          = h,
    Effect     = factor(c("1", "2", "3", "4+"))
  )
}

# -----------------------------
# PARAMETERS & LOADING
# -----------------------------
n_values <- c( 20, 30, 50 ,70 , 100 ,200)
h_values <- c(10, 15, 25, 30, 50, 75)

# Using a list and incrementing is good;
# but we can use expand.grid for cleaner looping if you prefer
df_list <- list()

for (n in n_values) {
  for (h in h_values) {

    # Define file patterns and their corresponding labels
    configs <- list(
      list(f = paste0("small_sample_susie", n, "_h", h, "_L1.RData"), m = "Default SER"),
      list(f = paste0(dir_cor_smal,"cor_small_sample_susie", n, "_h", h, "_L1.RData"), m = "Servin & Stephens SER")

    )

    for (conf in configs) {
      full_path <- paste0(res_path, conf$f)
      if (file.exists(full_path)) {
        load(full_path) # Assumes object is named 'temp'
        df_list[[length(df_list) + 1]] <- cal_sum_stat(temp, method = conf$m, n = n, h = h)
      } else {
        warning(paste("File missing:", full_path))
      }
    }
  }
}

df_out <- do.call(rbind, df_list)


library(
  ggplot2
)



ggplot(df_out, aes( x= Effect, y= obs_purity, col=Method))+
  geom_point()+
  facet_grid(H~N)+
  ylab("Purity")+
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(size=10)
  )+
  scale_color_manual(values =colors)


ggplot(df_out, aes( x= Effect, y= (obs_power ), col=Method))+
  geom_point()+
  scale_y_log10()+
  facet_grid(H~N)+
  ylab("Power")+
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(size=10)
  )+
  scale_color_manual(values =colors)


ggplot(df_out, aes( x= Effect, y= obs_cov, col=Method))+
  geom_point()+
  facet_grid(H~N)+
  geom_hline(yintercept = .95)+

  ylab("Coverage")+
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(size=10)
  )+
  scale_color_manual(values =colors)

