library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
colors <- c("#D41159", "#1A85FF", "darkgreen")

# --- Function 0: For standard SuSiE outputs ---
cal_sum_stat0 <- function(temp, method = "Default SER", n = 10, h = 25, type = "Fixed") {
  obs_cov <- numeric(4); obs_purity <- numeric(4); obs_power <- numeric(4)

  for (i in 1:4) {
    idx_no_dummy <- if(i < 4) temp$n_effect == i & temp$is.dummy == 0 else temp$n_effect >= i & temp$is.dummy == 0
    idx_all      <- if(i < 4) temp$n_effect == i else temp$n_effect >= i

    obs_cov[i]    <- sum(temp$n_true_cs[idx_no_dummy], na.rm = TRUE) / sum(temp$n_cs[idx_no_dummy], na.rm = TRUE)
    obs_purity[i] <- mean(temp$purity[idx_no_dummy], na.rm = TRUE)
    obs_power[i]  <- sum(temp$n_true_cs[idx_all], na.rm = TRUE) / sum(temp$n_effect[idx_all], na.rm = TRUE)
  }

  data.frame(obs_cov, obs_purity, obs_power, Method = factor(method),
             N = n, H = h, Effect = factor(c("1", "2", "3", "4+")), Type = type)
}

# --- Function 1: For custom/list-based outputs ---
cal_sum_stat <- function(temp, method = "Default SER", n = 10, h = 25, purity_thresh = 0.1, type = "Fixed") {
  n_sim <- length(temp)
  obs_cov <- rep(NA, n_sim); obs_purity <- rep(NA, n_sim); obs_power <- rep(NA, n_sim); n_effect <- rep(NA, n_sim)

  for (i in 1:n_sim) {
    # FIX: Ensure we handle cases where purity might be a vector or matrix
    purity_vals <- temp[[i]]$cs$purity[, 1]
    idx_n <- which(purity_vals > purity_thresh)

    n_effect[i] <- temp[[i]]$summary_reg[3]

    if (length(idx_n) > 0) {
      is_true <- 0
      # Compare CS to true positions
      for (k in idx_n) {
        if (length(intersect(temp[[i]]$true_pos, temp[[i]]$cs$cs[[k]])) > 0) {
          is_true <- is_true + 1
        }
      }
      obs_cov[i]    <- is_true / length(idx_n)
      obs_purity[i] <- mean(purity_vals[idx_n], na.rm = TRUE)
      obs_power[i]  <- is_true / n_effect[i]
    }
  }

  # Helper to get means safely
  get_m <- function(vec, cond) if(any(cond)) mean(vec[cond], na.rm=TRUE) else NA

  data.frame(
    obs_cov    = c(get_m(obs_cov, n_effect==1), get_m(obs_cov, n_effect==2), get_m(obs_cov, n_effect==3), get_m(obs_cov, n_effect>=4)),
    obs_purity = c(get_m(obs_purity, n_effect==1), get_m(obs_purity, n_effect==2), get_m(obs_purity, n_effect==3), get_m(obs_purity, n_effect>=4)),
    obs_power  = c(get_m(obs_power, n_effect==1), get_m(obs_power, n_effect==2), get_m(obs_power, n_effect==3), get_m(obs_power, n_effect>=4)),
    Method = method, N = n, H = h, Effect = factor(c("1", "2", "3", "4+")), Type = type
  )
}

# --- Execution Loop ---
n_values <- c(   20, 30, 50, 70); h_values <- c(  15, 25, 30, 50, 75)
df_out <- list()

for (n in n_values) {
  for (h in h_values) {
    # 1. Default Susie
    f1 <- paste0(results_path_susie, "small_sample_susie", n, "_h", h, ".RData")
    if(file.exists(f1)) { load(f1); df_out[[length(df_out)+1]] <- cal_sum_stat0(temp, "Default SER", n, h) }

    # 2. Servin & Stephens
    f2 <- paste0(results_path_susie, "cor_small_sample_susie", n, "_h", h, "_unfilter_L1.RData")
    if(file.exists(f2)) { load(f2); df_out[[length(df_out)+1]] <- cal_sum_stat(temp, "Servin & Stephens SER", n, h) }

    # 3. TBF
    f3 <- paste0(results_path_susie, "tbf_small_sample_susie", n, "_h", h, "_unfilter_L1.RData")
    if(file.exists(f3)) { load(f3); df_out[[length(df_out)+1]] <- cal_sum_stat(temp, "TBF", n, h) }
  }
}

df_final <- bind_rows(df_out)

# --- Visualization ---
ggplot(df_final, aes(x = Effect, y = obs_cov, color = Method, shape = Type, group = Method)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  facet_grid(N ~ H, labeller = label_both) +
  geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
  theme_bw(base_size = 12) +
  scale_color_manual(values = colors) +
  labs(title = "Coverage by Effect Size, Sample Size (N), and Heritability (H)",
       y = "Observed Coverage", x = "Number of Effects")




ggplot(df_out, aes( x= Effect, y= obs_purity, col=Method))+
  geom_point()+
  facet_grid(H~N)+
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(size=10)
  )+
  scale_color_manual(values =colors)


ggplot(df_out, aes( x= Effect, y= (obs_power+0.0001), col=Method))+
  geom_point()+
  scale_y_log10()+
  facet_grid(H~N)+
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(size=10)
  )+
  scale_color_manual(values =colors)
