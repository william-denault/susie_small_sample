rm(list=ls())
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

dir_cor_smal="ss_sqrt/"#"sqrt_alphabeta_s0/"#"alpha0_beta_05/"#"s0_05/"
# Paths where your simulation outputs are saved
results_path_cor   <- paste0("C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/",
                             dir_cor_smal)
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
colors <- c( "#1A85FF" ,"#D41159","darkgreen" )
n_values <- c(   20, 30, 50,  70 )
h_values <- c(   15,  25,  30 ,   50,    75)
create_NCP_table_stratified = function(temp, method="Default SER", N=10) {
  # Filter dummies if they exist
  if (length(which(temp$is.dummy == 0)) > 0) {
    temp = temp[which(temp$is.dummy == 0), ]
  }

  out = list()
  o = 1
  for (i in 1:nrow(temp)) {
    # Extract the number of effects for this simulation row
    # Adjust "n_effect" to the exact column name in your 'temp' object
    current_n_eff = temp$n_effect[i]

    if (temp$n_cs[i] == 1) {
      out[[o]] = c(temp$n_true_cs[i], temp$n_cs[i], temp[i, ncol(temp)], current_n_eff)
      o = o + 1
    } else {
      # Handle multiple CS cases
      for (k in 1:temp$n_true_cs[i]) {
        out[[o]] = c(1, 1, temp[i, ncol(temp)], current_n_eff)
        o = o + 1
      }
      nfalse = temp$n_cs[i] - temp$n_true_cs[i]
      if(nfalse > 0){
        for (k in 1:nfalse) {
          out[[o]] = c(0, 1, temp[i, ncol(temp)], current_n_eff)
          o = o + 1
        }
      }
    }
  }

  out = do.call(rbind, out)
  out = data.frame(
    response = out[, 1],
    NCP      = out[, 3]/out[, 4],
    n_effect = out[, 4],
    Method   = factor(rep(method, nrow(out))),
    N        = rep(N, nrow(out))
  )
  return(out)
}






df_out=list()
o=1
# Use the stratified function defined previously
df_out = list()
o = 1

for (n in n_values) {
  for (h in h_values) {
    # Load Susie results
    file_susie = paste0(results_path_susie, "small_sample_susie",n,"_h",h,"_L1.RData")
    if (file.exists(file_susie)) {
      load(file_susie)
      df_out[[o]] = create_NCP_table_stratified(temp, method="Default SER", N=n)
      o = o + 1
    }

    # Load Correlated Susie results
    file_csusie = paste0(results_path_cor, "cor_small_sample_susie", n,"_h",h,"_L1.RData")
    if (file.exists(file_csusie)) {
      load(file_csusie)
      df_out[[o]] = create_NCP_table_stratified(temp, method="Servin & Stephens SER", N=n)
      o = o + 1
    }
  }
}

# IMPORTANT: Convert the list to a Data Frame (not a matrix)
df_out1 <- bind_rows(df_out)
df_binned_strat <- df_out1 %>%
  mutate(
    log_ncp = log10(NCP),
    # Using cut_number or cut_interval to ensure enough points per bin
    bin = cut_interval(log_ncp, n = 30)
  ) %>%
  group_by(Method, N, n_effect, bin) %>%
  summarise(
    mean_ncp = mean(NCP, na.rm = TRUE),
    mean_res = mean(response, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_ncp)) # Clean up any empty bins

df_binned_strat <- df_binned_strat[ -which(df_binned_strat$n_effect>1),]


# 2. Plot using the numeric 'mean_ncp'
P1_stratified = ggplot(df_binned_strat, aes(x = mean_ncp, y = mean_res, colour = Method)) +
  geom_line(linewidth = 0.8) +
  geom_point(alpha = 0.6) +
  # Stratify by Number of Effects (rows) and Sample Size (columns)
  facet_grid(n_effect ~ N, labeller = label_both) +
  scale_x_log10( ) +  scale_y_log10( ) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  labs(
    x = "NCP (Log-Spaced Bins)",
    y = "Mean Coverage (Response)",
    title = "CS Coverage vs NCP , single effect L=1 "
  ) +
  scale_color_manual(values = colors) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray95")
  )

print(P1_stratified)
