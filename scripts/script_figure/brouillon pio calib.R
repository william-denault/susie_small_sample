library(data.table)
library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)

# -----------------------------
# PARAMETERS
# -----------------------------
colors <- c("#1A85FF","#D41159",  "darkgreen")

n_values <- c(10, 20, 30, 50, 70, 100)
h_values <- c(0.1, 0.15, 0.25, 0.3, 0.5, 0.75)

results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

# -----------------------------
# DATA LOADING
# -----------------------------
load_sim_data <- function(file_path, method_name, N, h) {
  mat <- fread(file_path)
  # Use [[]] for robust single-column extraction from data.table
  df <- data.frame(
    causal_flags = as.integer(mat[[1]]),
    pip_scores   = as.numeric(mat[[2]]),
    Method       = method_name,
    N            = N,
    h            = h
  )
  # Drop rows with missing PIPs or flags
  df <- df[!is.na(df$pip_scores) & !is.na(df$causal_flags), ]
  return(df)
}

# -----------------------------
# ROC DATA EXTRACTION
# -----------------------------
get_roc_data <- function(raw_df) {
  roc_obj <- roc(raw_df$causal_flags, raw_df$pip_scores, quiet = TRUE)
  data.frame(
    FPR    = 1 - roc_obj$specificities,
    TPR    = roc_obj$sensitivities,
    AUC    = as.numeric(auc(roc_obj)),
    Method = raw_df$Method[1],   # [1] instead of unique() to guarantee scalar
    N      = raw_df$N[1],
    h      = raw_df$h[1]
  )
}

# -----------------------------
# LOOP OVER ALL COMBINATIONS
# -----------------------------
all_roc     <- list()
all_raw_pip <- list()

for (n in n_values) {
  for (h in h_values) {

    file_cor   <- paste0(results_path_cor,   "pip_corsusie_n", n, "_h", h, ".csv")
    file_susie <- paste0(results_path_susie, "pip_susie_n",    n, "_h", h, ".csv")
    file_tbf   <- paste0(results_path_susie, "pip_tbf_n",      n, "_h", h, ".csv")

    if (file.exists(file_cor) && file.exists(file_susie) && file.exists(file_tbf)) {

      df_cor_raw   <- load_sim_data(file_cor,   "Servin & Stephens SER", n, h)
      df_susie_raw <- load_sim_data(file_susie, "Default SER",           n, h)
      df_tbf_raw   <- load_sim_data(file_tbf,   "TBF",                   n, h)

      combined_raw <- rbind(df_cor_raw, df_susie_raw, df_tbf_raw)
      all_raw_pip[[length(all_raw_pip) + 1]] <- combined_raw

      # ROC: only valid if both classes are present
      for (df_raw in list(df_cor_raw, df_susie_raw, df_tbf_raw)) {
        if (length(unique(df_raw$causal_flags)) == 2) {
          all_roc[[length(all_roc) + 1]] <- get_roc_data(df_raw)
        }
      }
    }
  }
}

# -----------------------------
# PIP CALIBRATION
# -----------------------------
pip_df <- do.call(rbind, all_raw_pip)

num_bins   <- 10
bin_edges  <- seq(0, 1, length.out = num_bins + 1)

# cut() with labels=FALSE returns integer bin index 1..num_bins
pip_df$bin_index <- cut(
  pip_df$pip_scores,
  breaks         = bin_edges,
  include.lowest = TRUE,   # ensures 0 and 1 are included
  labels         = FALSE
)

# FIX: compute bin_center directly as midpoint of each bin index,
# rather than indexing into bin_centers after summarise (which loses the mapping)
bin_centers <- (bin_edges[-length(bin_edges)] + bin_edges[-1]) / 2

summary_df <- pip_df %>%
  group_by(bin_index, N, h, Method) %>%
  summarise(
    mean_pip = mean(pip_scores, na.rm = TRUE),
    eip      = mean(causal_flags, na.rm = TRUE),   # empirical inclusion probability
    count    = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    # Attach bin center by integer index — safe here because bin_index is still integer
    bin_center = bin_centers[as.integer(bin_index)],
    se         = sqrt(eip * (1 - eip) / count),
    ci_lower   = pmax(eip - 1.96 * se, 0),
    ci_upper   = pmin(eip + 1.96 * se, 1)
  ) %>%
  # Remove bins with very few observations to avoid noisy calibration estimates
  filter(count >= 30)

# -----------------------------
# CALIBRATION PLOT
# -----------------------------
plot_pip <- ggplot(summary_df, aes(x = mean_pip, y = eip, color = Method)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.02, alpha = 0.6) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  facet_grid(N ~ h, labeller = label_both) +
  labs(
    title = "PIP Calibration Across Sample Sizes and Heritabilities",
    x     = "Mean PIP in Bin",
    y     = "Empirical Inclusion Probability",
    color = "Method"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 10),
    aspect.ratio    = 1   # keeps calibration axes square for visual clarity
  )

ggsave("PIP_Calibration_grid.png", plot = plot_pip, width = 20, height = 10, dpi = 150)
print(plot_pip)

# -----------------------------
# ROC / AUC SUMMARY PLOT
# -----------------------------
roc_df <- do.call(rbind, all_roc)

# AUC heatmap: one value per (Method, N, h)
auc_df <- roc_df %>%
  select(Method, N, h, AUC) %>%
  distinct()

plot_auc <- ggplot(auc_df, aes(x = factor(h), y = factor(N), fill = AUC)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(AUC, 2)), size = 3) +
  scale_fill_gradient2(
    low      = "white",
    mid      = "#FDE725",
    high     = "#440154",
    midpoint = 0.75,
    limits   = c(0.5, 1)
  ) +
  facet_wrap(~ Method) +
  labs(
    title = "AUC Across Sample Sizes and Heritabilities",
    x     = "Heritability (h)",
    y     = "Sample Size (N)",
    fill  = "AUC"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "right")

ggsave("AUC_heatmap.png", plot = plot_auc, width = 14, height = 6, dpi = 150)
print(plot_auc)
