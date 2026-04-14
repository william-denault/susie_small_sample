library(data.table)
library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)

# -----------------------------
# PARAMETERS
# -----------------------------
colors <- c( "#1A85FF","#D41159","darkgreen" )

n_values <- c( 10, 20, 30, 50, 70, 100)
h_values <- c(.1 , .15, .25, .3 , .5 , .75)

# Paths where your simulation outputs are saved
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

# -----------------------------
# FUNCTION TO LOAD RAW DATA
# -----------------------------
# Instead of just returning ROC stats, this returns the raw true/score pairs
# so we can use them for BOTH ROC curves and PIP calibration.

load_sim_data <- function(file_path, method_name, N, h) {
  mat <- fread(file_path)

  df <- data.frame(
    causal_flags = c(unlist((mat[,1]))),
    pip_scores   = c(unlist((mat[,2]))),
    Method       = method_name,
    N            = N,
    h            = h
  )
  return(df)
}

# Function to extract ROC data from the raw dataframe
get_roc_data <- function(raw_df) {
  roc_obj <- roc(raw_df$causal_flags, raw_df$pip_scores, quiet = TRUE)
  data.frame(
    FDR    = 1 - roc_obj$specificities,
    Power    = roc_obj$sensitivities,
    Method = unique(raw_df$Method),
    N      = unique(raw_df$N),
    h      = unique(raw_df$h)
  )
}

# -----------------------------
# LOOP OVER ALL COMBINATIONS
# -----------------------------

all_roc <- list()
all_raw_pip <- list() # List to store raw PIPs for calibration

for (n in n_values) {
  for (h in h_values) {

    file_cor   <- paste0(results_path_cor,   "pip_corsusie_n",n,"_h" ,h, ".csv")
    file_susie <- paste0(results_path_susie, "pip_susie_n",n,"_h" ,h, ".csv")
    file_tbf   <- paste0(results_path_susie, "pip_tbf_n",n,"_h" ,h, ".csv")

    if (file.exists(file_cor) & file.exists(file_susie) & file.exists(file_tbf)) {

      # 1. Load Raw Data
      df_cor_raw   <- load_sim_data(file_cor,   "Servin and Stephens SER", n, h)
      df_susie_raw <- load_sim_data(file_susie, "Default SER",           n, h)
      df_tbf_raw   <- load_sim_data(file_tbf,   "tbf",                   n, h)

      # Store raw data for PIP calibration
      combined_raw <- rbind(df_cor_raw, df_susie_raw, df_tbf_raw)
      all_raw_pip[[length(all_raw_pip)+1]] <- combined_raw

      # 2. Compute ROC Data
      df_cor_roc   <- get_roc_data(df_cor_raw)
      df_susie_roc <- get_roc_data(df_susie_raw)
      df_tbf_roc   <- get_roc_data(df_tbf_raw)

      combined_roc <- rbind(df_cor_roc, df_susie_roc, df_tbf_roc)
      all_roc[[length(all_roc)+1]] <- combined_roc
    }
  }
}



# -----------------------------
# PIP CALIBRATION PROCESSING
# -----------------------------
# Combine all raw data into one massive dataframe
pip_df <- do.call(rbind, all_raw_pip)

# Define bins
num_bins <- 11
bin_edges <- seq(0, 1, length.out = num_bins + 1)
bin_centers <- (bin_edges[-length(bin_edges)] + bin_edges[-1]) / 2

# Assign bin indices
pip_df$bin_indices <- cut(pip_df$pip_scores, breaks = bin_edges, include.lowest = TRUE, labels = FALSE)

# Compute summary statistics per bin, N, h, AND method
summary_df <- pip_df %>%
  group_by(bin_indices, N, h, Method) %>%
  summarise(
    mean_pip = mean(pip_scores, na.rm = TRUE),
    eip      = mean(causal_flags, na.rm = TRUE),
    count    = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    se         = sqrt(eip * (1 - eip) / count),
    ci_lower   = pmax(eip - 1.96 * se, 0),
    ci_upper   = pmin(eip + 1.96 * se, 1),
    bin_center = bin_centers[as.numeric(bin_indices)]
  )

# Filter out bins with too few observations
# *Note:* Because you are splitting the data across N, h, and Method, a count of 60
# might remove too many bins if your simulation runs per grid square are small.
# Adjust this number if your plot looks too sparse!
#summary_df <- summary_df %>% filter(count >= 60)

# -----------------------------
# PIP CALIBRATION PLOT
# -----------------------------
plot_pip <- ggplot(summary_df, aes(x = mean_pip, y = eip, color = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.02) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  scale_color_manual(values = colors) +
  facet_grid(N ~ h, labeller = label_both) +
  labs(
    title = "PIP Calibration Across Sample Sizes and Heritabilities",
    x = "Mean PIP in Bin",
    y = "Empirical Inclusion Probability",
    color = "Method"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  )

ggsave("PIP_Calibration_grid.png", plot = plot_pip, width = 20, height = 10)
print(plot_pip)
