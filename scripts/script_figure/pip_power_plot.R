library(pROC)
library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)

# ============================================================
# Paths & colors
# ============================================================
colors <- c(
  "Default SER"             = "#1A85FF",
  "Servin and Stephens SER" = "#D41159"
)

results_path <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

# ============================================================
# Parameters
# ============================================================
n_values <- c(20, 30, 50, 70, 100, 200)
h_values <- c(0.1, 0.15, 0.25, 0.3, 0.5, 0.75)

# ============================================================
# Helper: load one file and return ROC data frame
# ============================================================
compute_roc_df <- function(file_path, method_name, N, h) {
  mat <- fread(file_path)

  truth   <- as.numeric(unlist(mat[, 1]))
  score   <- as.numeric(unlist(mat[, 2]))

  roc_obj <- roc(truth, score, quiet = TRUE)

  data.frame(
    FPR    = 1 - roc_obj$specificities,
    TPR    = roc_obj$sensitivities,
    Method = factor(method_name,
                    levels = c("Default SER", "Servin and Stephens SER")),
    N      = N,
    h      = h
  )
}

# ============================================================
# Load all combinations
# ============================================================
all_roc <- list()

for (n in n_values) {
  for (h in h_values) {
    file_susie <- paste0(results_path, "pip_susie_n",    n, "_h", h, ".csv")
    file_cor   <- paste0(results_path, "pip_corsusie_n", n, "_h", h, ".csv")

    if (file.exists(file_susie) && file.exists(file_cor)) {
      all_roc[[length(all_roc) + 1]] <- compute_roc_df(file_susie, "Default SER",             n, h)
      all_roc[[length(all_roc) + 1]] <- compute_roc_df(file_cor,   "Servin and Stephens SER", n, h)
    } else {
      warning(paste("Missing files for n =", n, ", h =", h))
    }
  }
}

final_df <- do.call(rbind, all_roc)

# ============================================================
# Labellers  (h stored as decimal → display as %)
# ============================================================
n_labeller <- labeller(
  N = function(x) paste0("n = ", x),
  h = function(x) paste0("h² = ", as.numeric(x) * 100, "%")
)

# ============================================================
# Shared theme
# ============================================================
theme_paper <- theme_cowplot() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    strip.background = element_blank(),
    strip.text       = element_text(size = 9, face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    axis.title       = element_text(size = 12)
  )

# ============================================================
# Fig 9 – ROC curves
# ============================================================
fig9 <- ggplot(final_df,
               aes(x = FPR, y = TPR, colour = Method)) +
  geom_line(linewidth = 0.6) +
  geom_abline(linetype = "dashed", colour = "gray40") +
  facet_grid(N ~ h, labeller = n_labeller) +
  xlim(c(0, 0.5)) +
  scale_color_manual(values = colors) +
  labs(
    title = "ROC Curves Across Sample Sizes and Heritabilities",
    x     = "FPR",
    y     = "TPR"
  ) +
  theme_paper


ggsave(
  filename = paste0(getwd(),"/plots/fig9_ROC_curves.png"),
  plot     = fig9,
  width    = 20,
  height   = 10
)
