library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(PRROC)
library(cowplot)

# ============================================================
# Paths & colors
# ============================================================
colors <- c(
  "Default SER"           = "#1A85FF",
  "Servin and Stephens SER" = "#D41159"
)

results_path <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

# ============================================================
# Parameters
# ============================================================
n_values <- c(20, 30, 50, 70, 100, 200)
h_values <- c(0.1, 0.15, 0.25, 0.3, 0.5, 0.75)

# ============================================================
# Helper: load one file and return PR curve data frame
# ============================================================
compute_pr_df <- function(file_path, method_name, N, h) {
  mat <- as.matrix(fread(file_path))

  if (sum(is.na(mat)) > 0) {
    to_rm <- unique(c(which(is.na(mat[, 1])), which(is.na(mat[, 2]))))
    mat   <- mat[-to_rm, ]
  }

  truth      <- as.numeric(mat[, 1])
  score      <- as.numeric(mat[, 2])
  scores_pos <- score[truth == 1]
  scores_neg <- score[truth == 0]

  pr <- PRROC::pr.curve(
    scores.class0 = scores_pos,
    scores.class1 = scores_neg,
    curve         = TRUE
  )

  data.frame(
    Recall    = pr$curve[, 1],
    Precision = pr$curve[, 2],
    Method    = factor(method_name,
                       levels = c("Default SER", "Servin and Stephens SER")),
    N         = N,
    h         = h
  )
}

# ============================================================
# Load all combinations
# ============================================================
all_pr <- list()

for (n in n_values) {
  for (h in h_values) {
    file_susie <- paste0(results_path, "pip_susie_n",    n, "_h", h, ".csv")
    file_cor   <- paste0(results_path, "pip_corsusie_n", n, "_h", h, ".csv")

    if (file.exists(file_susie) && file.exists(file_cor)) {
      all_pr[[length(all_pr) + 1]] <- compute_pr_df(file_susie, "Default SER",           n, h)
      all_pr[[length(all_pr) + 1]] <- compute_pr_df(file_cor,   "Servin and Stephens SER", n, h)
    } else {
      warning(paste("Missing files for n =", n, ", h =", h))
    }
  }
}

final_df <- do.call(rbind, all_pr)

# ============================================================
# Labellers  (h already in decimal → convert to % for display)
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
    panel.grid.major  = element_line(color = "gray80"),
    strip.background  = element_blank(),
    strip.text        = element_text(size = 9, face = "bold"),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.title        = element_text(size = 12)
  )

# ============================================================
# Fig 8 – Precision–Recall curves  (log Precision, matching paper)
# ============================================================
fig8 <- ggplot(final_df,
               aes(x = Recall,
                   y = log(Precision + 1e-6),
                   colour = Method)) +
  geom_line(linewidth = 0.6) +
  facet_grid(N ~ h, labeller = n_labeller) +
  xlim(c(0, 0.5)) +
  scale_color_manual(values = colors) +
  labs(
    title = "Precision–Recall Curves Across Sample Sizes and Heritabilities",
    x     = "Recall",
    y     = "log(Precision + 1e-6)"
  ) +
  theme_paper

fig8

ggsave(
  filename = paste0(getwd(),"/plots/fig8_PR_curves.png"),
  plot     = fig8,
  width    = 20,
  height   = 10
)

# Save the plot
ggsave(
  filename =  paste0(getwd(),"/plots/RRC_N_updated_labels.png"),
  plot = final_plot,
  width = 20,
  height = 10
)


