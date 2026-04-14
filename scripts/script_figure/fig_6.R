library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)

# ============================================================
# Paths & colors
# ============================================================
dir_cor_smal       <-"ss_sqrt/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_cor   <- paste0(results_path_susie, dir_cor_smal)

colors <- c(
  "Default SER"           = "#1A85FF",
  "Servin & Stephens SER" = "#D41159"
)

# ============================================================
# Helper
# ============================================================
cal_sum_stat <- function(temp, method = "Default SER",
                         n = 10, h = 25, type = "Fixed") {
  i <- 1
  idx_nd <- temp$n_effect == i & temp$is.dummy == 0
  idx_all <- temp$n_effect == i

  data.frame(
    obs_cov    = sum(temp$n_true_cs[idx_nd])  / sum(temp$n_cs[idx_nd]),
    obs_purity = mean(temp$purity[idx_nd]),
    obs_power  = sum(temp$n_true_cs[idx_all]) / sum(temp$n_effect[idx_all]),
    Method     = factor(method,
                        levels = c("Default SER", "Servin & Stephens SER")),
    N          = n,
    H          = h,
    Effect     = factor("1"),
    Type       = type
  )
}

# ============================================================
# Load data
# ============================================================
n_values <- c(  20, 30, 50, 70, 100,200)
h_values <- c(10, 15, 25, 30, 50, 75)

df_list <- list()

for (n in n_values) {
  for (h in h_values) {

    # Default SER – Fixed (L = 1)
    f <- paste0(results_path_susie, "small_sample_susie", n, "_h", h, "_L1.RData")
    if (file.exists(f)) {
      load(f)
      df_list[[length(df_list) + 1]] <-
        cal_sum_stat(temp, "Default SER", n, h, "Fixed")
    }

    # Default SER – Estimated
    f <- paste0(results_path_susie, "small_sample_susie", n, "_h", h, ".RData")
    if (file.exists(f)) {
      load(f)
      df_list[[length(df_list) + 1]] <-
        cal_sum_stat(temp, "Default SER", n, h, "Estimated")
    }

    # SS-SER – Fixed (L = 1)
    f <- paste0(results_path_cor, "cor_small_sample_susie", n, "_h", h, "_L1.RData")
    if (file.exists(f)) {
      load(f)
      temp <- temp[temp$n_effect == 1, ]
      df_list[[length(df_list) + 1]] <-
        cal_sum_stat(temp, "Servin & Stephens SER", n, h, "Fixed")
    }

    # SS-SER – Estimated
    f <- paste0(results_path_cor, "cor_small_sample_susie", n, "_h", h, ".RData")
    if (file.exists(f)) {
      load(f)
      temp <- temp[temp$n_effect == 1, ]
      df_list[[length(df_list) + 1]] <-
        cal_sum_stat(temp, "Servin & Stephens SER", n, h, "Estimated")
    }
  }
}

df_out <- do.call(rbind, df_list)

# ============================================================
# Labellers  (H already in percent)
# ============================================================
fig6_labeller <- labeller(
  N = function(x) paste0("n = ", x),
  H = function(x) paste0("h² = ", x, "%")
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
# Fig – Coverage by Method / Type
# ============================================================
fig_type <- ggplot(df_out,
                   aes(x      = Method,
                       y      = obs_cov,
                       colour = Method,
                       shape  = Type,
                       group  = Type)) +
  geom_point(position = position_dodge(width = 0.3), size = 2.5) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_grid(H ~ N, labeller = fig6_labeller) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c("Fixed" = 17, "Estimated" = 16)) +
  labs(y = "Coverage", x = "") +
  theme_paper +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

fig_type

ggsave(
  filename =  paste0(getwd(),"/plots/fig6.pdf"),
  plot = fig_type,
  width = 15,
  height = 15
)
