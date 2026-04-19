# ============================================================
# Libraries
# ============================================================
library(pROC)
library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)

# ============================================================
# Paths & colors
# ============================================================
res_path     <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"


colors <- c(
  "Default SER"           = "#1A85FF",
  "Servin & Stephens SER" = "#D41159",
  "darkgreen"             = "darkgreen"   # keep slot if needed
)

# ============================================================
# Helper: summarise one loaded 'temp' object
# ============================================================
cal_sum_stat <- function(temp, method = "Default SER", n = 10, h = 25) {
  obs_cov    <- numeric(4)
  obs_purity <- numeric(4)
  obs_power  <- numeric(4)
  obs_cs_size  <- numeric(4)

  for (i in 1:4) {
    if (i < 4) {
      idx_no_dummy <- temp$n_effect == i & temp$is.dummy == 0
      idx_all      <- temp$n_effect == i
    } else {
      idx_no_dummy <- temp$n_effect >= i & temp$is.dummy == 0
      idx_all      <- temp$n_effect >= i
    }
    obs_cov[i]    <- sum(temp$n_true_cs[idx_no_dummy], na.rm = TRUE) /
      sum(temp$n_cs[idx_no_dummy],      na.rm = TRUE)
    obs_purity[i] <- mean(temp$purity[idx_no_dummy], na.rm = TRUE)

    obs_cs_size[i] <- mean(temp$mean_cs_size [idx_no_dummy], na.rm = TRUE)
    obs_power[i]  <- sum(temp$n_true_cs[idx_all],    na.rm = TRUE) /
      sum(temp$n_effect[idx_all],        na.rm = TRUE)
  }

  data.frame(
    obs_cov    = obs_cov,
    obs_purity = obs_purity,
    obs_power  = obs_power,

    obs_cs_size=obs_cs_size,
    Method     = factor(method,
                        levels = c("Default SER", "Servin & Stephens SER")),
    N          = n,
    H          = h,
    Effect     = factor(c("1", "2", "3", "4+"),
                        levels = c("1", "2", "3", "4+"))
  )
}

# ============================================================
# Load data
# ============================================================
n_values <- c(20, 30, 50, 70, 100, 200)
h_values <- c(10, 15, 25, 30, 50, 75)

df_list <- list()
for (n in n_values) {
  for (h in h_values) {
    configs <- list(
      list(f = paste0("small_sample_susie", n, "_h", h, "_L1.RData"),
           m = "Default SER"),
      list(f = paste0(
                      "cor_small_sample_susie", n, "_h", h, "_L1.RData"),
           m = "Servin & Stephens SER")
    )
    for (conf in configs) {
      full_path <- paste0(res_path, conf$f)
      if (file.exists(full_path)) {
        load(full_path)
        df_list[[length(df_list) + 1]] <-
          cal_sum_stat(temp, method = conf$m, n = n, h = h)
      } else {
        warning(paste("File missing:", full_path))
      }
    }
  }
}

df_out <- do.call(rbind, df_list)

# ============================================================
# Shared style
# ============================================================
n_labeller <- labeller(
  N = function(x) paste0("N = ", x),
  H = function(x) paste0("h² = ", x, "%")
)
theme_paper <- theme_cowplot() +
  theme(
    panel.grid.major  = element_line(color = "gray80"),
    strip.background  = element_blank(),
    strip.text        = element_text(size = 15, face = "bold"),
    legend.position   = "none",
    legend.title      = element_blank(),
    axis.title        = element_text(size = 15)
  )

# ============================================================
# Fig 2 – Coverage
# ============================================================
fig2 <- ggplot(df_out,
               aes(x = Effect, y = obs_cov, col = Method)) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_grid(H ~ N, labeller = n_labeller) +
  scale_color_manual(values = colors) +
  ylim(c(min(df_out$obs_cov, na.rm = TRUE) - 0.02, 1)) +
  labs(y = "Coverage", x = "Number of effects") +
  theme_paper

fig2

ggsave(
  filename =  paste0(getwd(),"/plots/fig2.pdf"),
  plot = fig2,
  width = 15,
  height = 10
)
# ============================================================
# Fig 3 – CS size  (add cs_size column if present in temp)
# ============================================================
# Uncomment and adapt once cs_size is in df_out
# fig3 <- ggplot(df_out, aes(x = Effect, y = obs_cs_size, col = Method)) + ...

fig3 <- ggplot(df_out,
               aes(x = Effect, y = obs_cs_size, col = Method)) +
  geom_point(size = 2.5) +
  facet_grid(H ~ N, labeller = n_labeller) +
  scale_color_manual(values = colors) +
  #ylim(c(min(df_out$obs_cs_size, na.rm = TRUE) - 0.02, 1)) +
  labs(y = "CS size", x = "Number of effects") +
  theme_paper

fig3

ggsave(
  filename =  paste0(getwd(),"/plots/fig3.pdf"),
  plot = fig3,
  width = 15,
  height = 10
)
# ============================================================
# Fig 4 – Purity
# ============================================================
fig4 <- ggplot(df_out,
               aes(x = Effect, y = obs_purity, col = Method)) +
  geom_point(size = 2.5) +
  facet_grid(H ~ N, labeller = n_labeller) +
  scale_color_manual(values = colors) +
  ylim(c(0.90, 1)) +
  labs(y = "Purity", x = "Number of effects") +
  theme_paper

fig4
ggsave(
  filename =  paste0(getwd(),"/plots/fig4.pdf"),
  plot = fig4,
  width = 15,
  height = 10
)
# ============================================================
# Fig 5 – Power  (log scale, matching paper)
# ============================================================
fig5 <- ggplot(df_out,
               aes(x = Effect, y = obs_power, col = Method)) +
  geom_point(size = 2.5) +
  scale_y_log10(
    limits = c(max(min(df_out$obs_power, na.rm = TRUE), 1e-3), 1),
    breaks = c(0.01, 0.05, 0.10, 0.25, 0.50, 1.00)
  ) +
  facet_grid(H ~ N, labeller = n_labeller) +
  scale_color_manual(values = colors) +
  labs(y = "Power", x = "Number of effects") +
  theme_paper

fig5
ggsave(
  filename =  paste0(getwd(),"/plots/fig5.pdf"),
  plot = fig5,
  width = 15,
  height = 10
)
