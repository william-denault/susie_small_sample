library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)

# ============================================================
# Paths & colors
# ============================================================
dir_cor_smal       <- "ss_sqrt/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_cor   <- paste0(results_path_susie, dir_cor_smal)

colors <- c(
  "Default SER"           = "#1A85FF",
  "Servin & Stephens SER" = "#D41159"
)

# ============================================================
# Parameters
# ============================================================
n_values <- c(20, 30, 50, 70, 100, 200)
h_values <- c(15, 25, 30, 50, 75)

# ============================================================
# Helper
# ============================================================
create_NCP_table_stratified <- function(temp, method = "Default SER", N = 10) {
  if (length(which(temp$is.dummy == 0)) > 0) {
    temp <- temp[which(temp$is.dummy == 0), ]
  }

  out <- list()
  o   <- 1

  for (i in 1:nrow(temp)) {
    current_n_eff <- temp$n_effect[i]

    if (temp$n_cs[i] == 1) {
      out[[o]] <- c(temp$n_true_cs[i], temp$n_cs[i],
                    temp[i, ncol(temp)], current_n_eff)
      o <- o + 1
    } else {
      for (k in seq_len(temp$n_true_cs[i])) {
        out[[o]] <- c(1, 1, temp[i, ncol(temp)], current_n_eff)
        o <- o + 1
      }
      nfalse <- temp$n_cs[i] - temp$n_true_cs[i]
      if (nfalse > 0) {
        for (k in seq_len(nfalse)) {
          out[[o]] <- c(0, 1, temp[i, ncol(temp)], current_n_eff)
          o <- o + 1
        }
      }
    }
  }

  out <- do.call(rbind, out)
  data.frame(
    response = out[, 1],
    NCP      = out[, 3] / out[, 4],
    n_effect = out[, 4],
    Method   = factor(method,
                      levels = c("Default SER", "Servin & Stephens SER")),
    N        = N
  )
}

# ============================================================
# Load data
# ============================================================
df_list <- list()

for (n in n_values) {
  for (h in h_values) {
    file_susie  <- paste0(results_path_susie, "small_sample_susie", n, "_h", h, "_L1.RData")
    file_csusie <- paste0(results_path_cor,   "cor_small_sample_susie", n, "_h", h, "_L1.RData")

    if (file.exists(file_susie)) {
      load(file_susie)
      df_list[[length(df_list) + 1]] <-
        create_NCP_table_stratified(temp, method = "Default SER", N = n)
    }
    if (file.exists(file_csusie)) {
      load(file_csusie)
      df_list[[length(df_list) + 1]] <-
        create_NCP_table_stratified(temp, method = "Servin & Stephens SER", N = n)
    }
  }
}

df_out1 <- bind_rows(df_list)

# ============================================================
# Bin & summarise
# ============================================================
df_binned <- df_out1 %>%
  filter(n_effect <= 4) %>%
  mutate(
    log_ncp = log10(NCP),
    bin     = cut_interval(log_ncp, n = 10)
  ) %>%
  group_by(Method, N, n_effect, bin) %>%
  summarise(
    mean_ncp = mean(NCP,      na.rm = TRUE),
    mean_res = mean(response, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(!is.na(mean_ncp))

# ============================================================
# Labellers
# ============================================================
ncp_labeller <- labeller(
  N        = function(x) paste0("n = ", x),
  n_effect = function(x) paste0("n effect = ", x)
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
# Fig 6 – CS Coverage vs NCP
# ============================================================
fig_ncp <- ggplot(df_binned,
                  aes(x = mean_ncp, y = mean_res, colour = Method)) +
  geom_line(linewidth = 0.8) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_grid(n_effect ~ N, labeller = ncp_labeller) +
  scale_x_log10(limits = c(0.1, 600)) +
  scale_y_log10() +
  scale_color_manual(values = colors) +
  labs(
    x = "NCP (log scale)",
    y = "Coverage"
  ) +
  theme_paper

fig_ncp


ggsave(
  filename =  paste0(getwd(),"/plots/fig10_ncp_strat.pdf"),
  plot = fig_ncp,
  width =15,
  height = 10
)
