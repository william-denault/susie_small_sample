library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)

# ============================================================
# Paths & colors
# ============================================================
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

colors <- c(
  "Default SER" = "#1A85FF",
  "SS SER"      = "#D41159"
)

# ============================================================
# Parameters
# ============================================================
n_values <- c(20, 30, 50, 70, 100, 200)
h_values <- c(10, 15, 25, 30, 50, 75)

# ============================================================
# Load data
# ============================================================
df_list <- list()

for (n in n_values) {
  for (h in h_values) {

    f1 <- paste0(results_path_susie, "small_sample_susie", n, "_h", h, "_unfilter.RData")
    f2 <- paste0(results_path_susie, "cor_small_sample_susie", n, "_h", h, "_unfilter_L1.RData")

    if (file.exists(f1) && file.exists(f2)) {
      load(f1)
      t0         <- do.call(rbind, temp)
      susie_cov  <- mean(t0[-which(is.na(t0[, 4])), 1])

      load(f2)
      cor_cov    <- mean(do.call(rbind, temp)[, 1])

      df_list[[length(df_list) + 1]] <- data.frame(
        cov    = c(susie_cov, cor_cov),
        n      = rep(n, 2),
        h      = rep(h, 2),
        Method = factor(c("Default SER", "SS SER"),
                        levels = c("Default SER", "SS SER"))
      )
    } else {
      warning(paste("Missing files for n =", n, ", h =", h))
    }
  }
}

df_final <- bind_rows(df_list)

# ============================================================
# Labellers  (h already in percent)
# ============================================================
unfilter_labeller <- labeller(
  n = function(x) paste0("N = ", x),
  h = function(x) paste0("h² = ", x, "%")
)

# ============================================================
# Shared theme
# ============================================================
theme_paper <- theme_cowplot() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    strip.background = element_blank(),
    strip.text       = element_text(size = 15, face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    axis.title       = element_text(size = 15),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  )

# ============================================================
# Plot
# ============================================================
fig_unfilter <- ggplot(df_final,
                       aes(x = Method, y = cov, colour = Method)) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_grid(n ~ h, labeller = unfilter_labeller) +
  scale_color_manual(values = colors) +
  labs(y = "Coverage", x = "") +
  theme_paper

fig_unfilter
# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  "coverage_figure.pdf",
  plot   = p,
  width  = 8,    # inches – adjust to column width of your journal
  height = 6,
  device = "pdf"
)


