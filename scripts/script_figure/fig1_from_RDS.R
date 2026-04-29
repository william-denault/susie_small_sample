fig1_data <- readRDS("C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/plots/fig1_data.rds")


# ============================================================
# Reproduce Figure 1 from fig1_data.rds
# ============================================================
library(susieR)
library(ggplot2)
library(cowplot)
library(ggpubr)

rds_path <- paste0(getwd(), "/plots/fig1_data.rds")
out_pdf  <- paste0(getwd(), "/plots/fig1.pdf")

d <- readRDS(rds_path)
list2env(d, envir = environment())   # unpack: df_A, df_binned, etc.

# ============================================================
# Shared style
# ============================================================

colors_A <- c("Default SER" = "#1A85FF", "Servin & Stephens SER" = "#D41159")
colors_C <- c("#1A85FF", "#D41159")
n_labeller <- labeller(N = function(x) paste0("N = ", x))

theme_panelA <- function(show_strip = TRUE) {
  base <- theme_cowplot() +
    theme(
      legend.position  = "none",
      panel.grid.major = element_line(color = "gray80"),
      strip.background = element_blank(),
      strip.text       = element_text(size = 12, face = "bold")
    )
  if (!show_strip) base <- base + theme(strip.text = element_blank())
  base
}

# ============================================================
# Panel A
# ============================================================

PA_cov <- ggplot(df_A, aes(x = Effect, y = obs_cov, col = Method)) +
  geom_point(size = 3) +
  facet_grid(. ~ N, labeller = n_labeller) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  ylab("Coverage") + xlab("") +
  ylim(c(min(df_A$obs_cov, na.rm = TRUE) - 0.02, 1)) +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = TRUE)

PA_pow <- ggplot(df_A, aes(x = Effect, y = obs_power, col = Method)) +
  geom_point(size = 3) +
  facet_grid(. ~ N) +
  scale_y_log10(limits = c(min(df_A$obs_power, na.rm = TRUE), 1),
                breaks  = c(0.01, 0.05, 0.1, 0.25, 0.5, 1)) +
  ylab("Power") + xlab("") +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = FALSE)

PA_pur <- ggplot(df_A, aes(x = Effect, y = obs_purity, col = Method)) +
  geom_point(size = 3) +
  facet_grid(. ~ N) +
  ylim(c(0.90, 1)) +
  ylab("Purity") + xlab("") +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = FALSE)

PA_ncp <- ggplot(df_binned[df_binned$n_effect == 1, ],
                 aes(x = mean_ncp, y = mean_res, colour = Method)) +
  geom_line(linewidth = 0.8) + geom_point(alpha = 0.6) +
  facet_grid(. ~ N) +
  scale_x_log10(lim = c(2.3, 600)) + scale_y_log10() +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "NCP (log scale)", y = "Coverage") +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

P_perf <- plot_grid(PA_cov, PA_pow, PA_pur, PA_ncp, ncol = 1, align = "v", axis = "lr")

# ============================================================
# Panel B: PIP plots
# ============================================================

make_pip_plot <- function(model, title_str) {
  color_cycle <- c("dodgerblue2","green4","#6A3D9A","#FF7F00","gold1",
                   "skyblue2","#FB9A99","palegreen2","#CAB2D6","#FDBF6F",
                   "gray70","khaki2","maroon","orchid1","deeppink1",
                   "blue1","steelblue4","darkturquoise","green1",
                   "yellow4","yellow3","darkorange4","brown")
  max_cs <- 400
  p <- model$pip; pos <- seq_along(p)
  gg <- ggplot(data.frame(pos = pos, p = p), aes(x = pos, y = p)) +
    geom_point(color = "black") +
    labs(x = "SNP", y = "PIP") + theme_cowplot() +
    theme(legend.position = "none", panel.grid.major = element_line(color = "gray80"))
  for (i in rev(seq_len(nrow(model$alpha)))) {
    if (!is.null(model$sets$cs_index) && !(i %in% model$sets$cs_index)) next
    if (susieR:::n_in_CS(model, model$sets$requested_coverage)[i] < max_cs) {
      x0 <- intersect(pos, which(susieR:::in_CS(model, model$sets$requested_coverage)[i,] > 0))
      y1 <- p[x0]
    } else { x0 <- NULL; y1 <- NULL }
    if (is.null(x0)) next
    gg <- gg + geom_point(data = data.frame(pos = x0, p = y1), aes(x = pos, y = p),
                          color = head(color_cycle, 1), shape = 1, stroke = 1.4, size = 2)
    color_cycle <- c(color_cycle[-1], color_cycle[1])
  }
  gg + ggtitle(title_str) + theme(plot.title = element_text(size = 16, face = "plain"))
}

pip_plot_susie       <- make_pip_plot(res_susie,       "SuSiE default SER, n = 75")
pip_plot_susie_small <- make_pip_plot(res_susie_small, "SuSiE Servin & Stephens SER")

# ============================================================
# Panel C: Predicted vs observed
# ============================================================

P_pred <- ggplot(df_plot_pred, aes(y = y, x = x, col = col)) +
  geom_abline(slope = 1, intercept = 0) + geom_point() +
  ylab("Normalized gene expression") + xlab("Predicted gene expression") +
  theme_cowplot() +
  theme(legend.position = "none", panel.grid.major = element_line(color = "gray80")) +
  scale_color_manual(values = colors_C, name = "SuSiE:",
                     labels = c("Default SER", "Servin and Stephens SER"))

# ============================================================
# Panel D: Bubble plot
# ============================================================

P_Miga <- ggplot(df_bubble, aes(x = SuSiE, y = SuSiE_SS, size = n_genes)) +
  geom_point(color = "steelblue", alpha = 0.85) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", linewidth = 0.6) +
  scale_size_area(max_size = 10, breaks = c(10, 100, 500, 1000), name = "Number of genes") +
  scale_x_continuous(breaks = 0:max(df_bubble$SuSiE)) +
  scale_y_continuous(breaks = 0:max(df_bubble$SuSiE_SS),
                     expand = expansion(mult = c(0.07, 0.05))) +
  labs(x = "CSs Default SER", y = "CSs SS SER") +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom", legend.direction = "horizontal") +
  guides(size = guide_legend(title.position = "left", nrow = 1, label.position = "right"))

# ============================================================
# Final assembly
# ============================================================

legend_plot      <- get_legend(P_pred  + theme(legend.position = "bottom"))
legend_plot_miga <- get_legend(P_Miga  + theme(legend.position = "bottom"))

grid_plot <- ggdraw() +
  draw_plot(P_perf,                                         x=0.00, y=0.00, width=0.47, height=1.00) +
  draw_plot(pip_plot_susie + theme(legend.position="none"), x=0.48, y=0.50, width=0.25, height=0.50) +
  draw_plot(P_pred,                                         x=0.48, y=0.10, width=0.25, height=0.35) +
  draw_plot(legend_plot,                                    x=0.50, y=0.00, width=0.25, height=0.10) +
  draw_plot(pip_plot_susie_small + theme(legend.position="none") + ylim(c(0,1)),
            x=0.75, y=0.50, width=0.25, height=0.50) +
  draw_plot(P_Miga + theme(legend.position="none"),         x=0.75, y=0.10, width=0.25, height=0.35) +
  draw_plot(legend_plot_miga,                               x=0.77, y=0.00, width=0.25, height=0.10) +
  draw_label("A", fontface="bold", size=20, x=0.01, y=0.99, vjust=1) +
  draw_label("B", fontface="bold", size=20, x=0.50, y=0.99, vjust=1) +
  draw_label("C", fontface="bold", size=20, x=0.50, y=0.48, vjust=1) +
  draw_label("D", fontface="bold", size=20, x=0.75, y=0.48, vjust=1)

ggsave(filename = out_pdf, plot = plot_grid(grid_plot), width = 20, height = 10)
cat("Saved to:", out_pdf, "\n")
