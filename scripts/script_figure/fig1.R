# ============================================================
# Libraries
# ============================================================
library(susieR)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(cowplot)
library(dplyr)
library(tidyr)
library(pROC)
library(purrr)
library(data.table)

# ============================================================
# PANEL A – Row 1–3: Coverage / Power / Purity
# ============================================================

res_path     <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"


cal_sum_stat <- function(temp, method = "Default SER", n = 10, h = 25) {
  obs_cov    <- numeric(4)
  obs_purity <- numeric(4)
  obs_power  <- numeric(4)

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
    obs_power[i]  <- sum(temp$n_true_cs[idx_all], na.rm = TRUE) /
      sum(temp$n_effect[idx_all],  na.rm = TRUE)
  }

  data.frame(
    obs_cov    = obs_cov,
    obs_purity = obs_purity,
    obs_power  = obs_power,
    Method     = factor(method,
                        levels = c("Default SER", "Servin & Stephens SER")),
    N          = n,
    H          = h,
    Effect     = factor(c("1", "2", "3", "4+"),
                        levels = c("1", "2", "3", "4+"))
  )
}

n_values <- c(20, 30, 50, 70)#, 100, 200)
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
df_A   <- df_out[df_out$H == 25, ]

# ============================================================
# PANEL A – Row 4: Coverage vs NCP (single effect, L = 1)
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
    Method   = factor(rep(method, nrow(out)),
                      levels = c("Default SER", "Servin & Stephens SER")),
    N        = rep(N, nrow(out))
  )
}

# NCP loop uses the same   res_path defined above
df_ncp_list <- list()
o <- 1
for (n in n_values) {
  for (h in c(  15, 25, 30, 50, 75)) {
    file_susie <- paste0(res_path,
                         "small_sample_susie", n, "_h", h, "_L1.RData")
    if (file.exists(file_susie)) {
      load(file_susie)
      df_ncp_list[[o]] <- create_NCP_table_stratified(temp,
                                                      method = "Default SER",
                                                      N = n)
      o <- o + 1
    }
    file_csusie <- paste0(res_path,
                          "cor_small_sample_susie", n, "_h", h, "_L1.RData")
    if (file.exists(file_csusie)) {
      load(file_csusie)
      df_ncp_list[[o]] <- create_NCP_table_stratified(temp,
                                                      method = "Servin & Stephens SER",
                                                      N = n)
      o <- o + 1
    }
  }
}

df_ncp <- bind_rows(df_ncp_list)

df_binned <- df_ncp %>%
  mutate(
    log_ncp = log10(NCP),
    bin     = cut_interval(log_ncp, n = 30)
  ) %>%
  group_by(Method, N, n_effect, bin) %>%
  summarise(
    mean_ncp = mean(NCP,      na.rm = TRUE),
    mean_res = mean(response, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(!is.na(mean_ncp))

# ============================================================
# Shared style helpers
# ============================================================

colors_A <- c("Default SER"           = "#1A85FF",
              "Servin & Stephens SER" = "#D41159")

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
# Build the four panel-A rows
# ============================================================

PA_cov <- ggplot(df_A, aes(x = Effect, y = obs_cov, col = Method)) +
  geom_point(size = 3) +
  facet_grid(. ~ N, labeller = n_labeller) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  ylab("Coverage") +
  xlab("") +
  ylim(c(min(df_A$obs_cov, na.rm = TRUE) - 0.02, 1)) +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = TRUE)

PA_pow <- ggplot(df_A, aes(x = Effect, y = obs_power, col = Method)) +
  geom_point(size = 3) +
  facet_grid(. ~ N) +
  scale_y_log10(limits = c(min(df_A$obs_power, na.rm = TRUE), 1),
                breaks  = c(0.01, 0.05, 0.1, 0.25, 0.5, 1)) +
  ylab("Power") +
  xlab("") +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = FALSE)

PA_pur <- ggplot(df_A, aes(x = Effect, y = obs_purity, col = Method)) +
  geom_point(size = 3) +
  facet_grid(. ~ N) +
  ylim(c(0.90, 1)) +
  ylab("Purity") +
  xlab("") +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = FALSE)

PA_ncp <- ggplot(
  df_binned[df_binned$n_effect == 1, ],
  aes(x = mean_ncp, y = mean_res, colour = Method)
) +
  geom_line(linewidth = 0.8) +
  geom_point(alpha = 0.6) +

  facet_grid(. ~ N) +
  scale_x_log10(lim=c(2.3, 600)) +
  scale_y_log10() +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "NCP (log scale)", y = "Coverage") +
  scale_color_manual(values = colors_A) +
  theme_panelA(show_strip = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stack rows; only top row gets strip labels
P_perf <- plot_grid(PA_cov, PA_pow, PA_pur, PA_ncp,
                    ncol  = 1,
                    align = "v",
                    axis  = "lr")

# ============================================================
# Panels B / C / D  (unchanged)
# ============================================================

colors <- c("#1A85FF" ,"#D41159"  )

your_path  <- "C:/Document/Serieux/Travail/Package/susie_small_sample/data_gao/"
small_data<- readRDS(paste0(your_path,
                             "MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"))

y <- small_data$ENSG00000151694$residual_Y[[3]]
X <- small_data$ENSG00000151694$residual_X[[3]]

res_susie       <- susieR::susie(X = X, y = y, L = 20)
res_susie_small = susieR::susie(X = X,y=y,L=10,
                                estimate_residual_method ="Servin_Stephens" , tol = 0.0000001 ,
                                alpha0 = 1/sqrt(nrow(X) ),
                                beta0 = 1/sqrt(nrow(X) ))

# ---- Panel C ----
df_plot_pred <- data.frame(
  y   = rep(y, 2),
  x   = c(predict(res_susie_small, X), predict(res_susie, X)),
  col = factor(c(
    rep("SS SER",      length(predict(res_susie_small, X))),
    rep("default SER", length(predict(res_susie,       X)))
  ))
)

P_pred <- ggplot(df_plot_pred, aes(y = y, x = x, col = col)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  ylab("Normalized gene expression") +
  xlab("Predicted gene expression") +
  theme_cowplot() +
  theme(legend.position  = "none",
        panel.grid.major = element_line(color = "gray80")) +
  scale_color_manual(values = colors,
                     name   = "SuSiE:",
                     labels = c("Default SER", "Servin and Stephens SER"))

# ---- Panel B helper ----
make_pip_plot <- function(model, title_str) {
  color_cycle <- c("dodgerblue2", "green4", "#6A3D9A", "#FF7F00", "gold1",
                   "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "#FDBF6F",
                   "gray70", "khaki2", "maroon", "orchid1", "deeppink1",
                   "blue1", "steelblue4", "darkturquoise", "green1",
                   "yellow4", "yellow3", "darkorange4", "brown")
  max_cs <- 400
  p      <- model$pip
  pos    <- seq_along(p)
  b      <- rep(0, length(p))
  df     <- data.frame(pos = pos, p = p, b = b)

  gg <- ggplot(df, aes(x = pos, y = p)) +
    geom_point(aes(color = b != 0)) +
    scale_color_manual(values = c("black", "red")) +
    labs(x = "SNP", y = "PIP") +
    theme_cowplot() +
    theme(legend.position  = "none",
          panel.grid.major = element_line(color = "gray80"))

  for (i in rev(seq_len(nrow(model$alpha)))) {
    if (!is.null(model$sets$cs_index) && !(i %in% model$sets$cs_index)) next
    if (susieR:::n_in_CS(model, model$sets$requested_coverage)[i] < max_cs) {
      x0 <- intersect(pos,
                      which(susieR:::in_CS(model,
                                           model$sets$requested_coverage)[i, ] > 0))
      y1 <- p[x0]
    } else {
      x0 <- NULL; y1 <- NULL
    }
    if (is.null(x0)) next
    gg <- gg +
      geom_point(data  = data.frame(pos = x0, p = y1),
                 aes(x = pos, y = p),
                 color = head(color_cycle, 1),
                 shape = 1, stroke = 1.4, size = 2)
    color_cycle <- c(color_cycle[-1], color_cycle[1])
  }

  gg + ggtitle(title_str) +
    theme(plot.title = element_text(size = 16, face = "plain"))
}

pip_plot_susie       <- make_pip_plot(res_susie,       "SuSiE default SER, n = 75")
pip_plot_susie_small <- make_pip_plot(res_susie_small, "SuSiE Servin & Stephens SER")


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/permutation_Lopes.RData")
# ---- Panel D ----
mat <-as.matrix(table(res[,1], res[,4]))

mat[1,1]=0

mat[1,1]=0
df  <- as.data.frame(as.table(mat))
colnames(df) <- c("SuSiE", "SuSiE_SS", "n_genes")

# Keep numeric axes
df$SuSiE_SS <- as.integer(as.character(df$SuSiE_SS))
df$SuSiE    <- as.integer(as.character(df$SuSiE))

# Drop zeros (no bubble to draw)
df <- df[df$n_genes > 0, ]
P_Miga <- ggplot(df, aes(x = SuSiE, y = SuSiE_SS, size = n_genes)) +
  geom_point(color = "lightblue3", alpha = 0.85) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", linewidth = 0.6) +
  scale_size_area(
    max_size = 10,
    breaks   = c(10,   100, 500, 1000),
    name     = "Number of genes"
  ) +
  scale_x_continuous(breaks = 0:max(df$SuSiE)) +
   scale_y_continuous(
    breaks = 0:max(df$SuSiE_SS),
    expand = expansion(mult = c(0.07 , 0.05))  # 5% bottom, 10% top
  )+
  labs(x = "CSs Default SER", y = "CSs SS SER") +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(size = guide_legend(
    title.position = "left",
    nrow           = 1,
    label.position = "right"))

# ============================================================
# Final assembly
# ============================================================

legend_plot      <- get_legend(P_pred + theme(legend.position = "bottom"))
legend_plot_miga <- get_legend(P_Miga + theme(legend.position = "bottom"))

grid_plot <- ggdraw() +
  draw_plot(P_perf,
            x = 0.00, y = 0.00, width = 0.47, height = 1.00) +
  draw_plot(pip_plot_susie + theme(legend.position = "none"),
            x = 0.48, y = 0.50, width = 0.25, height = 0.50) +
  draw_plot(P_pred,
            x = 0.48, y = 0.10, width = 0.25, height = 0.35) +
  draw_plot(legend_plot,
            x = 0.50, y = 0.00, width = 0.25, height = 0.10) +
  draw_plot(pip_plot_susie_small +
              theme(legend.position = "none") +
              ylim(c(0, 1)),
            x = 0.75, y = 0.50, width = 0.25, height = 0.50) +
  draw_plot(P_Miga + theme(legend.position = "none"),
            x = 0.75, y = 0.10, width = 0.25, height = 0.35) +
  draw_plot(legend_plot_miga,
            x = 0.77, y = 0.00, width = 0.25, height = 0.10) +
  draw_label("A", fontface = "bold", size = 20,
             x = 0.01, y = 0.99, vjust = 1) +
  draw_label("B", fontface = "bold", size = 20,
             x = 0.50, y = 0.99, vjust = 1) +
  draw_label("C", fontface = "bold", size = 20,
             x = 0.50, y = 0.48, vjust = 1) +
  draw_label("D", fontface = "bold", size = 20,
             x = 0.75, y = 0.48, vjust = 1)

Pfinal= plot_grid(grid_plot)
ggsave(
  filename = paste0(getwd(),"/plots/fig1.pdf"),
  plot     = Pfinal,
  width    = 20,
  height   = 10
)
