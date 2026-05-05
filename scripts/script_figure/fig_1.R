# =============================================================================
# fig_1.R — Generate Figure 1 (single self-contained script).
#
# Run from anywhere inside the repo:
#     Rscript scripts/script_figure/fig_1.R
#
# Inputs (resolved automatically):
#   - simulations/*.rds                (Coverage / Power / Purity / NCP)
#   - gwd_results/permutation_Lopes.rds (Panel D bubble counts)
#   - MiGA univariate data (~/Documents/susie_small/ ...)
#
# Output:
#   - plots/fig1_data.rds   (cached data bundle; delete to force a rebuild)
#   - plots/fig1.pdf        (final figure)
# =============================================================================

suppressPackageStartupMessages({
  library(susieR)
  library(ggplot2)
  library(cowplot)
  library(gridExtra)
  library(grid)
  library(ggpubr)
  library(dplyr)
})

# ── Paths ──────────────────────────────────────────────────────────────────
repo_root <- (function() {
  p <- normalizePath(".", mustWork = TRUE)
  while (p != "/" && !dir.exists(file.path(p, "scripts/script_figure"))) {
    p <- dirname(p)
  }
  if (p == "/") stop("could not locate repo root from getwd()=", getwd())
  p
})()

res_path  <- file.path(repo_root, "simulations/")
perm_path <- file.path(repo_root, "gwd_results/permutation_Lopes.rds")
out_rds   <- file.path(repo_root, "plots/fig1_data.rds")
out_pdf   <- file.path(repo_root, "plots/fig1.pdf")

miga_candidates <- c(
  path.expand("~/Documents/susie_small/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"),
  file.path(repo_root, "data_gao/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds")
)

# =============================================================================
# Step 1 — build (or load cached) data bundle
# =============================================================================

build_fig1_data <- function() {

  # ── Panel A rows 1–3: Coverage / Power / Purity ──
  cal_sum_stat <- function(temp, method, n, h) {
    obs_cov <- obs_purity <- obs_power <- numeric(4)
    for (i in 1:4) {
      if (i < 4) {
        idx_no_dummy <- temp$n_effect == i & temp$is.dummy == 0
        idx_all      <- temp$n_effect == i
      } else {
        idx_no_dummy <- temp$n_effect >= i & temp$is.dummy == 0
        idx_all      <- temp$n_effect >= i
      }
      obs_cov[i]    <- sum(temp$n_true_cs[idx_no_dummy], na.rm = TRUE) /
        sum(temp$n_cs[idx_no_dummy], na.rm = TRUE)
      obs_purity[i] <- mean(temp$purity[idx_no_dummy], na.rm = TRUE)
      obs_power[i]  <- sum(temp$n_true_cs[idx_all], na.rm = TRUE) /
        sum(temp$n_effect[idx_all], na.rm = TRUE)
    }
    data.frame(
      obs_cov    = obs_cov,
      obs_purity = obs_purity,
      obs_power  = obs_power,
      Method     = factor(method, levels = c("SER", "SS-SER")),
      N = n, H = h,
      Effect     = factor(c("1","2","3","4+"), levels = c("1","2","3","4+"))
    )
  }

  n_values <- c(20, 30, 50, 70)
  h_values <- c(10, 15, 25, 30, 50, 75)

  df_list <- list()
  for (n in n_values) {
    for (h in h_values) {
      configs <- list(
        list(f = paste0("small_sample_susie",     n, "_h", h, "_L1.rds"), m = "SER"),
        list(f = paste0("cor_small_sample_susie", n, "_h", h, "_L1.rds"), m = "SS-SER")
      )
      for (conf in configs) {
        fp <- paste0(res_path, conf$f)
        if (file.exists(fp)) {
          df_list[[length(df_list) + 1]] <- cal_sum_stat(readRDS(fp), conf$m, n, h)
        } else {
          warning("Missing: ", fp)
        }
      }
    }
  }
  df_A <- do.call(rbind, df_list)
  df_A <- df_A[df_A$H == 25, ]

  # ── Panel A row 4: Coverage vs NCP (single-effect) ──
  ncp_table <- function(temp, method, N) {
    if (any(temp$is.dummy == 0)) temp <- temp[temp$is.dummy == 0, ]
    out <- list(); o <- 1
    for (i in seq_len(nrow(temp))) {
      cur <- temp$n_effect[i]
      if (temp$n_cs[i] == 1) {
        out[[o]] <- c(temp$n_true_cs[i], temp$n_cs[i], temp[i, ncol(temp)], cur); o <- o + 1
      } else {
        for (k in seq_len(temp$n_true_cs[i])) {
          out[[o]] <- c(1, 1, temp[i, ncol(temp)], cur); o <- o + 1
        }
        nf <- temp$n_cs[i] - temp$n_true_cs[i]
        if (nf > 0) for (k in seq_len(nf)) {
          out[[o]] <- c(0, 1, temp[i, ncol(temp)], cur); o <- o + 1
        }
      }
    }
    out <- do.call(rbind, out)
    data.frame(
      response = out[,1],
      NCP      = out[,3] / out[,4],
      n_effect = out[,4],
      Method   = factor(method, levels = c("SER", "SS-SER")),
      N        = N
    )
  }

  df_ncp_list <- list(); o <- 1
  for (n in n_values) {
    for (h in c(15, 25, 30, 50, 75)) {
      cfgs <- list(
        list(f = paste0(res_path, "small_sample_susie",     n, "_h", h, "_L1.rds"), m = "SER"),
        list(f = paste0(res_path, "cor_small_sample_susie", n, "_h", h, "_L1.rds"), m = "SS-SER")
      )
      for (cfg in cfgs) {
        if (file.exists(cfg$f)) {
          df_ncp_list[[o]] <- ncp_table(readRDS(cfg$f), cfg$m, n)
          o <- o + 1
        }
      }
    }
  }
  df_ncp <- bind_rows(df_ncp_list)

  df_binned <- df_ncp %>%
    mutate(log_ncp = log10(NCP), bin = cut_interval(log_ncp, n = 30)) %>%
    group_by(Method, N, n_effect, bin) %>%
    summarise(mean_ncp = mean(NCP, na.rm = TRUE),
              mean_res = mean(response, na.rm = TRUE),
              .groups  = "drop") %>%
    filter(!is.na(mean_ncp))

  # ── Panels B & C: MiGA eQTL fits ──
  miga_file <- miga_candidates[file.exists(miga_candidates)][1]
  if (is.na(miga_file)) {
    stop("MiGA univariate file not found in any of:\n  ",
         paste(miga_candidates, collapse = "\n  "))
  }
  message("MiGA file: ", miga_file)

  small_data <- readRDS(miga_file)
  y <- small_data$ENSG00000151694$residual_Y[[3]]
  X <- small_data$ENSG00000151694$residual_X[[3]]

  res_susie       <- susieR::susie(X = X, y = y, L = 20)
  res_susie_small <- susieR::susie(X = X, y = y, L = 10,
                                   estimate_residual_method = "NIG",
                                   tol = 1e-7)

  df_plot_pred <- data.frame(
    y   = rep(y, 2),
    x   = c(predict(res_susie_small, X), predict(res_susie, X)),
    col = factor(c(rep("SS-SER", length(y)), rep("SER", length(y))),
                 levels = c("SER", "SS-SER"))
  )

  # ── Panel D: MiGA permutation bubble ──
  res <- readRDS(perm_path)
  mat <- as.matrix(table(res[, 1], res[, 4]))
  mat[1, 1] <- 0
  df_bubble <- as.data.frame(as.table(mat))
  colnames(df_bubble) <- c("SuSiE", "SuSiE_SS", "n_genes")
  df_bubble$SuSiE    <- as.integer(as.character(df_bubble$SuSiE))
  df_bubble$SuSiE_SS <- as.integer(as.character(df_bubble$SuSiE_SS))
  df_bubble <- df_bubble[df_bubble$n_genes > 0, ]

  list(df_A            = df_A,
       df_binned       = df_binned,
       res_susie       = res_susie,
       res_susie_small = res_susie_small,
       df_plot_pred    = df_plot_pred,
       df_bubble       = df_bubble)
}

if (file.exists(out_rds)) {
  message("Loading cached ", out_rds, " (delete it to force a rebuild)")
  d <- readRDS(out_rds)
} else {
  message("Building data bundle ...")
  d <- build_fig1_data()
  saveRDS(d, out_rds)
  message("Saved ", out_rds)
}
list2env(d, envir = environment())

# =============================================================================
# Step 2 — shared style
# =============================================================================

method1        <- "SS-SER"
method2        <- "SER"
methods_colors <- setNames(c("#D41159", "#1A85FF"), c(method1, method2))
legend_labels  <- c("SS-SER" = "SS-SER (Normal-Inverse-Gamma prior)",
                    "SER"    = "SER")
# Per-method alpha so red sits a touch stronger than blue.
methods_alpha  <- c("SS-SER" = 0.9, "SER" = 0.8)

# Panel A & C dot style: gray "halo" beneath each colored dot.
dot_size_bg     <- 5      # halo
dot_alpha_bg    <- 0.6
dot_size_color  <- 3.8    # colored dot on top
dodge_w         <- 0.35   # space between methods at each Effect level

perf_theme <- theme_cowplot(font_size = 16) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(size = 1, color = "black"),
    axis.ticks         = element_line(size = 1, color = "black"),
    axis.ticks.length  = unit(0.25, "cm"),
    plot.margin        = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
    axis.text          = element_text(size = 14, face = "bold"),
    axis.title         = element_text(size = 16, face = "bold"),
    plot.title         = element_text(size = 16, face = "bold"),
    strip.background   = element_blank(),
    strip.text         = element_blank()
  )

n_labeller <- labeller(N = function(x) paste0("N = ", x))

# =============================================================================
# Step 3 — Panel A
# =============================================================================

pos_d <- position_dodge(width = dodge_w)

PA_cov <- ggplot(df_A, aes(x = Effect, y = obs_cov, col = Method, group = Method)) +
  geom_point(color = "gray85", size = dot_size_bg, alpha = dot_alpha_bg, position = pos_d) +
  geom_point(aes(alpha = Method), size = dot_size_color, position = pos_d) +
  facet_grid(. ~ N, labeller = n_labeller) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black", size = 1) +
  ylim(c(min(df_A$obs_cov, na.rm = TRUE) - 0.02, 1)) +
  scale_color_manual(values = methods_colors) +
  scale_alpha_manual(values = methods_alpha, guide = "none") +
  ylab("Coverage") + xlab("") +
  perf_theme +
  theme(strip.text = element_text(size = 16, face = "bold"))

PA_pow <- ggplot(df_A, aes(x = Effect, y = obs_power, col = Method, group = Method)) +
  geom_point(color = "gray85", size = dot_size_bg, alpha = dot_alpha_bg, position = pos_d) +
  geom_point(aes(alpha = Method), size = dot_size_color, position = pos_d) +
  facet_grid(. ~ N) +
  scale_y_log10(limits = c(min(df_A$obs_power, na.rm = TRUE), 1),
                breaks  = c(0.01, 0.05, 0.1, 0.25, 0.5, 1)) +
  scale_color_manual(values = methods_colors) +
  scale_alpha_manual(values = methods_alpha, guide = "none") +
  ylab("Power") + xlab("") +
  perf_theme

PA_pur <- ggplot(df_A, aes(x = Effect, y = obs_purity, col = Method, group = Method)) +
  geom_point(color = "gray85", size = dot_size_bg, alpha = dot_alpha_bg, position = pos_d) +
  geom_point(aes(alpha = Method), size = dot_size_color, position = pos_d) +
  facet_grid(. ~ N) +
  scale_y_continuous(limits = c(0.93, 0.97),
                     breaks = c(0.93, 0.94, 0.95, 0.96, 0.97)) +
  scale_color_manual(values = methods_colors) +
  scale_alpha_manual(values = methods_alpha, guide = "none") +
  ylab("Purity") + xlab("") +
  perf_theme

PA_ncp <- ggplot(df_binned[df_binned$n_effect == 1, ],
                 aes(x = mean_ncp, y = mean_res, colour = Method)) +
  geom_point(color = "gray85", size = dot_size_bg, alpha = dot_alpha_bg) +
  geom_point(aes(alpha = Method), size = dot_size_color) +
  facet_grid(. ~ N) +
  scale_x_log10(limits = c(3, 250),
                breaks = c(3, 10, 30, 100, 250)) +
  scale_y_log10(limits = c(0.4, 1.0),
                breaks = c(0.4, 0.5, 0.65, 0.8, 1.0),
                labels = c("0.4", "0.5", "0.65", "0.8", "1.0")) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = methods_colors) +
  scale_alpha_manual(values = methods_alpha, guide = "none") +
  labs(x = "NCP (log scale)", y = "Coverage") +
  perf_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

P_perf <- plot_grid(PA_cov, PA_pow, PA_pur, PA_ncp,
                    ncol = 1, align = "v", axis = "lr")

# =============================================================================
# Step 4 — Panel B (PIP plots)
# =============================================================================

cs_palette <- c("dodgerblue2","green4","#6A3D9A","#FF7F00","gold1","skyblue2",
                "#FB9A99","palegreen2","#CAB2D6","#FDBF6F","gray70","khaki2",
                "maroon","orchid1","deeppink1","blue1","steelblue4",
                "darkturquoise","green1","yellow4","yellow3","darkorange4","brown")

# Manual-legend layout (data coords). We draw the legend as annotate() layers
# so it survives cowplot::draw_plot rescaling (built-in inside-legends get
# clipped under cowplot 1.2 in some setups).
panel_b_legend_x   <- 1500   # anchor x for entries (markers + text)
panel_b_legend_y   <- 0.925  # title baseline
panel_b_title_dx   <- -180   # x-offset of the title from the anchor (negative = to the left)
panel_b_title_gap  <- 0.10   # extra space between title and first entry
panel_b_line_step  <- 0.07   # vertical spacing between entries
panel_b_marker_dx  <- 90     # x-offset of marker from anchor
panel_b_text_dx    <- 280    # x-offset of label from anchor (text sits well to the right of marker)
panel_b_marker_size <- 2.5   # circle size in legend

panel_b_theme <- theme_cowplot(font_size = 20) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_line(color = "gray85", size = 0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(size = 14, face = "bold"),
    axis.title         = element_text(size = 16, face = "bold"),
    plot.margin        = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
    axis.line          = element_line(size = 1.0, color = "black"),
    axis.ticks         = element_line(size = 1.0, color = "black"),
    axis.ticks.length  = unit(0.3, "cm"),
    plot.title         = element_text(size = 14, face = "bold"),
    panel.border       = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA)
  )

# PIP plot with susie_plot-style compressed CS legend:
#   singleton CS:    "L<idx>: C=1"
#   multi-variant:   "L<idx>: C=<size>/R=<purity>"   (R = min.abs.corr)
create_pip_plot <- function(model, title_text, max_cs = 400) {
  p   <- model$pip
  pos <- seq_along(p)

  cs_names <- names(model$sets$cs)
  cs_index <- model$sets$cs_index
  keep <- vapply(seq_along(cs_names),
                 function(k) length(model$sets$cs[[k]]) < max_cs,
                 logical(1))
  cs_names <- cs_names[keep]
  cs_index <- if (!is.null(cs_index)) cs_index[keep] else seq_along(cs_names)

  legend_levels <- character(0)
  cs_membership <- rep(NA_character_, length(p))

  if (length(cs_names) > 0) {
    purity <- model$sets$purity[cs_names, "min.abs.corr"]
    for (k in seq_along(cs_names)) {
      idx   <- model$sets$cs[[cs_names[k]]]
      label <- if (length(idx) == 1) {
        sprintf("L%d: C=1", cs_index[k])
      } else {
        sprintf("L%d: C=%d/R=%g", cs_index[k], length(idx), round(purity[k], 4))
      }
      legend_levels      <- c(legend_levels, label)
      cs_membership[idx] <- label
    }
  }

  df    <- data.frame(pos = pos, p = p, cs = cs_membership)
  df_cs <- df[!is.na(df$cs), ]
  df_cs$cs <- factor(df_cs$cs, levels = legend_levels)
  pal <- setNames(cs_palette[seq_along(legend_levels)], legend_levels)

  gg <- ggplot(df, aes(x = pos, y = p)) +
    geom_point(color = "black", size = 3, alpha = 0.8) +
    geom_point(data = df_cs, aes(x = pos, y = p, color = cs),
               shape = 1, stroke = 2.5, size = 5) +
    scale_color_manual(values = pal, guide = "none") +
    scale_y_continuous(limits = c(0, 1.05),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "SNP", y = "PIP", title = title_text) +
    panel_b_theme

  # Manual legend as annotation layers — part of the panel data, so cowplot
  # cannot drop them.
  gg <- gg + annotate("text",
                      x = panel_b_legend_x + panel_b_title_dx,
                      y = panel_b_legend_y,
                      label = "95% Credible Sets",
                      hjust = 0, vjust = 1, size = 4.0, fontface = "bold")

  if (length(legend_levels) > 0) {
    for (k in seq_along(legend_levels)) {
      yk <- panel_b_legend_y - panel_b_title_gap - panel_b_line_step * (k - 1)
      gg <- gg +
        annotate("point",
                 x = panel_b_legend_x + panel_b_marker_dx, y = yk,
                 color = pal[k], shape = 1, stroke = 1.8, size = panel_b_marker_size) +
        annotate("text",
                 x = panel_b_legend_x + panel_b_text_dx, y = yk,
                 label = legend_levels[k],
                 hjust = 0, vjust = 0.5, size = 3.4)
    }
  }

  gg
}

pip_plot_susie       <- create_pip_plot(res_susie,       "SuSiE with SER, N = 75")
pip_plot_susie_small <- create_pip_plot(res_susie_small, "SuSiE with SS-SER, N = 75")

# =============================================================================
# Step 5 — Panel C (predicted vs observed)
# =============================================================================

n_pts <- nrow(df_plot_pred) / 2
y_obs <- df_plot_pred$y[seq_len(n_pts)]
x_ss  <- df_plot_pred$x[df_plot_pred$col == method1][seq_len(n_pts)]
x_def <- df_plot_pred$x[df_plot_pred$col == method2][seq_len(n_pts)]

var_explained_small   <- 1 - var(y_obs - x_ss)  / var(y_obs)
var_explained_default <- 1 - var(y_obs - x_def) / var(y_obs)

message(sprintf("SS-SER R^2: %.3f", var_explained_small))
message(sprintf("SER R^2:    %.3f", var_explained_default))

P_pred <- ggplot(df_plot_pred, aes(y = y, x = x, col = col)) +
  geom_point(data = df_plot_pred, aes(y = y, x = x),
             color = "gray85", size = 5, alpha = 0.6, inherit.aes = FALSE) +
  geom_point(size = 3.8, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "black", size = 1.0, alpha = 0.7) +
  ylab("Observed gene expression") +
  xlab("Predicted gene expression") +
  theme_cowplot(font_size = 14) +
  theme(
    legend.position    = "none",
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "gray85", size = 0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(size = 14, face = "bold"),
    axis.title         = element_text(size = 16, face = "bold"),
    plot.margin        = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
    axis.line          = element_line(size = 1.0, color = "black"),
    axis.ticks         = element_line(size = 1.0, color = "black"),
    axis.ticks.length  = unit(0.3, "cm"),
    panel.border       = element_blank()
  ) +
  scale_color_manual(values = methods_colors,
                     labels = legend_labels,
                     name   = "") +
  # R^2 labels parked between y=0.5 and y=1.0 (above the diagonal-cluster, below the top)
  annotate("text",
           x = max(df_plot_pred$x) - 0.35 * diff(range(df_plot_pred$x)),
           y = 0.90,
           label = paste0("'SS-SER ' * R^2 == ", round(var_explained_small, 3)),
           color = methods_colors[method1],
           size  = 5, fontface = "bold", hjust = 1, parse = TRUE) +
  annotate("text",
           x = max(df_plot_pred$x) - 0.35 * diff(range(df_plot_pred$x)),
           y = 0.70,
           label = paste0("'SER ' * R^2 == ", round(var_explained_default, 3)),
           color = methods_colors[method2],
           size  = 5, fontface = "bold", hjust = 1, parse = TRUE)

# =============================================================================
# Step 6 — Panel D (bubble)
# =============================================================================

# Bin counts to compress the long upper tail and dramatize the asymmetry.
# All gene counts are preserved (binning aggregates, never truncates).
#   SER (x):   0, 1, 2, 3, 4, 5+
#   SS-SER (y): 0, 1, 2, 3+
df_bubble_binned <- df_bubble |>
  within({
    x_bin <- cut(SuSiE,    breaks = c(-1, 0, 1, 2, 3, 4, Inf),
                            labels = c("0","1","2","3","4","5+"), right = TRUE)
    y_bin <- cut(SuSiE_SS, breaks = c(-1, 0, 1, 2, Inf),
                            labels = c("0","1","2","3+"), right = TRUE)
  })
df_bubble_binned <- aggregate(n_genes ~ x_bin + y_bin, df_bubble_binned, sum)
df_bubble_binned$side <- factor(
  ifelse(as.integer(df_bubble_binned$x_bin) >  as.integer(df_bubble_binned$y_bin),
         "SS-SER fewer CSs",
  ifelse(as.integer(df_bubble_binned$x_bin) <  as.integer(df_bubble_binned$y_bin),
         "SS-SER more CSs", "equal")),
  levels = c("SS-SER fewer CSs", "equal", "SS-SER more CSs"))

side_pal <- c("SS-SER fewer CSs" = "#1A85FF",
              "equal"            = "gray60",
              "SS-SER more CSs"  = "#D41159")

P_Miga <- ggplot(df_bubble_binned, aes(x = x_bin, y = y_bin, size = n_genes)) +
  geom_point(color = "gray85", size = 2.4, alpha = 0.6) +
  geom_point(aes(color = side), alpha = 0.6) +
  geom_point(aes(color = side), shape = 1, stroke = 1.8) +
  scale_color_manual(values = side_pal, guide = "none") +
  scale_size_area(
    name     = "Gene \ncounts",
    breaks   = c(50, 100, 500, 1000),
    labels   = c(50, 100, 500, 1000),
    max_size = 12
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  expand_limits(x = 0.5, y = 0.5) +
  labs(x = "CSs SER", y = "CSs SS-SER") +
  theme_cowplot(font_size = 14) +
  theme(
    legend.position    = "right",
    panel.grid.major.y = element_line(color = "gray85", size = 0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(size = 14, face = "bold"),
    axis.title         = element_text(size = 16, face = "bold"),
    plot.margin        = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
    axis.line          = element_line(size = 1.0, color = "black"),
    axis.ticks         = element_line(size = 1.0, color = "black"),
    axis.ticks.length  = unit(0.3, "cm"),
    plot.title         = element_text(size = 20, face = "bold"),
    panel.border       = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA)
  )

# =============================================================================
# Step 7 — final assembly + save
# =============================================================================

legend_plot      <- get_legend(P_pred + theme(legend.position = "bottom"))
legend_plot_miga <- get_legend(P_Miga + theme(legend.position = "bottom"))

grid_plot <- ggdraw() +
  draw_plot(P_perf,
            x = 0.00, y = 0.00, width = 0.42, height = 1.00) +
  draw_plot(pip_plot_susie + theme(legend.position = "none"),
            x = 0.48, y = 0.57, width = 0.24, height = 0.40) +
  draw_plot(P_pred,
            x = 0.48, y = 0.10, width = 0.24, height = 0.40) +
  draw_plot(legend_plot,
            x = 0.48, y = 0.01, width = 0.24, height = 0.08) +
  draw_plot(pip_plot_susie_small + theme(legend.position = "none"),
            x = 0.73, y = 0.57, width = 0.24, height = 0.40) +
  draw_plot(P_Miga + theme(legend.position = "none"),
            x = 0.74, y = 0.10, width = 0.24, height = 0.40) +
  draw_plot(legend_plot_miga,
            x = 0.77, y = 0.01, width = 0.24, height = 0.08) +
  draw_label("A", fontface = "bold", size = 30, x = 0.01, y = 0.99, vjust = 1) +
  draw_label("B", fontface = "bold", size = 30, x = 0.49, y = 0.99, vjust = 1) +
  draw_label("C", fontface = "bold", size = 30, x = 0.49, y = 0.55, vjust = 1) +
  draw_label("D", fontface = "bold", size = 30, x = 0.74, y = 0.55, vjust = 1)

ggsave(out_pdf, plot = grid_plot,
       width = 450, height = 250, units = "mm", bg = "white")
message("Saved ", out_pdf)
