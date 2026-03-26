library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(gridExtra)

colors <- c("#1A85FF","#D41159",  "darkgreen")

# ── helper: expand rows, one row per CS ──────────────────────────────────────
create_NCP_table <- function(temp, method = "Default SER", N = 10,
                             effect_filter = NULL) {
  if (length(which(temp$is.dummy == 0)) > 0) {
    keep <- temp$is.dummy == 0
    if (!is.null(effect_filter))
      keep <- keep & temp$n_effect == effect_filter
    temp <- temp[keep, ]
  }

  # guard: nothing survived the filter
  if (nrow(temp) == 0) return(NULL)

  out <- list(); o <- 1
  for (i in seq_len(nrow(temp))) {
    if (temp$n_cs[i] == 1) {
      out[[o]] <- c(temp$n_true_cs[i], 1, temp[i, ncol(temp)]); o <- o + 1
    } else if (temp$n_true_cs[i] == temp$n_cs[i]) {
      for (k in seq_len(temp$n_true_cs[i])) {
        out[[o]] <- c(1, 1, temp[i, ncol(temp)]); o <- o + 1
      }
    } else {
      for (k in seq_len(temp$n_true_cs[i])) {
        out[[o]] <- c(1, 1, temp[i, ncol(temp)]); o <- o + 1
      }
      for (k in seq_len(temp$n_cs[i] - temp$n_true_cs[i])) {
        out[[o]] <- c(0, 1, temp[i, ncol(temp)]); o <- o + 1
      }
    }
  }

  out <- do.call(rbind, out)
  data.frame(
    response = out[, 1],
    NCP      = out[, 3],
    Method   = factor(rep(method, nrow(out))),
    N        = rep(N, nrow(out))
  )
}

# ── shared bin-and-summarise helper ──────────────────────────────────────────
bin_data <- function(df, n_bins = 30) {
  df %>%
    mutate(
      log_ncp = log10(NCP),
      bin     = cut_interval(log_ncp, n = n_bins)
    ) %>%
    group_by(Method, N, bin) %>%
    summarise(
      mean_ncp = mean(NCP,      na.rm = TRUE),
      mean_res = mean(response, na.rm = TRUE),
      n_bin    = n(),
      se       = sqrt(mean_res * (1 - mean_res) / n_bin),
      .groups  = "drop"
    )
}

# ── shared plot helper ────────────────────────────────────────────────────────
ncp_plot <- function(df_binned, title) {
  ggplot(df_binned,
         aes(x = mean_ncp, y = mean_res,
             colour = Method, fill = Method, group = Method)) +
    geom_ribbon(
      aes(ymin = pmax(0, mean_res - 1.96 * se),
          ymax = pmin(1, mean_res + 1.96 * se)),
      alpha = 0.15, colour = NA
    ) +
    geom_line() +
    geom_point(size = 1.2) +
    facet_grid(. ~ N) +
    scale_x_log10() +
    geom_hline(yintercept = 0.95, linetype = "dashed") +
    labs(x = "NCP (log-spaced bins)",
         y = "P(CS contains causal SNP)",
         title = title) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values  = colors) +
    theme_bw(base_size = 13) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 10))
}

# ─────────────────────────────────────────────────────────────────────────────
# PARAMETERS
# ─────────────────────────────────────────────────────────────────────────────
n_values <- c(10, 20, 30, 50, 70, 100)
h_values <- c(10, 15, 25, 30, 50, 75)
res_path <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

configs_base <- list(
  list(prefix = "small_sample_susie",     method = "Default SER"),
  list(prefix = "cor_small_sample_susie", method = "Servin & Stephens SER"),
  list(prefix = "tbf_sample_susie",       method = "tbf")
)

# ── generic loader for datasets 1 & 2 ────────────────────────────────────────
load_all <- function(suffix, effect_filter = NULL) {
  df_list <- list()
  for (n in n_values) {
    for (h in h_values) {
      for (cfg in configs_base) {
        fp <- paste0(res_path, cfg$prefix, n, "_h", h, suffix, ".RData")
        if (file.exists(fp)) {
          load(fp)   # loads 'temp'
          res <- create_NCP_table(temp, method = cfg$method, N = n,
                                  effect_filter = effect_filter)
          if (!is.null(res))
            df_list[[length(df_list) + 1]] <- res
        } else {
          warning("Missing: ", fp)
        }
      }
    }
  }
  do.call(rbind, df_list)
}

# ── Dataset 1: all effects, L estimated ──────────────────────────────────────
df_out1 <- load_all(suffix = "")
P1 <- ncp_plot(
  bin_data(df_out1, n_bins = 30),
  "P(CS contains causal SNP) vs NCP\nL estimated, all effect counts"
)

# ── Dataset 2: n_effect == 1 only, L estimated ───────────────────────────────
df_out2 <- load_all(suffix = "", effect_filter = 1)
P2 <- ncp_plot(
  bin_data(df_out2, n_bins = 50),
  "P(CS contains causal SNP) vs NCP\nL estimated, n_effect = 1"
)

# ── Dataset 3: L fixed = 1 (separate file suffix, different tbf prefix) ──────
configs_L1 <- list(
  list(prefix = "small_sample_susie",     method = "Default SER"),
  list(prefix = "cor_small_sample_susie", method = "Servin & Stephens SER"),
  list(prefix = "tbf_small_sample_susie", method = "tbf")   # note: tbf_ prefix differs
)

df_out3_list <- list()
for (n in n_values) {
  for (h in h_values) {
    for (cfg in configs_L1) {
      fp <- paste0(res_path, cfg$prefix, n, "_h", h, "_L1.RData")
      if (file.exists(fp)) {
        load(fp)   # loads 'temp'
        res <- create_NCP_table(temp, method = cfg$method, N = n)
        if (!is.null(res))
          df_out3_list[[length(df_out3_list) + 1]] <- res
      } else {
        warning("Missing: ", fp)
      }
    }
  }
}
df_out3 <- do.call(rbind, df_out3_list)
P3 <- ncp_plot(
  bin_data(df_out3, n_bins = 30),
  "P(CS contains causal SNP) vs NCP\nL fixed = 1, n_effect = 1"
)

grid.arrange(P1, P2, P3, nrow = 3)
