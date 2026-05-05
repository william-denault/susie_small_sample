# ============================================================
# Export all data needed to reproduce Figure 1
# ============================================================
library(susieR)
library(ggplot2)   # cut_interval
library(dplyr)

# ── Paths (resolve against repo root, with Downloads fallback for MiGA) ──
repo_root <- (function() {
  p <- normalizePath(".", mustWork = TRUE)
  while (p != "/" && !file.exists(file.path(p, "scripts/script_figure/generate_RDS_for_fig1.R"))) {
    p <- dirname(p)
  }
  if (p == "/") stop("could not locate repo root from getwd()=", getwd())
  p
})()

res_path   <- file.path(repo_root, "simulations/")
perm_path  <- file.path(repo_root, "gwd_results/permutation_Lopes.rds")
out_rds    <- file.path(repo_root, "plots/fig1_data.rds")

# MiGA univariate file is large data, not committed to the repo.
# Kept locally at ~/Documents/susie_small/ for re-runs.
miga_candidates <- c(
  path.expand("~/Documents/susie_small/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"),
  file.path(repo_root, "data_gao/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"),
  "/home/gw/Downloads/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"
)
miga_file <- miga_candidates[file.exists(miga_candidates)][1]
if (is.na(miga_file)) stop("MiGA univariate file not found in: ", paste(miga_candidates, collapse = "\n  "))
cat("MiGA file:", miga_file, "\n")

# ============================================================
# Panel A rows 1–3: Coverage / Power / Purity
# ============================================================

cal_sum_stat <- function(temp, method = "Default SER", n = 10, h = 25) {
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
      sum(temp$n_cs[idx_no_dummy],      na.rm = TRUE)
    obs_purity[i] <- mean(temp$purity[idx_no_dummy],   na.rm = TRUE)
    obs_power[i]  <- sum(temp$n_true_cs[idx_all],      na.rm = TRUE) /
      sum(temp$n_effect[idx_all],        na.rm = TRUE)
  }
  data.frame(
    obs_cov    = obs_cov,
    obs_purity = obs_purity,
    obs_power  = obs_power,
    Method     = factor(method, levels = c("Default SER", "SS-SER")),
    N = n, H = h,
    Effect = factor(c("1","2","3","4+"), levels = c("1","2","3","4+"))
  )
}

n_values <- c(20, 30, 50, 70)
h_values <- c(10, 15, 25, 30, 50, 75)

df_list <- list()
for (n in n_values) {
  for (h in h_values) {
    configs <- list(
      list(f = paste0("small_sample_susie",     n, "_h", h, "_L1.rds"), m = "Default SER"),
      list(f = paste0("cor_small_sample_susie", n, "_h", h, "_L1.rds"), m = "SS-SER")
    )
    for (conf in configs) {
      fp <- paste0(res_path, conf$f)
      if (file.exists(fp)) {
        temp <- readRDS(fp)
        df_list[[length(df_list) + 1]] <- cal_sum_stat(temp, conf$m, n, h)
      } else warning(paste("Missing:", fp))
    }
  }
}

df_out <- do.call(rbind, df_list)
df_A   <- df_out[df_out$H == 25, ]

# ============================================================
# Panel A row 4: Coverage vs NCP
# ============================================================

create_NCP_table_stratified <- function(temp, method = "Default SER", N = 10) {
  if (length(which(temp$is.dummy == 0)) > 0) temp <- temp[temp$is.dummy == 0, ]
  out <- list(); o <- 1
  for (i in 1:nrow(temp)) {
    cur <- temp$n_effect[i]
    if (temp$n_cs[i] == 1) {
      out[[o]] <- c(temp$n_true_cs[i], temp$n_cs[i], temp[i, ncol(temp)], cur); o <- o+1
    } else {
      for (k in seq_len(temp$n_true_cs[i])) { out[[o]] <- c(1,1,temp[i,ncol(temp)],cur); o<-o+1 }
      nf <- temp$n_cs[i] - temp$n_true_cs[i]
      if (nf > 0) for (k in seq_len(nf)) { out[[o]] <- c(0,1,temp[i,ncol(temp)],cur); o<-o+1 }
    }
  }
  out <- do.call(rbind, out)
  data.frame(
    response = out[,1], NCP = out[,3]/out[,4], n_effect = out[,4],
    Method   = factor(rep(method, nrow(out)), levels = c("Default SER","SS-SER")),
    N        = rep(N, nrow(out))
  )
}

df_ncp_list <- list(); o <- 1
for (n in n_values) {
  for (h in c(15, 25, 30, 50, 75)) {
    for (cfg in list(
      list(f = paste0(res_path, "small_sample_susie",     n, "_h", h, "_L1.rds"), m = "Default SER"),
      list(f = paste0(res_path, "cor_small_sample_susie", n, "_h", h, "_L1.rds"), m = "SS-SER")
    )) {
      if (file.exists(cfg$f)) {
        temp <- readRDS(cfg$f)
        df_ncp_list[[o]] <- create_NCP_table_stratified(temp, cfg$m, n); o <- o+1
      }
    }
  }
}

df_ncp <- bind_rows(df_ncp_list)

df_binned <- df_ncp %>%
  mutate(log_ncp = log10(NCP), bin = cut_interval(log_ncp, n = 30)) %>%
  group_by(Method, N, n_effect, bin) %>%
  summarise(mean_ncp = mean(NCP, na.rm=TRUE), mean_res = mean(response, na.rm=TRUE), .groups="drop") %>%
  filter(!is.na(mean_ncp))

# ============================================================
# Panels B & C: MiGA eQTL models
# ============================================================

small_data     <- readRDS(miga_file)
y              <- small_data$ENSG00000151694$residual_Y[[3]]
X              <- small_data$ENSG00000151694$residual_X[[3]]

res_susie       <- susieR::susie(X = X, y = y, L = 20)
res_susie_small <- susieR::susie(X = X, y = y, L = 10,
                                 estimate_residual_method = "NIG",
                                 tol = 1e-7)

df_plot_pred <- data.frame(
  y   = rep(y, 2),
  x   = c(predict(res_susie_small, X), predict(res_susie, X)),
  col = factor(c(rep("SS-SER", length(y)), rep("Default SER", length(y))))
)

# ============================================================
# Panel D: MiGA permutation bubble data
# ============================================================

res        <- readRDS(perm_path)
mat        <- as.matrix(table(res[,1], res[,4]))
mat[1,1]   <- 0
df_bubble  <- as.data.frame(as.table(mat))
colnames(df_bubble) <- c("SuSiE", "SuSiE_SS", "n_genes")
df_bubble$SuSiE    <- as.integer(as.character(df_bubble$SuSiE))
df_bubble$SuSiE_SS <- as.integer(as.character(df_bubble$SuSiE_SS))
df_bubble  <- df_bubble[df_bubble$n_genes > 0, ]

# ============================================================
# Bundle and save
# ============================================================

fig1_data <- list(
  # Panel A
  df_A             = df_A,
  df_binned        = df_binned,
  # Panels B & C
  res_susie        = res_susie,
  res_susie_small  = res_susie_small,
  df_plot_pred     = df_plot_pred,
  # Panel D
  df_bubble        = df_bubble
)

saveRDS(fig1_data, file = out_rds)
cat("Saved to:", out_rds, "\n")
