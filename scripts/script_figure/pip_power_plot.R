library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
 library(data.table)

library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)

# -----------------------------
# PARAMETERS
# -----------------------------

n_values <- c(  20, 30, 50, 70, 100)
h_values <- c(.1 , .15, .25, .3 , .5 , .75)

# Paths where your simulation outputs are saved
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

# -----------------------------
# FUNCTION TO LOAD + COMPUTE ROC
# -----------------------------

compute_roc_df <- function(file_path, method_name, N, h){

  mat <- fread(file_path)  # change to load() if needed

  truth <- c(unlist((mat[,1])))
  score <- c(unlist((mat[,2])))

  roc_obj <- roc(truth, score, quiet=TRUE)

  df <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Method = method_name,
    N = N,
    h = h
  )

  return(df)
}

# -----------------------------
# LOOP OVER ALL COMBINATIONS
# -----------------------------

all_roc <- list()

tt <- 1

for (n in n_values) {
  for (h in h_values) {

    file_cor   <- paste0(results_path_cor,   "pip_corsusie_n",n,"_h" ,h, ".csv")
    file_susie <- paste0(results_path_susie, "pip_susie_n",n,"_h" ,h, ".csv")

    if (file.exists(file_cor) & file.exists(file_susie)) {

      df_cor   <- compute_roc_df(file_cor,   "cor_susie", n, h)
      df_susie <- compute_roc_df(file_susie, "susie",     n, h)

      combined <- rbind(df_cor, df_susie)

      # Save individual plot
      p <- ggplot(combined, aes(FPR, TPR, color=Method)) +
        geom_line(size=1) +
        geom_abline(linetype="dashed") +
        theme_bw(base_size = 14) +
        ggtitle(paste0("ROC Curve - N=", n, "  h=", h)) +
        theme(legend.position="bottom")

      ggsave(
        filename = paste0("ROC_N", n, "_h", h, ".pdf"),
        plot = p,
        width = 6,
        height = 5
      )

      all_roc[[length(all_roc)+1]] <- combined
    }

    tt <- tt + 1
  }
}

# -----------------------------
# FINAL COMBINED PLOT
# -----------------------------

final_df <- do.call(rbind,(all_roc))

final_plot <- ggplot(final_df,
                     aes(FPR, TPR, color=Method)) +
  geom_line(size=0.9) +
  xlim(c(0,.2))+
  geom_abline(linetype="dashed") +
  facet_grid(N ~ h, labeller = label_both) +
  theme_bw(base_size = 13) +
  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  ) +
  ggtitle("ROC Curves Across Sample Sizes and Heritabilities")

