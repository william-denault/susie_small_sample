library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(PRROC)

library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)

colors <- c( "#D41159","#1A85FF" )
# -----------------------------
# PARAMETERS
# -----------------------------

n_values <- c( 10, 20, 30, 50, 70, 100)
h_values <- c(.1 , .15, .25, .3 , .5 , .75)

# Paths where your simulation outputs are saved
results_path_cor   <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"

# -----------------------------
# FUNCTION TO LOAD + COMPUTE ROC
# -----------------------------
compute_pr_df <- function(file_path, method_name, N, h){

  mat <- as.matrix(fread(file_path))

  truth <- as.numeric(mat[,1])
  score <- as.numeric(mat[,2])

  # Separate positive and negative scores
  scores_pos <- score[truth == 1]
  scores_neg <- score[truth == 0]

  pr <-PRROC::pr.curve(
    scores.class0 = scores_pos,
    scores.class1 = scores_neg,
    curve = TRUE
  )

  df <- data.frame(
    Recall = pr$curve[,1],
    Precision = pr$curve[,2],
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

      df_cor   <- compute_pr_df(file_cor,   "Servin and Stephens SER", n, h)
      df_susie <- compute_pr_df(file_susie, "Default SER", n, h)


      combined <- rbind(df_cor, df_susie)

      # Save individual plot
      p <- ggplot(combined, aes(Recall, Precision, color=Method)) +
        geom_line(size=1) +
        theme_bw(base_size = 14) +
        ggtitle(paste0("PR Curve - N=", n, "  h=", h)) +
        theme(legend.position="bottom")



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
                     aes(Recall, log10(Precision+0.0001), color=Method)) +
  geom_line(linewidth=0.5) +
  facet_grid(N ~ h, labeller = label_both) +
  theme_bw(base_size = 13) +
  xlim(c(0,.2))+
  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  ) +
  scale_color_manual(values = colors) +
  ggtitle("Precision–Recall Curves Across Sample Sizes and Heritabilities")

ggsave(
  filename =  "RRC_N.png" ,
  plot = final_plot,
  width = 20,
  height = 10
)
