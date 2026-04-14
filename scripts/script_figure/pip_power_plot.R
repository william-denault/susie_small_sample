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

colors <- c( "#1A85FF","#D41159","darkgreen" )
# -----------------------------
# PARAMETERS
# -----------------------------

n_values <- c(  20, 30, 50, 70, 100,200)
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

    file_tbf <- paste0(results_path_susie, "pip_tbf_n",n,"_h" ,h, ".csv")
    if (file.exists(file_cor) & file.exists(file_susie)) {

      df_cor   <- compute_roc_df(file_cor,   "Servin and Stephens SER", n, h)
      df_susie <- compute_roc_df(file_susie, "Default SER",     n, h)
     # df_tbf  <- compute_roc_df( file_tbf, "tbf", n, h)
      combined <- rbind(df_cor, df_susie  )

      # Save individual plot
      p <- ggplot(combined, aes(FPR, TPR, color=Method)) +
        geom_line(size=1) +
        geom_abline(linetype="dashed") +
        theme_bw(base_size = 14) +
        ggtitle(paste0("ROC Curve - N=", n, "  h=", h)) +
        theme(legend.position="bottom")

      # ggsave(
      #   filename = paste0("ROC_N", n, "_h", h, ".pdf"),
      #   plot = p,
      #   width = 6,
      #   height = 5
      # )

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
  geom_line(linewidth=0.5) +
  # 1. Update the labeller to change "N:" to "N=" and "h:" to "h²="
  facet_grid(N ~ h, labeller = labeller(
    N =  as_labeller(c(`10` = 'N=10', `20` = 'N=20', `30` = 'N=30', `50` = 'N=50', `70` = 'N=70', `100` = 'N=100',`200` = 'N=200'),
                     default = label_value),
    h = as_labeller(c(`0.1` = 'h²=0.1', `0.15` = 'h²=0.15', `0.25` = 'h²=0.25',
                      `0.3` = 'h²=0.3', `0.5` = 'h²=0.5', `0.75` = 'h²=0.75'),
                    default = label_value)
  )) +
  # Using the simpler `label_both` for N as you requested "N=", which it produces.
  # For 'h', we need to manually define the mapping to get the h² symbol.
  # If `label_both` isn't working for 'N' due to your R version, you can manually define it like 'h':
  # N = as_labeller(c(`10` = 'N=10', `20` = 'N=20', `30` = 'N=30', `50` = 'N=50', `70` = 'N=70', `100` = 'N=100'))

  theme_bw(base_size = 13) +
  xlim(c(0,.5))+
  theme(
    legend.position="bottom",
    # 2. Change grey box to white
    strip.background = element_rect(fill = "white", color = "white"),
    # Optional: ensure text is black and clean
    strip.text = element_text(size=10, color = "black")
  ) +
  scale_color_manual(values = colors) +  ggtitle("ROC Curves Across Sample Sizes and Heritabilities")

# Save the plot
ggsave(
  filename =  "ROC_updated_labels.png",
  plot = final_plot,
  width = 20,
  height = 10
)


final_plot <- ggplot(final_df,
                     aes(FPR, TPR, color=Method)) +
  geom_line(size=0.9) +
  xlim(c(0,.5))+
  geom_abline(linetype="dashed") +
  facet_grid(N ~ h, labeller = label_both) +
  theme_bw(base_size = 13) +
  theme(
    legend.position="bottom",
    strip.text = element_text(size=10)
  ) +
  scale_color_manual(values =colors)+
  ggtitle("ROC Curves Across Sample Sizes and Heritabilities")

ggsave(
  filename =  "ROC.png" ,
  plot = final_plot,
  width = 20,
  height = 10
)





