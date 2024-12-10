library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

# Function to calculate ROC
simple_roc <- function(labs, scores) {
  labs <- labs[order(scores, decreasing = TRUE)]
  data.frame(TPR = cumsum(labs) / sum(labs), FPR = cumsum(!labs) / sum(!labs), labs)
}

# Parameters
h2_values <- c(25, 3, 5, 75)
n_values <- c(10, 20, 30, 50, 75, 100)

# Initialize a list to store plots
plot_list <- list()

# Loop over n and h2 values
for (h2 in h2_values) {
  for (n in n_values) {
    # Load data for SER Gaus
    pip_obs_g <- fread(paste0("../susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
    labs_g <- pip_obs_g[, 1]
    score_g <- pip_obs_g[, 2]
    roc_g <- simple_roc(labs_g, score_g)
    colnames(roc_g)[1:2]= c("TPR", "FPR")
    # Load data for SER SS
    pip_obs_ss <- fread(paste0("../susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
    labs_ss <- pip_obs_ss[, 1]
    score_ss <- pip_obs_ss[, 2]
    roc_ss <- simple_roc(labs_ss, score_ss)
    colnames(roc_ss)[1:2]= c("TPR", "FPR")
    # Create a ggplot for the current combination of n and h2
    p <- ggplot() +
      geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", size = 1, linetype = "solid") +
      geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", size = 1, linetype = "solid") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      xlim(0, 0.2) +
      ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()

# Add the plot to the list
plot_list[[paste(h2, n)]] <- p
  }
}# Dummy plot to create a consistent legend
df_dummy <- data.frame(
  x = c(1:4, 1:4),
  y = c(1:4, 1:4),
  lab = factor(rep(c("SER Gaus", "SER SS"), each = 4))
)

p0 <- ggplot(df_dummy, aes(x = x, y = y, colour = lab)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red"), name = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Extract the legend
legend <-  get_legend(p0 )

# Combine plots and add legend
final_plot <- plot_grid(plotlist = plot_list, ncol = length(n_values))
final_plot_with_legend <- plot_grid(final_plot, legend, ncol = 1, rel_heights = c(10, 1))

# Save the final plot
ggsave("roc_grid_plot_with_legend2.png", final_plot_with_legend, width = 20, height = 15)
