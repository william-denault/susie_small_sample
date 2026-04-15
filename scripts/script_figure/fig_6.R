library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

results_path_susie <- "C:/Document/Serieux/Travail/Data_analysis_and_papers/revision_susie_small_sample/simulations/"
colors <- c("#1A85FF","#D41159" )

# --- Execution Loop ---
n_values <- c(   20, 30, 50, 70,100,200); h_values <- c( 10, 15, 25, 30, 50, 75)
df_out <- list()
o=1
for (n in n_values) {
  for (h in h_values) {
    # 1. Default Susie
    f1 <- paste0(results_path_susie, "small_sample_susie", n, "_h", h, "_unfilter.RData")
    load(f1)

    t0 =do.call(rbind, temp)

    susie_cov=  mean(mean(t0 [ -which(is.na(t0 [,4])),1]))

    # 2. Servin & Stephens
    f2 <- paste0(results_path_susie, "cor_small_sample_susie", n, "_h", h, "_unfilter_L1.RData")
    load(f2)
    cor_susie_cov=  mean(do.call(rbind, temp)[,1])
    df_out[[o]] = data.frame(cov= c(susie_cov,cor_susie_cov),
                             n= rep(n, 2),
                             h= rep(h, 2),
                             Method= factor(c("Default SER" ,
                             "SS SER" )))
    o=o+1

  }
}

df_final <- bind_rows(df_out)

# --- Visualization ---
ggplot(df_final, aes(x = Method, y =  cov, color = Method   )) +
  geom_point(   size = 2) +
  facet_grid(n ~ h ) +
  geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
  theme_bw(base_size = 12) +
  scale_color_manual(values = colors) +
  labs(
       y = "Observed Coverage", x = "SER")


