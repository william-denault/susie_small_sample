library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)


# Function to calculate ROC
simple_roc <- function(labs, scores) {
  labs <- labs[order(scores, decreasing = TRUE)]
  data.frame(TPR = cumsum(labs) / sum(labs), FPR = cumsum(!labs) / sum(!labs), labs)
}

#### n=10 ----

n=10
h2=25

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P11 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P11



h2=3

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P12 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P12





#### Pb here ----

h2=5

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P13 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P13




h2=75

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P14 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P14





#### n=20 ----

n=20
h2=25

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P21 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P21



h2=3

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P22 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P22





#### PB here ----

h2=5

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P23 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P23




h2=75

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P24 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P24









#### n=30 ----

n=30
h2=25

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P31 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P31



h2=3

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P32 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P32







h2=5

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P33 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P33




h2=75

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P34 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P34





#### n=50 ----

n=50
h2=25

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P41 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P41



h2=3

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P42 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P42







h2=5

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P43 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P43




h2=75

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P44 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P44






#### n=75 ----

n=75
h2=25

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P51 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P51



h2=3

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P52 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P52





### PB hezree ----

h2=5

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P53 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P53




h2=75

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P54 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P54







#### n=100 ----

n=100
h2=25

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P61 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P61


#### Pb here
h2=3

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P62 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P62







h2=5

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P63 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P63




h2=75

pip_obs_g <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))
labs_g <- pip_obs_g[, 1]
score_g <- pip_obs_g[, 2]
roc_g <- simple_roc(labs_g, score_g)
colnames(roc_g)[1:2]= c("TPR", "FPR")
# Load data for SER SS
pip_obs_ss <- fread(paste0("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))
labs_ss <- pip_obs_ss[, 1]
score_ss <- pip_obs_ss[, 2]
roc_ss <- simple_roc(labs_ss, score_ss)
colnames(roc_ss)[1:2]= c("TPR", "FPR")
# Create a ggplot for the current combination of n and h2
P64 <- ggplot() +
  geom_line(data = roc_g, aes(x =  FPR, y =  TPR), color = "blue", linewidth = 1, linetype = "solid") +
  geom_line(data = roc_ss, aes(x =  FPR, y =  TPR), color = "red", linewidth = 1, linetype = "solid") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(0, 0.2) +
  ylim(0,.6) +
  labs(
    title = paste0("n = ", n, ", h2 = ", ifelse(h2 == 3 | h2 == 5, h2 * 10, h2), "%"),
    x = "FPR",
    y = "TPR"
  ) +
  theme_minimal()


P64




library(gridExtra)

P_power = grid.arrange(P11, P21, P31,P41,P51,P61,
                          P12, P22, P32,P42,P52,P62,
                          P13, P23, P33,P43,P53,P63,
                          P14, P24, P34,P44,P54,P64,
                          ncol=6)


ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_power.pdf",
       plot =
         P_power  ,
       width = 320,
       height = 210,
       units = "mm")

