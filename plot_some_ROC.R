rm(list = ls())

library(ggplot2)
library(dplyr)

simple_roc <- function(labs, scores) {
  labs <- labs[order(scores, decreasing = TRUE)]
  data.frame(TPR = cumsum(labs) / sum(labs), FPR = cumsum(!labs) / sum(!labs), labs)
}

# n = 10, h2 = 25
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n10_h0.25.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n10_h0.25.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n10_h2_25") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 10, h2 = 3
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n10_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n10_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n10_h2_3") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 10, h2 = 5
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n10_h0.5.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n10_h0.5.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n10_h2_5") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 10, h2 = 75
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n10_h0.75.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n10_h0.75.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n10_h2_75") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 20, h2 = 25
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n20_h0.25.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n20_h0.25.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n20_h2_25") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 20, h2 = 3
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n20_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n20_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n20_h2_3") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 20, h2 = 5
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n20_h0.5.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n20_h0.5.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n20_h2_5") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 20, h2 = 75
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n20_h0.75.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n20_h0.75.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n20_h2_75") +
#  geom_abline(intercept = 0, slope = 1, color = "black") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 30, h2 = 25
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n30_h0.25.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n30_h0.25.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n30_h2_25") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))




# Continuation for all combinations

# n = 30, h2 = 3
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n30_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n30_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  ggtitle("n30_h2_3") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 30, h2 = 5
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n30_h0.5.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n30_h0.5.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n30_h2_5") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 30, h2 = 75
# load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n30_h0.75.RData")
# labs <- temp[, 1]
# score <- temp[, 2]
# roc_susie <- simple_roc(labs, score)
# roc_susie$Method <- "SuSiE"

# load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n30_h0.75.RData")
# labs <- temp[, 1]
# score <- temp[, 2]
# roc_corsusie <- simple_roc(labs, score)
# roc_corsusie$Method <- "CorSuSiE"

# roc_data <- rbind(roc_susie, roc_corsusie)
# ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#   geom_line() +
#   ggtitle("n30_h2_75") +
#   theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 50, h2 = 25
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n50_h0.25.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n50_h0.25.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n50_h2_25") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 50, h2 = 3
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n50_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n50_h0.3.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n50_h2_3") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 50, h2 = 5
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n50_h0.5.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n50_h0.5.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n50_h2_5") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 50, h2 = 75
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n50_h0.75.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n50_h0.75.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n50_h2_75") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# n = 75, h2 = 25
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n75_h0.25.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n75_h0.25.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n75_h2_25") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# Continue for all combinations

# n = 75, h2 = 3
#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n75_h0.3.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_susie <- simple_roc(labs, score)
#roc_susie$Method <- "SuSiE"

#load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n75_h0.3.RData")
#labs <- temp[, 1]
#score <- temp[, 2]
#roc_corsusie <- simple_roc(labs, score)
#roc_corsusie$Method <- "CorSuSiE"

#roc_data <- rbind(roc_susie, roc_corsusie)
#ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
#  geom_line() +
#  ggtitle("n75_h2_3") +
#  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))

# Follow same for (n = 75, h2 = 5), (n = 75, h2 = 75), (n = 100, h2 = 25), (n = 100, h2 = 3), (n = 100, h2 = 5), (n = 100, h2 = 75)

# n = 75, h2 = 25
load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_susie_n75_h0.75.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_susie <- simple_roc(labs, score)
roc_susie$Method <- "SuSiE"

load("C:/Document/Serieux/Travail/Package/susie_small_sample/simulations/pip_corsusie_n75_h0.75.RData")
labs <- temp[, 1]
score <- temp[, 2]
roc_corsusie <- simple_roc(labs, score)
roc_corsusie$Method <- "CorSuSiE"

roc_data <- rbind(roc_susie, roc_corsusie)
ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
  geom_line() +
  ggtitle("n75_h2_25") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme_minimal()+xlim(c(0,0.3)) +ylim(c(0,0.5))
