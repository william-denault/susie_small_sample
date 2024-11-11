library(ggplot2)
library(cowplot)
library(dplyr)

simple_roc <- function(labs, scores){
  labs <- labs[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labs)/sum(labs), FPR=cumsum(!labs)/sum(!labs), labs)
}
# Load and process all datasets
h2_values <- c(25, 3,  75)
n_values <- c(10, 20, 30, 50, 75, 100)
bf_labels <- c("SER Gaus", "SER SS" )

data_list <- list()

for (n in n_values) {
  for (h2 in h2_values) {


    load(paste0("~/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".RData"))
    labs  <-  (temp[,1])
    score <- (temp[,2])
    roc <- simple_roc(labs, score)

    y_susie = 1-roc$FPR
    x_susie = 1-roc$TPR

    plot(  1- roc$FPR,1- roc$TPR, type='l',
          main= paste0("n", n, "_h2_", h2))


    load(paste0("~/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".RData"))


    labs  <-  (temp[,1])
    score <- (temp[,2])
    roc <- simple_roc(labs, score)

    y_corsusie = 1-roc$FPR
    x_corsusie = 1-roc$TPR

    roc <- simple_roc(labs, score)

    lines( 1- roc$FPR,1-roc$TPR,  col="red")
  }
}


