library(data.table)

n=30
h2=3

library(ggplot2)
library(cowplot)
library(dplyr)

simple_roc <- function(labs, scores){
  labs <- labs[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labs)/sum(labs), FPR=cumsum(!labs)/sum(!labs), labs)
}
# Load and process all datasets
h2_values <- c(25, 3, 5, 75)
n_values <- c(10, 20, 30, 50, 75, 100)
bf_labels <- c("SER Gaus", "SER SS" )


for (n in n_values) {
  for (h2 in h2_values) {

    pip_obs=  fread(paste0("~/susie_small_sample/simulations/pip_susie_n", n, "_h0.", h2, ".csv"))


    simple_roc <- function(labs, scores){
      labs <- labs[order(scores, decreasing=TRUE)]
      data.frame(TPR=cumsum(labs)/sum(labs), FPR=cumsum(!labs)/sum(!labs), labs)
    }


    #usage
    labs  <- pip_obs[,1]
    score <-  pip_obs[,2]



    if(h2==5 | h2==3){
      lab_h2= 10*h2
    }else{
      lab_h2=h2
    }



    roc <- simple_roc(labs, score)
    plot( 1-roc[,1],1-roc[,2], type="l",
          main= paste0("n=", n, ", h2=", lab_h2 ,"%"),
          xlab = "FPR",
          ylab="TPR",
          xlim=c(0,0.2),
          ylim=c(0,max(1-roc[which( (1-roc[,1])<0.2),2]))
    )

    pip_obs=  fread(paste0("~/susie_small_sample/simulations/pip_corsusie_n", n, "_h0.", h2, ".csv"))



    simple_roc <- function(labs, scores){
      labs <- labs[order(scores, decreasing=TRUE)]
      data.frame(TPR=cumsum(labs)/sum(labs), FPR=cumsum(!labs)/sum(!labs), labs)
    }


    #usage
    labs  <- pip_obs[,1]
    score <-  pip_obs[,2]


    roc <- simple_roc(labs, score)
    lines( 1-roc[,1],1-roc[,2], col="red")
    abline(a=0,b=1, lty=2)

  }

}




