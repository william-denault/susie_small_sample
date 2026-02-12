#labs binary variable 1 case, 0 control
#score prediction/fdr etc

simple_roc <- function(labs, scores){
  labs <- labs[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labs)/sum(labs), FPR=cumsum(!labs)/sum(!labs), labs)
}

#usage
load("~/susie_small_sample/simulations/pip_susie_n10_h0.3.RData")
labs  <-  (temp[,1])
score <- (temp[,2])
roc <- simple_roc(labs, score)
plot(   roc$FPR,roc$TPR, type='l')
load("~/susie_small_sample/simulations/pip_corsusie_n10_h0.3.RData")
labs  <-  (temp[,1])
score <- (temp[,2])
roc <- simple_roc(labs, score)
lines(  roc$FPR,roc$TPR,  col="red")

load("~/susie_small_sample/simulations/pip_susie_n20_h0.75.RData")
labs  <-  (temp[,1])

score <- (temp[,2])
roc <- simple_roc(labs, score)
plot(   roc$FPR,roc$TPR, type='l')
load("~/susie_small_sample/simulations/pip_corsusie_n20_h0.75.RData")
labs  <-  (temp[,1])
score <- (temp[,2])
roc <- simple_roc(labs, score)
lines(  roc$FPR,roc$TPR,  col="red")
