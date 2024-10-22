
source("C:/Document/Serieux/Travail/Data_analysis_and_papers/susie_small_sample/scripts/t_BF.R")
library(susieR)
attach(N3finemapping)


N=30
h=0.5

X <- N3finemapping$X[sample (1:nrow(N3finemapping$X), size=N, replace=FALSE), ]


X <- X[ ,-which( apply(X,2,var)==0)]
L <-sample(1:10, size=1)#Number of effect


predictor<- rep(0, nrow(X))
while(var(predictor)==0){
  true_pos <- sample( 1:ncol(X), L)# actually causal column/SNP

  if (L==1){
    predictor <- X[,true_pos]
  }else{
    predictor<- apply( X[,true_pos],1,sum)
  }

  y <- predictor + rnorm(N, sqrt(var( predictor) / h - var( predictor )))
}



plot(predictor, y)
out <-  susie(X,y, L=1 )
out$sets

str(out)
out$mu


plot(c(out$lbf_variable))
plot(t_lBF(out$mu, out$mu2, sqrt(out$V),df=30))


lBF<-  t_lBF(out$mu, out$mu2, sqrt(out$V),df=30)
alpha<- exp(lBF - max(lBF ) ) /sum( exp(lBF - max(lBF ) ))
cov_lev=0.95
temp        <- alpha

# check if temp has only 0 (i.e.  not yet updated)
#  if(sum(temp==0)==length(temp)){
temp_cumsum        <- cumsum( temp[order(temp, decreasing =TRUE)])
max_indx_cs        <- min(which( temp_cumsum >cov_lev ))
corrected_cs        <- order(temp, decreasing = TRUE)[1:max_indx_cs ]


corrected_cs
true_pos
plot(temp_cumsum )
alpha_susie= c(out$alpha)
points(cumsum( c(alpha_susie[order(alpha_susie, decreasing =TRUE)])))

