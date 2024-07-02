t_lBF <- function ( betahat, sdhat, sd_prior, df){


   up   <- LaplacesDemon::dstp(betahat,
                               tau = 1/(sdhat^2 + sd_prior^2),
                               nu  = df,
                               log = TRUE)
   down <- LaplacesDemon::dstp(betahat,
                               tau = 1/sdhat^2,
                               nu  = df,
                               log = TRUE)
   out <- up- down
   return(out)
}


Wake_lBF <-  function ( betahat, sdhat, sd_prior ){


    up   <- dnorm(betahat,
                  sd = sqrt(sdhat^2+  sd_prior^2) ,
                  log = TRUE)
    down <- dnorm(betahat,
                  sd = sqrt(sdhat^2 ) ,
                  log = TRUE)
  out <- up- down
  return(out)
}




betehat <- rnorm(1000)
sdhat <- runif(1000,min=0.3, max=0.5)
df=20
sd_prior=0.5
lBF <- t_lBF(betehat, sdhat, sd_prior, df)
Wake_lBF <- Wake_lBF(betehat, sdhat, sd_prior)
plot( Wake_lBF ,  lBF )




alpha<- exp(lBF - max(lBF ) ) /sum( exp(lBF - max(lBF ) ))
cov_lev=0.95
temp        <- alpha

# check if temp has only 0 (i.e.  not yet updated)
#  if(sum(temp==0)==length(temp)){
temp_cumsum        <- cumsum( temp[order(temp, decreasing =TRUE)])
max_indx_cs        <- min(which( temp_cumsum >cov_lev ))
corrected_cs        <- order(temp, decreasing = TRUE)[1:max_indx_cs ]


alpha<- exp(Wake_lBF - max(Wake_lBF) ) /sum( exp(Wake_lBF - max(Wake_lBF) ))
cov_lev=0.95
temp        <- alpha

# check if temp has only 0 (i.e.  not yet updated)
#  if(sum(temp==0)==length(temp)){
temp_cumsum_Wake       <- cumsum( temp[order(temp, decreasing =TRUE)])
max_indx_cs        <- min(which( temp_cumsum >cov_lev ))
corrected_cs_Wake        <- order(temp, decreasing = TRUE)[1:max_indx_cs ]
plot(temp_cumsum)
points(temp_cumsum_Wake, col="red")
abline(h=0.95)
