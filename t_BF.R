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