
library(susieR)
attach(N3finemapping)
library(data.table)
sim_dat <- function(N=20,sd_res=1, L=1, sd_prior=0.5) {



  if(is.null(L)){
    L <-sample(1:10, size=1)#Number of effect

  }

   X= N3finemapping$X
  X <-   X[sample (1:nrow(  X), size=N, replace=FALSE), ]

  if (length(which( apply(X,2,var)==0))>0){
    X <- X[ ,-which( apply(X,2,var)==0)]
  }



  effect= rnorm(L, sd= sd_prior)
  predictor<- rep(0, nrow(X))
  while(var(predictor)==0){
    true_pos <- sample( 1:ncol(X), L)# actually causal column/SNP

    if (L==1){
      predictor <-  effect*X[,true_pos]
    }else{
      predictor<-   X[,true_pos] %*%effect
    }
    noise <-  rnorm(N,sd=sd_res)

    y <- predictor +noise
  }



  return(list(X = X, y = y, true_pos = true_pos, sd_prior=sd_prior))
}


run_susie_sim <-  function(N=20,
                           h=0.5,
                           n_sim=100,
                           L_sim=1,
                           L_susie=1,
                           sd_res=1,
                           sd_prior=0.5){

  res <- list( )
  idx =1
  if(is.null(L_susie)){
    L_susie=10
  }


  for ( i  in (length(res)+1):n_sim){

    sim_data <- sim_dat(N=N,
                        L =L_sim,
                        sd_res= sd_res,
                        sd_prior=sd_prior)

    y <- sim_data$y
    X <- sim_data$X
    true_pos <- sim_data$true_pos
    if (var (y)>0.00001){
      out <-  susieR::susie(X,y, L= L_susie,
                            estimate_residual_variance = FALSE,
                            residual_variance = sd_res^2,
                            estimate_prior_variance = FALSE,
                            scaled_prior_variance=sd_prior^2)
      out$sets
      if(!is.null(out$sets$cs)){

        n_true_cs <-   Reduce("+",sapply(1:length(out$sets$cs), function(k)
          ifelse( length(which(true_pos%in%out$sets$cs[[k]] ))==0, 0,1)
        ))
        n_cs   <-    length(  out$sets$cs  )
        n_effect <- length(true_pos)

        res [[i ]] <- c( n_true_cs ,   n_cs,n_effect, mean (lengths(out$sets$cs )  ),0 , mean(out$sets$purity[,2]))
      }else{


        n_effect <- length(true_pos)
        res [[i ]] <- c( 0 ,   0,n_effect, NaN,1, 0 )

      }

        #  print(res)









      }


  }

  if(length(res)==0){
    warning("Susie only reports dummy CS, increase n_sim or change h")
    return(NULL)
  }

  temp <- do.call (rbind ,res)
  colnames(temp) <- c("n_true_cs", "n_cs", "n_effect", "mean_cs_size", "is.dummy", "purity")
  temp <- data.frame(temp)
  # Return results
  return(temp )
}








