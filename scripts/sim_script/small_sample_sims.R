sim_dat <- function(N=20,  h=0.5){
  
  predictor = rep(0, N)
  while(var(predictor)==0){
    library(susieR)
    data(N3finemapping)
    
    
    L <- sample (1:10, size=1) 
    
    X <- N3finemapping$X
    X <- N3finemapping$X[sample (1:nrow(X), size=N, replace=FALSE),]
    
   true_pos <- sample( 1:ncol(X), L)
  if( L==1){
    if (var( X[,true_pos])==0)  
    y <- X[, true_pos] 
  }else{
    y <- apply( X[, true_pos],1, sum)
  }
   predictor <- y
  } 
   
  
  
  y <- y + rnorm( N, sd=  sqrt(var(y)/h -var(y)))
  
  return(list(X=X, y=y, true_pos=true_pos))
  
}




run_susie_sim <-  function(N=20, 
                           h=0.5,
                           n_sim=10000){
  
  res <- list( )
  idx =1
  for ( i  in (length(res)+1):n_sim){
    
    sim_data <- sim_dat(N=N,
                        h=h)
     
    y <- sim_data$y
    X <- sim_data$X
    true_pos <- sim_data$true_pos
    if (var (y)>0.00001){
      out <-  susie(X,y, L=1 )  
      out$sets
      if(!is.null(out$sets$cs)){
        
        n_true_cs <-   Reduce("+",sapply(1:length(out$sets$cs), function(k)
          ifelse( length(which(true_pos%in%out$sets$cs[[k]] ))==0, 0,1)
        ))
        n_cs   <-    length(  out$sets$cs  ) 
        n_effect <- length(true_pos)
        
        res [[idx]] <- c( n_true_cs ,   n_cs,n_effect, mean (lengths(out$sets[[1]])  ) )
        
        idx <- idx+1
        print(res)
      }
      
    }
     
  }
  
 if(length(res)==0){
   warning("Susie only reports dummy CS, increase n_sim or change h")
   return(NULL)
 }
  
  temp <- do.call (rbind ,res)
  colnames(temp) <- c("n_true_cs", "n_cs", "n_effect", "mean_cs_size")
  temp <- data.frame(temp)
  # Return results
  return(temp )
}


compute_metric <-function( res_sim){
  
  
  
  obs_cs_size <- rep( NA,length(unique(res_sim$n_effect)))
  idx=1
  for ( i in unique(res_sim$n_effect)){
   
    obs_cs_size[ idx] <- mean( res_sim[which(res_sim$n_effect == (i )),4] )  
    idx= idx+1
  } 
  obs_cov <- rep( NA,length(unique(res_sim$n_effect)))
  idx=1
  for ( i in unique(res_sim$n_effect)){
    obs_cov[idx] <- sum( res_sim[which(res_sim[,3] == (i )),1] )/sum( res_sim[which(res_sim[,3] == (i )),2] )
    idx= idx+1
  } 
  
  out <- data.frame(L=unique(res_sim$n_effect),  obs_cs_size, obs_cov) 
  
  return(out )
}

