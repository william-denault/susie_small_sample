library(susieR
)

N <- 10
set.seed(123)

res <- list( )
data(N3finemapping)
h=1
for ( i in (length(res)+1):10000){
  
  L <- sample (1:10, size=1) 
  
  X <- N3finemapping$X
  X <- N3finemapping$X[sample (1:nrow(X), size=N, replace=FALSE),]
  true_pos <- sample( 1:ncol(X), L)
  if( L==1){
    if (var( X[,true_pos])==0) next
    y <- X[, true_pos] 
  }else{
    y <- apply( X[, true_pos],1, sum)
  }
  
  y <- y + rnorm( N, sd=  3*( sd(y)))
  
  if (var (y)>0.00001){
    out <-  susie(X,y, L=1 )  
    out$sets
    if(!is.null(out$sets$cs)){
      
      n_true_cs <-   Reduce("+",sapply(1:length(out$sets$cs), function(k)
        ifelse( length(which(true_pos%in%out$sets$cs[[k]] ))==0, 0,1)
      ))
      n_cs   <-    length(  out$sets$cs  ) 
      n_effect <- length(true_pos)
      
      res [[h]] <- c( n_true_cs ,   n_cs,n_effect, mean (lengths(res1$sets[[1]])  ) )
      
      print( res[[h]])
      
      h=h+1
    }
    
  }
  
  
  
} 

temp <- do.call (rbind ,res)

save(temp, file="small_sample_susie10_h25.RData") 


library(susieR
)

N <- 10
set.seed(123)

res <- list( )
data(N3finemapping)
h=1
for ( i in (length(res)+1):10000){
  
  L <- sample (1:10, size=1) 
  
  X <- N3finemapping$X
  X <- N3finemapping$X[sample (1:nrow(X), size=N, replace=FALSE),]
  true_pos <- sample( 1:ncol(X), L)
  if( L==1){
    if (var( X[,true_pos])==0) next
    y <- X[, true_pos] 
  }else{
    y <- apply( X[, true_pos],1, sum)
  }
  
  y <- y + rnorm( N, sd=  1*( sd(y)))
  
  if (var (y)>0.00001){
    out <-  susie(X,y, L=1 )  
    out$sets
    if(!is.null(out$sets$cs)){
      
      n_true_cs <-   Reduce("+",sapply(1:length(out$sets$cs), function(k)
        ifelse( length(which(true_pos%in%out$sets$cs[[k]] ))==0, 0,1)
      ))
      n_cs   <-    length(  out$sets$cs  ) 
      n_effect <- length(true_pos)
      
      res [[h]] <- c( n_true_cs ,   n_cs,n_effect, mean (lengths(res1$sets[[1]])  ) )
      
      print( res[[h]])
      
      h=h+1
    }
    
  }
  
  
  
} 

temp <- do.call (rbind ,res)

save(temp, file="small_sample_susie10_h50.RData") 






library(susieR
)

N <- 10
set.seed(123)

res <- list( )
data(N3finemapping)
h=1
for ( i in (length(res)+1):10000){
  
  L <- sample (1:10, size=1) 
  
  X <- N3finemapping$X
  X <- N3finemapping$X[sample (1:nrow(X), size=N, replace=FALSE),]
  true_pos <- sample( 1:ncol(X), L)
  if( L==1){
    if (var( X[,true_pos])==0) next
    y <- X[, true_pos] 
  }else{
    y <- apply( X[, true_pos],1, sum)
  }
  
  y <- y + rnorm( N, sd=  2*( sd(y)))
  
  if (var (y)>0.00001){
    out <-  susie(X,y, L=1 )  
    out$sets
    if(!is.null(out$sets$cs)){
      
      n_true_cs <-   Reduce("+",sapply(1:length(out$sets$cs), function(k)
        ifelse( length(which(true_pos%in%out$sets$cs[[k]] ))==0, 0,1)
      ))
      n_cs   <-    length(  out$sets$cs  ) 
      n_effect <- length(true_pos)
      
      res [[h]] <- c( n_true_cs ,   n_cs,n_effect, mean (lengths(res1$sets[[1]])  ) )
      
      print( res[[h]])
      
      h=h+1
    }
    
  }
  
  
  
} 

temp <- do.call (rbind ,res)

save(temp, file="small_sample_susie10_h30.RData") 


sum( temp[,1] )/sum( temp[,2] )
dim(temp
)
coverage <-  c()
for ( i in 1:10){
  coverage <- c(coverage, sum( temp[which(temp[,3]==i),1] )/sum( temp[which(temp[,3]==i),2] ) )
}
coverage
