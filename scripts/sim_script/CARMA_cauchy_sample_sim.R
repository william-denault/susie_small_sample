library(CARMA)
library(susieR)
attach(N3finemapping)
library(Rfast)
sim_dat <- function(N=20, h=0.5) {



  X <- N3finemapping$X[sample (1:nrow(N3finemapping$X), size=N, replace=FALSE), ]

  if (length(which( apply(X,2,var)==0))>0){
    X <- X[ ,-which( apply(X,2,var)==0)]
  }

  L <-sample(1:10, size=1)#Number of effect


  predictor<- rep(0, nrow(X))
  while(var(predictor)==0){
    true_pos <- sample( 1:ncol(X), L)# actually causal column/SNP

    if (L==1){
      predictor <- X[,true_pos]
    }else{
      predictor<- apply( X[,true_pos],1,sum)
    }
    noise <-  rnorm(N)

    y <- predictor + sqrt(var( predictor) / h - var( predictor ))*c(scale(noise))
  }



  return(list(X = X, y = y, true_pos = true_pos))
}


run_CARMA_sim <-  function(N=20,
                           h=0.5,
                           n_sim=100){

  res <- list( )
  idx =1
  for ( i  in (length(res)+1):n_sim){

    sim_data <- sim_dat(N=N,
                        h=h)
    y <- sim_data$y
    X <- sim_data$X
    true_pos <- sim_data$true_pos

    my_zscores <- c()
    for ( j in 1:ncol (X)){

      tt <- lm(y ~ X[,j])
      my_zscores <- c(my_zscores,  coef(summary(tt))[2,3])
    }


    z.list<- list(my_zscores)
    ld.list<- list(as.matrix(cor(X)))
    lambda.list<-list()
    lambda.list[[1]]<-1
    CARMA.results<-CARMA(z.list=z.list,
                         lambda.list=lambda.list,
                         ld.list=ld.list,
                         effect.size.prior = "Cauchy")



    if (var (y)>0.00001){
      out <- susieR::susie(X,y, L=10 )

      if(length(CARMA.results[[1]]$`Credible set`[[2]])!=0){

        n_true_cs <-   Reduce("+",sapply(1:length(CARMA.results[[1]]$`Credible set`[[2]]), function(k)
          ifelse( length(which(true_pos%in%CARMA.results[[1]]$`Credible set`[[2]][[k]] ))==0, 0,1)
        ))
        n_cs   <-    length(  CARMA.results[[1]]$`Credible set`[[2]]  )
        n_effect <- length(true_pos)

        res [[idx]] <- c( n_true_cs ,   n_cs,n_effect, mean (lengths(CARMA.results[[1]]$`Credible set`[[2]])  ) )

        idx <- idx+1
        print(res)
      }

    }

  }

  if(length(res)==0){
    warning("CARMA only reports dummy CS, increase n_sim or change h")
    return(NULL)
  }

  temp <- do.call (rbind ,res)
  colnames(temp) <- c("n_true_cs", "n_cs", "n_effect", "mean_cs_size")
  temp <- data.frame(temp)
  # Return results
  return(temp )
}
