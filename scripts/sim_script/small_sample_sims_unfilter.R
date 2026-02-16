
library(susieR)
attach(N3finemapping)
library(data.table)
sim_dat <- function(N=20, h=0.5, L=NULL) {



  if(is.null(L)){
    L <-sample(1:10, size=1)#Number of effect

  }

  lf = list.files("/project2/mstephens/wdenault/susie_small_sample/data/1kg/rds/")
  id = sample( 1:length(lf), size=1)
  X <- readRDS(paste0("/project2/mstephens/wdenault/susie_small_sample/data/1kg/rds/" ,lf[id]))
  X <-   X[sample (1:nrow(  X), size=N, replace=FALSE), ]
  if (length(which( apply(X,2,var)==0))>0){
    X <- X[ ,-which( apply(X,2,var)==0)]
  }



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



  return(list(X = X, y = y, true_pos = true_pos, ncp = h*N/ L))
}




run_susie_sim <-  function(N=20,
                           h=0.5,
                           n_sim=100,
                           L_sim=NULL,
                           L_susie=NULL,
                           save_pip=FALSE){

  res <- list( )
  idx =1
  if(is.null(L_susie)){
    L_susie=10
  }


  for ( i  in (length(res)+1):n_sim){

    sim_data <- sim_dat(N=N,
                        h=h,
                        L =L_sim)

    y <- sim_data$y
    X <- sim_data$X
    true_pos <- sim_data$true_pos
    if (var (y)>0.00001){
      out <-  susieR::susie(X,y, L= L_susie,
                            min_abs_corr = 0,
                            check_null_threshold = -1000,
                            estimate_prior_method = "EM")
      out$sets
      if(!is.null(out$sets$cs)){

        n_true_cs <-   Reduce("+",sapply(1:length(out$sets$cs), function(k)
          ifelse( length(which(true_pos%in%out$sets$cs[[k]] ))==0, 0,1)
        ))
        n_cs   <-    length(  out$sets$cs  )
        n_effect <- length(true_pos)



        summary_reg= c( n_true_cs ,
                        n_cs,n_effect,
                        mean (lengths(out$sets$cs )  ),
                        0 ,
                        mean(out$sets$purity[,2]),
                        out$sigma2 ,
                        out$V,sim_data $ncp)

     names(summary_reg)   =  c("n_true_cs", "n_cs", "n_effect", "mean_cs_size", "is.dummy", "purity","sigma", paste("sigma0",1:length(
                                                                                                                 out$V ), sep="_"),"NCP")

        cs= out$sets

      }else{

        n_effect <- length(true_pos)
        summary_reg <- c( 0 ,   0,n_effect, NaN,1, 0 ,0,0,sim_data $ncp)
        cs= out$sets
      }
      if(save_pip){

        tempfile <- paste0("/home/wdenault/susie_small_sample/simulations/pip_susie_n",N,"_h",h,".csv" )

        if( file.exists(paste0("/home/wdenault/susie_small_sample/simulations/pip_susie_n",N,"_h",h,".csv" ) )){

          temp = fread(paste0("/home/wdenault/susie_small_sample/simulations/pip_susie_n",N,"_h",h,".csv" ) )

          true_entry= 0*out$pip
          true_entry[true_pos]=1
          temp0= cbind(  true_entry, out$pip)
          if (ncol(temp) != ncol(temp0)) {
            stop("Column mismatch between temp and temp0")
          }
          colnames(temp0)= colnames(temp)
          temp = rbind(temp, temp0)


          fwrite(temp, file=tempfile, quote=FALSE, row.names=FALSE, col.names=TRUE, sep=",")

        }else{
          true_entry= 0*out$pip
          true_entry[true_pos]=1
          temp0= cbind(  true_entry, out$pip)
          temp = (temp0)
          rownames(temp) <- NULL
          fwrite(temp, file=tempfile, quote=FALSE, row.names=FALSE, col.names=TRUE, sep=",")

        }
      }


      res [[i ]] <- list(summary_reg=summary_reg,
                         cs= cs,
                         true_pos=true_pos)




  }
}
  if(length(res)==0){
    warning("Susie only reports dummy CS, increase n_sim or change h")
    return(NULL)
  }


  return(res)
}
