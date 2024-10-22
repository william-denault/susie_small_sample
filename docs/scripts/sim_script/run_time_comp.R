
run_time = function(){

  N= sample( c(10,20,30,50,75,100), size = 1)
  h= sample(c(0.25, .3, .5,.75), size = 1)

  lf = list.files("/home/wdenault/susie_small_sample/data/1kg/rds/")
  id = sample( 1:length(lf), size=1)
  X <- readRDS(paste0("/home/wdenault/susie_small_sample/data/1kg/rds/" ,lf[id]))
  X <-   X[sample (1:nrow(  X), size=N, replace=FALSE), ]

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
  sim_data <- list(X = X, y = y, true_pos = true_pos)




  y <- sim_data$y
  X <- sim_data$X
  true_pos <- sim_data$true_pos
  my_zscores <- c()
  for ( j in 1:ncol (X)){

    tt <- lm(y ~ X[,j])
    my_zscores <- c(my_zscores,  coef(summary(tt))[2,3])
  }


  z.list<- list(my_zscores)
  ld.list<- list(as.matrix(cor(X)^2))
  lambda.list<-list()
  lambda.list[[1]]<-1
  pt = proc.time()
  CARMA.results<- CARMA(z.list=z.list,
                        lambda.list=lambda.list,
                        ld.list=ld.list,
                        effect.size.prior = "Cauchy",
                        outlier.switch = "F",Max.Model.Dim=10)
  run_time_carma = proc.time()-pt


  pt= proc.time()
  susie_res = susieR::susie(X,y, L=10 )
  run_time_susie= proc.time()-pt


  pt= proc.time()
  susie_res = susieRsmall::susie(X,y, L=10 )
  run_time_susie_small= proc.time()-pt



  out =  data.frame(Method= c("CARMA", "SuSiE", "SuSiE_small"),
                    time =  c( run_time_carma[1], run_time_susie[1], run_time_susie_small[1]),
                    N= rep(N,3),
                    h = rep(h,3),
                    L= rep(L,3)

  )


  return(out)





}
