
logBF = function(g,y,sigmaa,sigmad){
  subset = complete.cases(y) & complete.cases(g)
  y=y[subset]
  g=g[subset]
  n=length(g)
  X = cbind(rep(1,n),g,g==1)
  invnu = diag(c(0,1/sigmaa^2,1/sigmad^2))
  invOmega = invnu + t(X) %*% X
  B = solve(invOmega, t(X) %*% cbind(y))
  invOmega0 = n
  return(-0.5*log10(det(invOmega)) + 0.5*log10(invOmega0) - log10(sigmaa)
         - log10(sigmad) -(n/2) * (log10( t(y- X %*% B) %*% y)
                                   - log10(t(y) %*% y - n*mean(y)^2) ))
}



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


set.seed(1)
n=500
beta=runif(1,min=0.2, max=1.5)
t_SS<- c()
t_student <- c()
t_Wake <-c()
#check 0.2, 1, 2 , 5  10
g  = rnorm(n)
y= beta*g+rnorm(length(g))

fit <- summary(  lm(y~g))
seq(0.1,5,by=0.001) -> s0_seq

for (i in 1:length(s0_seq)){
  t_SS  <- c(t_SS, logBF(g,y,sigmaa =   s0_seq[i],1)*log(10))
  t_student  <- c(t_student ,t_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i],length(g) ))
  t_Wake  <- c(t_Wake , Wake_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i]))
}

par(mfrow=c(1,1))
plot(s0_seq,t_Wake , type="l",col="darkgreen" , xlab="sigma_a", ylab="logBF")
lines(s0_seq,t_student, col="blue")
lines(s0_seq, t_SS ,col="red" )


abline(v=s0_seq[which.max(t_SS)] ,col="red")
abline(v=s0_seq[which.max(t_student)],col="blue")
abline(v=s0_seq[which.max(t_Wake)],col="darkgreen")


n=100
max_s0_SS<-c()
max_s0_t<-c()
max_s0_Wake<-c()
for ( o in 1:1000){



  beta=runif(1,min=0.2, max=1.5)
  t_SS<- c()
  t_student <- c()
  t_Wake <-c()
  #check 0.2, 1, 2 , 5  10
  g  = rnorm(n)
  y= beta*g+rnorm(length(g))

  fit <- summary(  lm(y~g))
  seq(0.1,2,by=0.001) -> s0_seq

  for (i in 1:length(s0_seq)){
    t_SS  <- c(t_SS, logBF(g,y,sigmaa =   s0_seq[i],1)*log(10))
    t_student  <- c(t_student ,t_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i],length(g) ))
    t_Wake  <- c(t_Wake , Wake_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i]))
  }

  max_s0_SS<-c(max_s0_SS, s0_seq[which.max(t_SS)])
  max_s0_t<-c(max_s0_t, s0_seq[which.max(t_student)])
  max_s0_Wake<-c(max_s0_Wake, s0_seq[which.max(t_Wake)])
}


 plot(  max_s0_SS, max_s0_t,pch=19, col="red")
 points(  max_s0_SS, max_s0_Wake,pch=19,col="blue")
