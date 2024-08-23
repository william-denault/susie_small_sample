
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



cor_WD_dens<- function(x, mu = 0, tau = 1, nu = 10, log = FALSE) {
  x <- as.vector(x)
  mu <- as.vector(mu)
  tau <- as.vector(tau)
  nu <- as.vector(nu)
  if (any(tau <= 0))
    stop("The tau parameter must be positive.")
  if (any(nu <= 0))
    stop("The nu parameter must be positive.")
  NN <- max(length(x), length(mu), length(tau), length(nu))
  x <- rep(x, len = NN)
  mu <- rep(mu, len = NN)
  tau <- rep(tau, len = NN)
  nu <- rep(nu, len = NN)
  dens <- (lgamma((nu + 1)/2) - lgamma(nu/2)) + 0.5 * log(tau/(nu *
                                                                 pi)) + (-(nu +1.8 )/2) * log(1 + (tau/(nu)) * (x - mu)^2)
  temp <- which(nu > 1e+06)
  dens[temp] <- dnorm(x[temp], mu[temp], sqrt(1/tau[temp]),
                      log = TRUE)
  if (log == FALSE)
    dens <- exp(dens)
  return(dens)
}




t_lBF_cor <- function ( betahat, sdhat, sd_prior, df){


  up   <-cor_WD_dens(betahat,
                              tau = 1/(sdhat^2 + sd_prior^2),
                              nu  = df,
                              log = TRUE)
  down <- cor_WD_dens(betahat,
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


t_tes_BF <- function( betahat, sdhat, sd_prior, df){


  up   <- dt(betahat,  2*df-2, log=TRUE)
  down <-  dt(betahat/sqrt(1/(sdhat^2 + sd_prior^2)), 2*df-2, log=TRUE)
  out <-  (down/( (sqrt(1/(sdhat^2 + sd_prior^2)))))/up
  return(log(out))
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


set.seed(1234)
n=50
beta=runif(1,min=0.2, max=1.5)
t_SS<- c()
t_student <- c()
t_Wake <-c()
t_student_cor  <- c()
#check 0.2, 1, 2 , 5  10
g  = rnorm(n)
y= beta*g+rnorm(length(g))

fit <- summary(  lm(y~g))
seq(0.1,2,by=0.001) -> s0_seq

for (i in 1:length(s0_seq)){
  t_SS  <- c(t_SS, logBF(g,y,sigmaa =   s0_seq[i],1)*log(10))
  t_student  <- c(t_student ,t_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i],length(g) ))
  t_Wake  <- c(t_Wake , Wake_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i]))
  t_student_cor  <- c(t_student_cor ,t_lBF_cor(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i],length(g) ))

}

par(mfrow=c(1,1))
plot(s0_seq,t_Wake , type="l",col="darkgreen" , xlab="sigma_a", ylab="logBF", main="Bayes factor vs prior, n=50  ")
lines(s0_seq,t_student, col="blue")
lines(s0_seq, t_SS ,col="red" )
lines(s0_seq, t_student_cor,col="orange" )

legend("topright", legend=c("Wakefeild","Student","Servin_Stephens"), col=c("darkgreen","blue","red"), lty=1:1, cex=0.8)

abline(v=s0_seq[which.max(t_SS)] ,col="red")
abline(v=s0_seq[which.max(t_student)],col="blue")
abline(v=s0_seq[which.max(t_Wake)],col="darkgreen")






library(BayesFactor)

n=100
max_s0_SS<-c()
max_s0_t<-c()
max_s0_Wake<-c()
max_SS<-c()
max_t<-c()
max_Wake<-c()
t_test <-  c( )
max_t_cor<-c()
for ( o in 1:100){


  n=sample(size=1,50:200)
  beta=runif(1,min=0.2, max=1.5)
  t_SS<- c()
  t_student <- c()
  t_Wake <-c()

  t_student_cor  <- c()
  #check 0.2, 1, 2 , 5  10
  g  = rnorm(n)
  y= beta*g+rnorm(length(g))

  fit <- summary(  lm(y~g))
  seq(0.1,2,by=0.001) -> s0_seq

  for (i in 1:length(s0_seq)){
    t_SS  <- c(t_SS, logBF(g,y,sigmaa =   s0_seq[i],1)*log(10))
    t_student  <- c(t_student ,t_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i],length(g) ))
    t_Wake  <- c(t_Wake , Wake_lBF(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i]))
    t_student_cor  <- c(t_student_cor ,t_lBF_cor(fit$coefficients[2,1],fit$coefficients[2,2],s0_seq[i],length(g) ))

  }

  data <- data.frame(y = y, x = g)

 # result <- regressionBF(y ~ x, data = data, rscaleCont = "ultrawide")

  max_SS<-c(max_SS, t_SS[which.max(t_SS)])
  max_t<-c(max_t, t_student[which.max(t_student)])
  max_Wake<-c(max_Wake, t_Wake[which.max(t_Wake)])
  max_s0_SS<-c(max_s0_SS, s0_seq[which.max(t_SS)])
  max_s0_t<-c(max_s0_t, s0_seq[which.max(t_student)])
  max_s0_Wake<-c(max_s0_Wake, s0_seq[which.max(t_Wake)])
  t_test <-  c(t_test, result@bayesFactor[1]
  )
  max_t_cor<- c(max_t_cor,  t_student_cor[which.max( t_student_cor)])
  print(o)
}
t_test<- do.call(c, t_test)

plot(  max_s0_SS, max_s0_t,pch=19, col="red")
points(  max_s0_SS, max_s0_Wake,pch=19,col="blue")
abline(a=0,b=1)

plot(  max_SS, max_t,pch=19, col="red",
       xlab = "log Servin Stepehens BF",
       ylab="log BF")
points(  max_SS, max_Wake,pch=19,col="blue")
points(  max_SS, t_test,pch=19,col="darkgreen")

points(  max_SS, max_t_cor,pch=19,col="orange")
legend("topleft", legend = c("Denault",
                             "Wakefeild",
                             "JZS") ,
       col = c("red",
               "blue",
               "darkgreen"),
       lwd=2)
abline(a=0,b=1)
summary(lm(max_SS~  max_t))
which(max_SS< max_t)
plot(  residuals(lm(max_SS~  max_t)),pch=19)
points(  residuals(lm(max_SS~   max_t_cor)), col="orange",pch=19)

