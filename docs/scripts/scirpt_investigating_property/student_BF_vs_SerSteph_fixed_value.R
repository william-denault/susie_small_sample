

comp_bf_ran <-function(n=100 , nsimu=10000,s0=1, beta=1){

  tl<- list()
  for( i in 1: nsimu){

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



#check 0.2, 1, 2 , 5  10
    g  = rnorm(n)
    y= beta*g+rnorm(length(g))

    Servin_BF <- logBF(g,y,sigmaa =   s0,1)*log(10)
    sigmaa=1
    sigmad=1
    fit <- summary(  lm(y~g))

    student_BF <- t_lBF(fit$coefficients[2,1],fit$coefficients[2,2],sd_prior = s0, n-1  )
    Wake       <- Wake_lBF(fit$coefficients[2,1],fit$coefficients[2,2],1)
    tl[[i]] <- c(Servin_BF,student_BF,Wake )
  }
  return(do.call(rbind, tl))
}

#### beta = 0.1 ----
par(mfrow=c(2,1))
#### n= 30
lol <- comp_bf_ran(n=30,beta = 0.1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF  ", ylab="log BF",
     main="n=30, beta =  0.1 ", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 50
lol <- comp_bf_ran(n=50,beta = 0.1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=50, beta =  0.1", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
 #### n= 100
lol <- comp_bf_ran(n=100,beta = 0.1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta =  0.1 ", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)

lm(lol[,1]~lol[,2])

lm(lol[,1]~lol[,3])


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")








#### beta =  1 ----
par(mfrow=c(2,1))
#### n= 30
lol <- comp_bf_ran(n=30,beta =  1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF  ", ylab="log BF",
     main="n=30, beta =  1", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 50
lol <- comp_bf_ran(n=50,beta =  1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=50, beta =  1", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 100
lol <- comp_bf_ran(n=100,beta =  1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta =  1", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)




summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")







#### beta = 2 ----
par(mfrow=c(2,1))
#### n= 30
lol <- comp_bf_ran(n=30,beta = 2 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF  ", ylab="log BF",
     main="n=30, beta = 2", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 50
lol <- comp_bf_ran(n=50,beta = 2 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=50, beta = 2", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 100
lol <- comp_bf_ran(n=100,beta = 2 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta = 2", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)







#### beta = 5 ----
par(mfrow=c(2,1))
#### n= 30
lol <- comp_bf_ran(n=30,beta = 5 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF  ", ylab="log BF",
     main="n=30, beta = 5", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")





#### n= 50
lol <- comp_bf_ran(n=50,beta = 5 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=50, beta = 5", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 100
lol <- comp_bf_ran(n=100,beta = 5 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta = 5", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")



lol <- comp_bf_ran(n=100,beta = 5,s0=5 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta = 5, s0=5", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### beta = 10 ----
par(mfrow=c(2,1))
#### n= 30
lol <- comp_bf_ran(n=30,beta = 10 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF  ", ylab="log BF",
     main="n=30, beta = 10", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 50
lol <- comp_bf_ran(n=50,beta = 10 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=50, beta = 10", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
#### n= 100
lol <- comp_bf_ran(n=100,beta = 10,s0=1 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta = 10", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")


lol <- comp_bf_ran(n=100,beta = 10,s0=10 )
plot(lol[,1],lol[,2], xlab="Servin & Stephens log BF ", ylab="log BF",
     main=" n=100, beta = 10", pch=19, col="blue")
points(lol[,1],lol[,3], pch=19, col="green")
abline(a=0,b=1)


summary(lm(lol[,1]~lol[,2]))
plot(residuals(lm(lol[,1]~lol[,2]))[order(lol[,1])],ylab="residuals",  pch=19,
     main="Residuals of the regression of Servin & Stephens log BF on Student log BF")

abline(h=0, col="red")
