posterior_mean_SS<- function(x,y, s0_t=1){
  omega <- (( 1/s0_t^2)+crossprod(x))^-1
  b_bar<- omega%*%(crossprod(x,y))
  return( b_bar)
}

posterior_mean_SS_suff <- function(xtx,xty, s0_t=1){
  omega <- (( 1/s0_t^2)+xtx)^-1
  b_bar<- omega%*%(xty)
  return( b_bar)
}

posterior_var_SS <- function (x,y, s0_t=1){

  omega <- (( 1/s0_t^2)+crossprod(x))^-1
  b_bar<- omega%*%(crossprod(x,y))

  post_var_up <- 0.5*(crossprod(y)  -  b_bar *(omega ^(-1))*b_bar)
  post_var_down <- 0.5*(length(y)*(1/omega ))
  post_var <- (post_var_up/post_var_down)* length(y)/(length(y)-2)
  return(  post_var)
}


posterior_var_SS_suff <- function (xtx,xty, n,s0_t=1){
  omega <- (( 1/s0_t^2)+xtx)^-1
  b_bar<- omega%*%(xty)
  post_var_up <- 0.5*(yty  -  b_bar *(omega ^(-1))*b_bar)
  post_var_down <- 0.5*(n*(1/omega ))
  post_var <- (post_var_up/post_var_down)* n/(n-2)
  return(  post_var)
}



n<-100
x<-rnorm(n )
y<-  2*x + rnorm(n)


xtx = t(x)%*%x
xty= t(x)%*%y
yty = t(y)%*%y
s0_t=1

summary(lm(y~x))$coefficient[2,1]


posterior_mean_SS(x,y)
posterior_mean_SS_suff (xtx,xty)
posterior_var_SS(x,y)
posterior_var_SS_suff (xtx,xty,n)




omega <- (( 1/s0_t^2)+xtx)^-1
b_bar<- omega%*%(xty)

post_var_up <- 0.5*(yty  -  b_bar *(omega ^(-1))*b_bar)
post_var_down <- 0.5*(n*(1/omega ))
post_var <- (post_var_up/post_var_down)* n/(n-2)



omega <- (( 1/s0_t^2)+xtx)^-1
b_bar<- omega%*%(xty)


summary(lm(y~x))$coefficient[2,2]^2
posterior_var_SS(x,y)
