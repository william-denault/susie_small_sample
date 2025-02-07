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
  post_var <- omega*(post_var_up/post_var_down)* length(y)/(length(y)-2)
  return(  post_var)
}


posterior_var_SS_suff <- function (xtx,xty, n,s0_t=1){
  omega <- (( 1/s0_t^2)+xtx)^-1
  b_bar<- omega%*%(xty)
  post_var_up <- 0.5*(yty  -  b_bar *(omega ^(-1))*b_bar)
  post_var_down <- 0.5*(n*(1/omega ))
  post_var <- omega*(post_var_up/post_var_down)* n/(n-2)
  return(  post_var)
}



n<-10
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
sqrt(posterior_var_SS(x,y))











df <- n    # degrees of freedom
mu <- c(b_bar )  # location parameter (mean)
sigma <- sqrt( post_var_up/post_var_down * n/(n-2)) # scale parameter (standard deviation)
sigma<- c(sigma)
# Create a sequence of x values
X <- seq(-10, 10, length.out = 1000)
# Calculate density values for the 3-parameter t-distribution
density_t <- dt((X - mu) / sigma, df) / sigma

# Calculate density values for the normal distribution with same mean and sd




density_normal <- dnorm(X, mean =mu , sd = summary(lm(y~x))$coefficient[2,2])

# Create a data frame to use with ggplot
df_plot <- data.frame(x = X, density_t = density_t, density_normal = density_normal)

# Create the density plot using ggplot2
ggplot(df_plot, aes(x = X)) +
  geom_line(aes(y = density_t, color = "t-distribution")) +
  geom_line(aes(y = density_normal, color = "normal distribution"), linetype = "dashed") +
  labs(title = "Posterior SER using Gaussian vs SS model",
       x = "x",
       y = "Density") +
  xlim(c(-0.5,4))+
  scale_color_manual(name = "Distribution", values = c("t-distribution" = "blue", "normal distribution" = "red")) +
  theme_minimal()

