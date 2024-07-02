# Load necessary libraries
library(ggplot2)
set.seed(1)
df=20
cov_lev =0.95
# Define the density functions
x <- seq(0, 10, length.out = 10000)
y_gaussian1 <- dnorm(x, mean = 0, sd = sqrt(1))
y_gaussian2 <- dnorm(x, mean = 0, sd = sqrt(1.5))



# Create a data frame for plotting
data <- data.frame(
  x = rep(x, 2),
  density = c(y_gaussian1, y_gaussian2),
  distribution = factor(rep(c("Gaussian Variance 1", "Gaussian Variance 1.5" ), each = length(x)))
)

# Plot the densities
ggplot(data, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 1) +
 # xlim(-4, 4) +
 # ylim(0, 0.4) +
  labs(title = "Density Tails of Gaussian and Student's t Distributions",
       x = "x",
       y = "Density") +
  theme_minimal()




y_student1 <- LaplacesDemon::dstp(x,
                                  tau = 1/(1 ),
                                  nu  = df )

y_student2 <- LaplacesDemon::dstp(x,
                                  tau = 1/(1.5),
                                  nu  = df )


data_t <- data.frame(
  x = rep(x, 4),
  density = c( y_student1, y_student2, y_gaussian1, y_gaussian2 ),
  var_param = factor(rep(c("var =1", "var =1.5" ,
                              "var =1", "var =1.5" ), each = length(x))),
  distribution= factor(rep(c( "Student", "Student","Gaussian", "Gaussian"  ), each = length(x)))


)



P1= ggplot(data_t, aes(x = x, y = density, color = distribution,  linetype = var_param )) +
  geom_line(size = 1) +
   xlim(3, 7) +
   ylim(0, 0.002) +
  labs(title = "Density Tails of Gaussian and Student's t Distributions",
       x = "x",
       y = "Density") +
  theme_minimal()

P1



data_bf  <-   data.frame(
  x = rep(x, 2),
  bf  =  c(y_gaussian2/ y_gaussian1, y_student2/y_student1),
  type = factor(rep(c("Wakefeild BF", "Student BF" ), each = length(x)))

)

# Plot the densities
P2 =ggplot(data_bf  , aes(x = x, y = bf, col= type)) +
  geom_line(size = 1) +
  ylim(0, 40) +
  xlim(1, 7) +
  labs(title = "Density Tails of Gaussian and Student's t Distributions",
       x = "Estimated effect",
       y = "Bayes factor") +
  theme_minimal()
P2

set.seed(1)
df=50
x <- c(rnorm(100, sd=.6),rnorm(100, mean=2, sd=5))

y_student1 <- LaplacesDemon::dstp(x,
                                  tau = 1/(1 ),
                                  nu  = df )

y_student2 <- LaplacesDemon::dstp(x,
                                  tau = 1/(1.5),
                                  nu  = df )

y_gaussian1 <- dnorm(x, mean = 0, sd = sqrt(1))
y_gaussian2 <- dnorm(x, mean = 0, sd = sqrt(1.5))



Wake_lBF<- log(y_gaussian2)- log( y_gaussian1)
alpha<- exp(Wake_lBF - max(Wake_lBF) ) /sum( exp(Wake_lBF - max(Wake_lBF) ))
cov_lev=0.95
temp        <- alpha
temp_cumsum_Wake       <- cumsum( temp[order(temp, decreasing =TRUE)])
max_indx_cs        <- min(which( temp_cumsum_Wake >cov_lev ))

student_lBF<- log( y_student2 )- log(y_student1)
alpha<- exp(student_lBF - max(student_lBF) ) /sum( exp(student_lBF- max(student_lBF) ))
cov_lev=0.95
temp        <- alpha
temp_cumsum        <- cumsum( temp[order(temp, decreasing =TRUE)])
corrected_cs_Wake        <- order(temp, decreasing = TRUE)[1:max_indx_cs ]

max_indx_cs_cor        <- min(which( temp_cumsum  >cov_lev ))

plot(temp_cumsum)
points(temp_cumsum_Wake, col="red")
abline(h=0.95)


df_alpha=  data.frame(
  x =1:length(x),
  cumsum  =  c(temp_cumsum_Wake,temp_cumsum),
  type = factor(rep(c("Wakefeild", "Student" ), each = length(x)))

)
P3=ggplot(df_alpha, aes(x = x, y = cumsum, col= type)) +
  geom_point(size = 3) +
  xlim(0, 50) +
  labs(title = "Stopping Rule",
       x = "Index",
       y = "Cumulative sum") +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_vline(xintercept = max_indx_cs, linetype = "dashed") +
  geom_vline(xintercept = max_indx_cs_cor, linetype = "dashed") +

  theme_minimal()

library(gridExtra)
grid.arrange(P1, P2,P3, ncol=1)
