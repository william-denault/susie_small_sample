

##### 10 ----
load("~/susie_small_sample/simulations/small_sample_susie10_h25.RData")
obs_size<- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <- mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)



load("~/susie_small_sample/simulations/cor_small_sample_susie10_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P11 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
   ggtitle("n=10, h=0.25")+theme_cowplot()+theme(legend.position = "none")
P11

load("~/susie_small_sample/simulations/small_sample_susie10_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)


load("~/susie_small_sample/simulations/cor_small_sample_susie10_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P12 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=10, h=0.3")+
  theme_cowplot()+theme(legend.position = "none")
P12




load("~/susie_small_sample/simulations/small_sample_susie10_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)

load("~/susie_small_sample/simulations/cor_small_sample_susie10_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P13 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=10, h=0.5")+
  theme_cowplot()+theme(legend.position = "none")
P13




##### 20 ----


load("~/susie_small_sample/simulations/small_sample_susie20_h25.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)

load("~/susie_small_sample/simulations/cor_small_sample_susie20_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P21 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.25")+theme_cowplot()+theme(legend.position = "none")
P21

load("~/susie_small_sample/simulations/small_sample_susie20_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)


load("~/susie_small_sample/simulations/cor_small_sample_susie20_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P22 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+theme_cowplot()+theme(legend.position = "none")
P22




load("~/susie_small_sample/simulations/small_sample_susie20_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)



load("~/susie_small_sample/simulations/cor_small_sample_susie20_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P23 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+theme_cowplot()+theme(legend.position = "none")
P23


##### 30 ----

load("~/susie_small_sample/simulations/small_sample_susie30_h25.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


load("~/susie_small_sample/simulations/cor_small_sample_susie30_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P31 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  ggtitle( expression(n == 30) )+
  theme_cowplot()+theme(legend.position = "none")
P31

load("~/susie_small_sample/simulations/small_sample_susie30_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
plot( obs_size)


load("~/susie_small_sample/simulations/cor_small_sample_susie30_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P32 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=30, h=0.3")+
  theme_cowplot()+theme(legend.position = "none")
P32




load("~/susie_small_sample/simulations/small_sample_susie30_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie30_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P33 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=30, h=0.5")+
  theme_cowplot()+theme(legend.position = "none")
P33

library(gridExtra)

#### 50 ----



load("~/susie_small_sample/simulations/small_sample_susie50_h25.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P41 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  ggtitle( expression(n == 40) ) +theme_cowplot()+theme(legend.position = "none")
P41

load("~/susie_small_sample/simulations/small_sample_susie50_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P42 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+theme_cowplot()+theme(legend.position = "none")
P42




load("~/susie_small_sample/simulations/small_sample_susie50_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P43 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+theme_cowplot()+theme(legend.position = "none")
P43

library(gridExtra)




load("~/susie_small_sample/simulations/small_sample_susie50_h25.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P41 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  ggtitle( expression(n == 50) )++theme_cowplot()+theme(legend.position = "none")
P41

load("~/susie_small_sample/simulations/small_sample_susie50_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P42 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+
  theme_cowplot()+theme(legend.position = "none")
P42




load("~/susie_small_sample/simulations/small_sample_susie50_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P43 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  theme_cowplot()+theme(legend.position = "none")
P43


load("~/susie_small_sample/simulations/small_sample_susie75_h25.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie75_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P51 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  ggtitle( expression(n == 75) )+
  theme_cowplot()+theme(legend.position = "none")
P51

load("~/susie_small_sample/simulations/small_sample_susie75_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie75_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P52 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.3")+
  theme_cowplot()+theme(legend.position = "none")
P52




load("~/susie_small_sample/simulations/small_sample_susie75_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie75_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P53 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  theme_cowplot()+theme(legend.position = "none")
P53




load("~/susie_small_sample/simulations/small_sample_susie100_h25.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie100_h25.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P61 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  ggtitle( expression(n == 100) )+
  theme_cowplot()+theme(legend.position = "none")
P61

load("~/susie_small_sample/simulations/small_sample_susie100_h30.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie100_h30.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P62 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+
  theme_cowplot()+theme(legend.position = "none")
P62




load("~/susie_small_sample/simulations/small_sample_susie100_h50.RData")
obs_size <- rep( NA, 10)
for ( i in 1:10){
  obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie100_h50.RData")
cor_obs_size <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_size[i] <-  mean( temp[which(temp[,3] == (i )),4] )
}


df <- data.frame(obs_size =c(obs_size, cor_obs_size),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P63 <- ggplot( df, aes(y=obs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
 ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  theme_cowplot()+theme(legend.position = "none")
P63




grid.arrange(P11, P21, P31,P41,P51, P61,
             P12, P22, P32,P42,P52, P62,
             P13, P23, P33,P43,P53, P63,
             ncol=6)
grid.arrange(  P31,P41,P51, P61,
               P32,P42,P52, P62,
              P33,P43,P53, P63,
            ncol=4)
