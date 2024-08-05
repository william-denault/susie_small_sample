

##### 10 ----
load("~/susie_small_sample/simulations/small_sample_susie10_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)



load("~/susie_small_sample/simulations/cor_small_sample_susie10_h25.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P11 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=10, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P11

load("~/susie_small_sample/simulations/small_sample_susie10_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


load("~/susie_small_sample/simulations/cor_small_sample_susie10_h30.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P12 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=10, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P12




load("~/susie_small_sample/simulations/small_sample_susie10_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)

load("~/susie_small_sample/simulations/cor_small_sample_susie10_h50.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P13 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=10, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P13




##### 20 ----


load("~/susie_small_sample/simulations/small_sample_susie20_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)

load("~/susie_small_sample/simulations/cor_small_sample_susie20_h25.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P21 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=20, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P21

load("~/susie_small_sample/simulations/small_sample_susie20_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


load("~/susie_small_sample/simulations/cor_small_sample_susie20_h30.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P22 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=20, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P22




load("~/susie_small_sample/simulations/small_sample_susie20_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)



load("~/susie_small_sample/simulations/cor_small_sample_susie20_h50.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))

library(ggplot2)
P23 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P23


##### 30 ----

load("~/susie_small_sample/simulations/small_sample_susie30_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


load("~/susie_small_sample/simulations/cor_small_sample_susie30_h25.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P31 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=30, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P31

load("~/susie_small_sample/simulations/small_sample_susie30_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


load("~/susie_small_sample/simulations/cor_small_sample_susie30_h30.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P32 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=30, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P32




load("~/susie_small_sample/simulations/small_sample_susie30_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie30_h50.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P33 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=30, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P33

library(gridExtra)

#### 50 ----



load("~/susie_small_sample/simulations/small_sample_susie50_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h25.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P41 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=50, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P41

load("~/susie_small_sample/simulations/small_sample_susie50_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h30.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P42 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=50, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P42




load("~/susie_small_sample/simulations/small_sample_susie50_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie50_h50.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P43 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P43










load("~/susie_small_sample/simulations/small_sample_susie75_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie75_h25.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P51 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=75, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P51

load("~/susie_small_sample/simulations/small_sample_susie75_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie75_h30.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P52 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=75, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P52




load("~/susie_small_sample/simulations/small_sample_susie75_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie75_h50.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P53 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P53










load("~/susie_small_sample/simulations/small_sample_susie100_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie100_h25.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P61 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=100, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P61

load("~/susie_small_sample/simulations/small_sample_susie100_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie100_h30.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P62 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=100, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P62




load("~/susie_small_sample/simulations/small_sample_susie100_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
load("~/susie_small_sample/simulations/cor_small_sample_susie100_h50.RData")
cor_obs_cov <- rep( NA, 10)
for ( i in 1:10){
  cor_obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}


df <- data.frame(obs_cov =c(obs_cov, cor_obs_cov),
                 BF = factor(rep(c("Wakefeild", "Student" ), each = length(1:10))),
                 L= rep(1:10,2))


library(ggplot2)
P63 <- ggplot( df, aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme(legend.position = "none")
abline( h=0.95)
P63



library(gridExtra)









library(gridExtra)






grid.arrange(P11, P21, P31,P41,P51,P61,
             P12, P22, P32,P42,P52,P62,
             P13, P23, P33,P43,P53,P63,
             ncol=6)

