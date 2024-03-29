 
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie10_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P11 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=10, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P11

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie10_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P12 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=10, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P12




load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie10_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P13 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=10, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P13






load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie20_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P21 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=20, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P21

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie20_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P22 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=20, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P22




load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie20_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P23 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P23




load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie50_h25.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P31 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=50, h=0.25")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P31

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie50_h30.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P32 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=50, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P32




load("C:/Document/Serieux/Travail/Data_analysis_and_papers/small_susie/small_sample_susie50_h50.RData")
obs_cov <- rep( NA, 10)
for ( i in 1:10){
  obs_cov[i] <- sum( temp[which(temp[,3] == (i )),1] )/sum( temp[which(temp[,3] == (i )),2] )
}
plot( obs_cov)


df <- data.frame(obs_cov =obs_cov,
                 L= 1:10)

library(ggplot2)
P33 <- ggplot( df, aes(y=obs_cov, x=as.factor(L)))+
  geom_point(
  )+
  ylab('observed coverage')+
  xlab('Number of effect in simulation')+
  ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))
abline( h=0.95)
P33

library(gridExtra)

grid.arrange(P11, P21, P31,
             P12, P22, P32,
             P31, P23, P33, ncol=3) 
