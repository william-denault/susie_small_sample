load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/unfilter_n30h25_susie_small.RData")
cor_small= as.data.frame (res)

 load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/unfilter_n30h25_susie.RData")
susie_gaus= as.data.frame (res)

df = data.frame(cbind(rbind(cor_small, susie_gaus),c(
                                                rep("SS_SER",nrow(cor_small)),
                                                rep("Gaus_SER", nrow(susie_gaus)))
                )
          )





library(ggplot2)
colnames(df)[ncol(df)]= "SER"



#sanity check

tdf= df[ which(df$purity>0.5  ),]
library(dplyr)

tdf%>%group_by(SER,n_effect  )%>%
  summarise(cov= mean(n_true_cs/n_cs), n=n())


ggplot(tdf, aes(purity, mean_cs_size, colour = SER ))+
  geom_point()+
  scale_y_log10()
ggplot(tdf, aes(purity, sigma0, colour = SER ))+
  geom_point()+
  scale_y_log10()


ggplot(df[which(df$n_effect==1 ),], aes(  purity, colour = SER ))+
  geom_histogram()+facet_wrap(.~SER)


ggplot(df[which(df$n_effect==1 ),], aes(  purity, fill= SER ))+
geom_histogram(position = "identity", alpha = 0.5, bins = 200)

ggplot(df[which(df$n_effect==1 ),], aes(  sigma0, fill= SER ))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 200)

ggplot(df[which(df$n_effect==1 ),], aes(  log10(sigma0), fill= SER ))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 200)




ggplot(df[which(df$n_effect==1 ),], aes( sigma0, fill = SER ))+
  geom_histogram(bins = 200)+facet_wrap(.~SER)

ggplot(df[which(df$n_effect==1 ),], aes( sigma0, fill = SER ))+
  geom_density()+facet_wrap(.~SER)




ggplot(df[which(df$n_effect==1 ),], aes(mean_cs_size, sigma, colour = SER ))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
ggplot(df[which(df$n_effect==1 ),], aes(purity, sigma, colour = SER ))+
  geom_point()+
  scale_y_log10()
ggplot(df[which(df$n_effect==1 ),], aes(  log10(sigma), colour = SER ))+
  geom_histogram()+facet_wrap(.~SER)
ggplot(df[which(df$n_effect==1 ),], aes(  mean_cs_size, colour = SER ))+
  geom_histogram()+facet_wrap(.~SER)
ggplot(df[which(df$n_effect==1 ),], aes(  purity, colour = SER ))+
  geom_histogram()+facet_wrap(.~SER)

ggplot(df, aes(purity, mean_cs_size, colour = SER ))+
  geom_point()+
  scale_y_log10()




