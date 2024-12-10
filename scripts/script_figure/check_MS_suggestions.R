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

tdf= df[ which(df$purity>0.5),]
library(dplyr)

tdf%>%group_by(SER,n_effect  )%>%
  summarise(cov= mean(n_true_cs/n_cs))









ggplot(df, aes(mean_cs_size, sigma, colour = SER ))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
ggplot(df, aes(purity, sigma, colour = SER ))+
  geom_point()+
  scale_y_log10()




ggplot(df, aes(purity, mean_cs_size, colour = SER ))+
  geom_point()+
  scale_y_log10()




