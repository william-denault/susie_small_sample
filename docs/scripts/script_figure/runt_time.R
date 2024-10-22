library(ggplot2)

load("~/susie_small_sample/simulations/run_time.RData")
ggplot(res, aes(x=Method, y=time))+
  geom_boxplot()+
  scale_y_log10()+
 # facet_wrap(.~N)+
  ggtitle("Comparison of computation time (seconds)")
