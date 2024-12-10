load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/n10.RData")
res
table(res$is.dummy)
tres=res[which(!res$is.dummy),]
#coverage n=10
sum(tres[,1])/sum(tres[,2])



load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/n20.RData")
res
table(res$is.dummy)
tres=res[which(!res$is.dummy),]
#coverage n=20
sum(tres[,1])/sum(tres[,2])




load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/n30.RData")
res
table(res$is.dummy)
tres=res[which(!res$is.dummy),]
#coverage n=30
sum(tres[,1])/sum(tres[,2])




load("C:/Document/Serieux/Travail/Package/susie_small_sample/local_sim/n40.RData")
res
table(res$is.dummy)
tres=res[which(!res$is.dummy),]
#coverage n=30
sum(tres[,1])/sum(tres[,2])
