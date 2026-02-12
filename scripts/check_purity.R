
n=30

h2=50

temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                    n,
                    "_h",
                    h2,
                    "_unfilter.rds",sep=""))
tt= do.call(c, sapply(1:length(temp),function(i)   temp[[i]]$cs$purity[ ,1]))

 length( which(tt>0.2))


 length( which(tt>0.5))
 library(ggplot2)
 df= data.frame(purity=tt )

 ggplot( df, aes(x=purity+0.0001))+geom_histogram()+

   xlim( c(0.0001,1))+scale_x_log10()+
   ggtitle("distribution of the purity +1e-4 \n log10scale" )

 df= data.frame(purity=tt[which(tt>0.0001)] )
 ggplot( df, aes(x=purity))+geom_histogram()+
   scale_x_log10()+
  ggtitle("distribution of the purity for CS with purity large than 1e-4 " )





 temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))
 tt= do.call(c, sapply(1:length(temp),function(i)   temp[[i]]$cs$purity[ ,1]))

 length( which(tt>0.2))


 length( which(tt>0.5))
 library(ggplot2)
 df= data.frame(purity=tt )

 ggplot( df, aes(x=purity+0.0001))+geom_histogram()+

   xlim( c(0.0001,1))+scale_x_log10()+
   ggtitle("distribution of the purity +1e-4 \n log10scale" )


 df= data.frame(purity=tt[which(tt>0.0001)] )
 ggplot( df, aes(x=purity))+geom_histogram()+
    scale_x_log10()+
   geom_vline(xintercept = 0.5)+
   ggtitle("distribution of the purity for CS with purity large than 1e-4 \n balck line correspond to purity of 0.5" )

