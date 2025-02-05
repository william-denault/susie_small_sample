rm(list=ls())
library(cowplot)
library(gridExtra)

purity=0.5
summary_sim = function(temp, i,idx){


  obs_cov =  Reduce("+",
                    sapply(idx, function(k){

                      ifelse( length(which(temp[[i]]$true_pos%in%temp[[i]]$cs$cs[[k]] ))==0, 0,1)
                    }


                    )
  )/length(idx)
  power=  length(
    which( temp[[i]]$true_pos %in% do.call(c,
                                           lapply(idx, function( k)
                                             temp[[i]]$cs$cs[[k]])))
  )/length(temp[[i]]$true_pos)

  cs_size= median(lengths(temp[[i]]$cs$cs[ idx ]))

  return( c(obs_cov, power, cs_size, length(temp[[i]]$true_pos)))

}

get_df_sim = function( temp, purity){
  res  =list ()
  h=1
  for ( i in 1:length( temp)){


    idx  = which(temp[[i]]$cs$purity[,1] >purity)
    if( length(idx)==0){

    }else{



      res  [[h]] =summary_sim(temp,i=i, idx=idx)
      h=h+1
    }

  }

  res = data.frame( do.call( rbind,res ))
  colnames(res )= c("obs_cov", "power", "cs_size","n_effect")

  res= res %>%group_by(n_effect)%>%
    summarise(cov= mean (obs_cov),
              power=mean(power),
              cs_size=mean(cs_size))
  return( res)

}
library(dplyr)
library(ggplot2)
seq_purity =c( 0.1 , 0.2, 0.3,  0.4,  0.5)

h2_values <- c(25, 30, 50, 75)
n_values <- c(10, 20, 30, 50, 70, 100)
bf_labels <- c("SER Gaus", "SER SS" )


### n===10 ------

n=10
h2=25

plot_cov=list()
plot_pow=list()
plot_cs=list()
o=1


if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=10
h2=30



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=10
h2=50



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1





n=10
h2=75



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1





### n===20 ------

n=20
h2=25



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=20
h2=30



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=20
h2=50



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



n=20
h2=75



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1






### n===30 ------

n=30
h2=25


if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=30
h2=30



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=30
h2=50



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



n=30
h2=75



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1





### n===50 ------

n=50
h2=25


if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=50
h2=30



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=50
h2=50



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



n=50
h2=75



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1




### n===75 ------

n=75
h2=25


if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=75
h2=30



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=75
h2=50



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



n=75
h2=75



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



### n===100 ------

n=100
h2=25



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=100
h2=30



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1


n=100
h2=50



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



n=100
h2=75



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp= readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/small_sample_susie",
                      n,
                      "_h",
                      h2,
                      "_unfilter.rds",sep=""))


  res_susie= get_df_sim (temp, purity= purity)
}



if(file.exists(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                     n,
                     "_h",
                     h2,
                     "_unfilter.rds",sep=""))){
  temp=  readRDS(paste("/project2/mstephens/wdenault/susie_small_sample/simulations/cor_small_sample_susie",
                       n,
                       "_h",
                       h2,
                       "_unfilter.rds",sep=""))

  res_susie_SS= get_df_sim (temp, purity= purity)
}else{res_susie_SS=res_susie}



res_susie$label= "SER Gaus"
res_susie_SS$label=  "SER SS"

res= rbind(res_susie,res_susie_SS)
plot_cov[[o]]=  ggplot(res, aes(x=n_effect, y=cov, col=label))+
  geom_point()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+ggtitle(paste("n= ",n, "h2= ",h2))+
  ylim(c(0.4 ,1))
plot_pow[[o]]=  ggplot(res, aes(x=n_effect, y=power, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")+
  ylim(c(0  ,1))
plot_cs[[o]]=  ggplot(res, aes(x=n_effect, y=cs_size, col=label))+
  geom_point()+ ggtitle(paste("n= ",n, "h2= ",h2))+
  theme(legend.position="none")
o=o+1



# Rearrange P_cov and P_cs to align with the structure of P_power

P_power = grid.arrange(plot_pow[[1]], plot_pow[[5]], plot_pow[[9]], plot_pow[[13]], plot_pow[[17]], plot_pow[[21]],
                       plot_pow[[2]], plot_pow[[6]], plot_pow[[10]], plot_pow[[14]], plot_pow[[18]], plot_pow[[22]],
                       plot_pow[[3]], plot_pow[[7]], plot_pow[[11]], plot_pow[[15]], plot_pow[[19]], plot_pow[[23]],
                       plot_pow[[4]], plot_pow[[8]], plot_pow[[12]], plot_pow[[16]], plot_pow[[20]], plot_pow[[24]],
                       ncol=6)

P_cov = grid.arrange(plot_cov[[1]], plot_cov[[5]], plot_cov[[9]], plot_cov[[13]], plot_cov[[17]], plot_cov[[21]],
                     plot_cov[[2]], plot_cov[[6]], plot_cov[[10]], plot_cov[[14]], plot_cov[[18]], plot_cov[[22]],
                     plot_cov[[3]], plot_cov[[7]], plot_cov[[11]], plot_cov[[15]], plot_cov[[19]], plot_cov[[23]],
                     plot_cov[[4]], plot_cov[[8]], plot_cov[[12]], plot_cov[[16]], plot_cov[[20]], plot_cov[[24]],
                     ncol=6)

P_cs = grid.arrange(plot_cs[[1]], plot_cs[[5]], plot_cs[[9]], plot_cs[[13]], plot_cs[[17]], plot_cs[[21]],
                    plot_cs[[2]], plot_cs[[6]], plot_cs[[10]], plot_cs[[14]], plot_cs[[18]], plot_cs[[22]],
                    plot_cs[[3]], plot_cs[[7]], plot_cs[[11]], plot_cs[[15]], plot_cs[[19]], plot_cs[[23]],
                    plot_cs[[4]], plot_cs[[8]], plot_cs[[12]], plot_cs[[16]], plot_cs[[20]], plot_cs[[24]],
                    ncol=6)


ggsave(paste( "/project2/mstephens/wdenault/susie_small_sample/plots/P_power_purity" ,purity ,".pdf", sep=""),
       plot = P_power ,
       width = 320,
       height = 210,
       units = "mm")
ggsave(paste( "/project2/mstephens/wdenault/susie_small_sample/plots/P_cov_purity" ,purity ,".pdf", sep=""),
       plot = P_cov ,
       width = 320,
       height = 210,
       units = "mm")
ggsave(paste( "/project2/mstephens/wdenault/susie_small_sample/plots/P_cs_purity" ,purity ,".pdf", sep=""),
       plot = P_cs ,
       width = 320,
       height = 210,
       units = "mm")

