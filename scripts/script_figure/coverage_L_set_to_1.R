library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(cowplot)
library(dplyr)
titles <- lapply(c(20, 30, 50, 75, 100), function(n) {
  textGrob(label = bquote(n == .(n)), gp = gpar(fontsize =16, fontface = "bold"))
})

# Function to load data and calculate observed coverage
load_and_calculate_cov_and_cs <- function(path, num_reps = 1 ) {
  load(path)

  if (length(which(temp$is.dummy==1) )>0){
    temp= temp[- which(temp$is.dummy==1),]
  }


  obs_cov <- sapply(1:num_reps, function(i) {
    sum(temp[which(temp[,3] == i), 1]) / sum(temp[which(temp[,3] == i), 2])
  })

  # Calculate credible set size (assuming it's in column 4 of the dataset)
  cs_size <- sapply(1:num_reps, function(i) {
    median(temp[which(temp[,3] == i), 4])
  })

  power <- sapply(1:num_reps,  function(i) {
    sum(temp$n_true_cs[which( temp$is.dummy==0 & temp$n_effect==i )])/ sum(temp$n_effect[which( temp$n_effect==i )])
  })


  return(list(obs_cov = obs_cov, cs_size = cs_size, power= power))
}

# Function to calculate error bars for each observation
calculate_error <- function(est_cov, n = 10) {
  1.96 * sqrt((est_cov * (1 - est_cov)) / n)
}

# Load and process all datasets
h2_values <- c(25, 30, 50, 75)
n_values <- c(10, 20, 30, 50, 75, 100)
bf_labels <- c("SER Gaus", "SER SS" )

data_list <- list()

for (n in n_values) {
  for (h2 in h2_values) {

    susie_data <- load_and_calculate_cov_and_cs(paste0("../susie_small_sample/simulations/L_1_small_sample_susie", n, "_h", h2, ".RData"))
    cor_data <- load_and_calculate_cov_and_cs(paste0("../susie_small_sample/simulations/L_1_cor_small_sample_susie", n, "_h", h2, ".RData"))
    #  carma_data <- load_and_calculate_cov_and_cs(paste0("../susie_small_sample/simulations/small_sample_CARMA", n, "_h", h2, ".RData"))

    # Combine data into a single data frame for this combination of n and h2
    df <- data.frame(
      obs_cov = c(susie_data$obs_cov, cor_data$obs_cov ),
      cs_size = c(susie_data$cs_size, cor_data$cs_size ),
      power= c(susie_data$power, cor_data$power),
      BF = factor(  bf_labels  ),
      L = rep(1, 2),
      n = rep(n,2),
      h2 = rep(h2,2)
    )



    data_list[[paste0("n", n, "_h2_", h2)]] <- df
  }
}

# Combine all data into a single dataframe
combined_data_L1 <- bind_rows(data_list)

#save(combined_data_L1 , file="../susie_small_sample/simulations/summary_L1.RData")


P_L1_cov_L1 = ggplot(combined_data_L1  , aes( x= BF , y=obs_cov,
                                                       color= BF ))+
  facet_grid(h2~n)+
  geom_point()+ theme_minimal()+
  geom_hline(yintercept = 0.95)+
  ggtitle("Observed coverage for SER (L=1 fixed) ")
print(P_L1_cov_L1)


# Proper custom labeller for LaTeX-style labels
custom_labeller <- labeller(
  n = label_both,
  h2 = function(h2) {
    paste0("h2 = ", h2, "%")
  }
)



P_L1_cov_L1 =ggplot(combined_data_L1 , aes( x= BF , y=obs_cov,
                                                       color= BF ))+
  facet_grid(h2~n,labeller = custom_labeller)+
  geom_point()+ theme_minimal()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+
  ylab("Observed coverage")+
  xlab("")+ ggtitle("Observed coverage for SER (L=1 fixed) ")

#ggsave("../susie_small_sample/plots/P_L_1_cov_L1.pdf",
#       plot = P_L1_cov_L1 ,
#       width = 320,
       #       height = 210,
#       units = "mm")



load( "../susie_small_sample/simulations/summary_L10.RData")

combined_data$L_type= "Estimated"
combined_data_L1$L_type= "Fixed"
df_l1_plot = rbind(combined_data[, c("obs_cov",
                                     "cs_size" ,
                                     "power"  ,
                                     "BF",
                                     "L" ,
                                     "n",
                                     "h2",  "L_type")] , combined_data_L1)
df_l1_plot= df_l1_plot[ which(df_l1_plot$L==1),]

P_L1 <- ggplot(df_l1_plot,
               aes(x = BF,
                   y = obs_cov,
                   color = BF,
                   shape = L_type)) +
  facet_grid(h2 ~ n, labeller = custom_labeller) +
  geom_point(position = position_dodge(width = 0.5), size = 3) + # Add position_dodge to avoid overlap
  theme_minimal() +
  theme() +
  geom_hline(yintercept = 0.95) +
  ylab("Observed coverage") +
  xlab("") +
  ggtitle("Observed coverage for different SER (L=1)") +
  labs(color = NULL, shape = NULL) # Remove legend titles

P_L1




combined_data_L1 = df_l1_plot[-which( df_l1_plot$n==10), ]


#### CS coverage -----
library(ggplot2)
P11 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==25),], aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  xlab(   "" )+
  ylab(expression(h^2 == 25*"%"))+
  ggtitle(  expression(n == 10)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2) +
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P11
library(ggplot2)
P12 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==30),],
               aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 30*"%"))+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P12

library(ggplot2)
P13 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==50),],
               aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 50*"%"))+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P13
library(ggplot2)
P14 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==75),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 75*"%"))+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P14
library(ggplot2)
P21 <- ggplot( combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==25),],
               aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(expression(h^2 == 25*"%"))+
  xlab(' ')+
  # ggtitle(  expression(n == 20)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P21
library(ggplot2)
P22 <- ggplot( combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==30),], aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(expression(h^2 == 30*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P22
library(ggplot2)
P23 <- ggplot(  combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==50),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(expression(h^2 == 50*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P23
library(ggplot2)
P24 <- ggplot(  combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==75),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(expression(h^2 == 75*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P24
library(ggplot2)
P31 <- ggplot(  combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==25),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  # ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P31
library(ggplot2)
P32 <- ggplot(  combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==30),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  #ylab(expression(h^2 == 30*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P32
library(ggplot2)
P33 <- ggplot( combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==50),],
               aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+

  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P33
library(ggplot2)
P34 <- ggplot(  combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==75),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  # ylab(expression(h^2 == 75*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P34
library(ggplot2)
P41 <- ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==25),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  # ggtitle(  expression(n == 50)  )+


  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P41
P42 <- ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==30),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P42
P43 <-  ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==50),],
                 aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P43

library(ggplot2)
P44 <- ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==75),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P44
library(ggplot2)
P51 <- ggplot(  combined_data_L1[which( combined_data_L1$n==75 & combined_data_L1$h2==25),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle(  expression(n == 75)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P51
library(ggplot2)
P52 <- ggplot(  combined_data_L1[which( combined_data_L1$n==75 & combined_data_L1$h2==30),],
                aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P52

library(ggplot2)
P53 <- ggplot( combined_data_L1[which( combined_data_L1$n==75 & combined_data_L1$h2==50),],
               aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P53
library(ggplot2)
P54 <- ggplot(  combined_data_L1[which( combined_data_L1$n==75 & combined_data_L1$h2==75),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P54

library(ggplot2)
P61 <- ggplot(  combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==25),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  # ggtitle(  expression(n == 100)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P61
library(ggplot2)
P62 <- ggplot(  combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==30),],  aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P62
library(ggplot2)
P63 <- ggplot(combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==50),], aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P63

library(ggplot2)
P64 <- ggplot( combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==75),], aes(y = obs_cov, x =  BF,color = BF, shape = L_type))+
  geom_point(position = position_dodge(width = 0.5), size = 3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim(c(min(combined_data_L1$obs_cov),1))+theme_cowplot()+theme(legend.position = "none")

P64
library(gridExtra)


P_coverage =grid.arrange(
  arrangeGrob(grobs = titles, ncol = 5),
  arrangeGrob(  P21, P31,P41,P51,P61,
                P22, P32,P42,P52,P62,
                P23, P33,P43,P53,P63,
                P24, P34,P44,P54,P64,
                ncol=5),
  heights = c(0.03, 1),  # Adjust height ratio to bring titles closer
  top = textGrob("Observed coverage for different SER (L=1) with purity filter",
                 gp = gpar(fontsize = 14, fontface = "bold"),
                 just = "left",
                 x = 0.02)   # Adjust height ratio to bring titles closer
)

P_coverage

 ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_coverage_L1_filter.pdf",
        plot =P_coverage  ,
        width = 320,
        height = 210,
        units = "mm")

