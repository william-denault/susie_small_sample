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
   tt = readRDS(path)
   temp=  do.call(rbind, lapply( 1:length( tt) , function(j ) tt[[j]]$summary_reg))

  temp= as.data.frame(temp)

  obs_cov <-      sum(temp$n_true_cs) / sum(temp$n_cs)
  cs_size <-  median( temp$mean_cs_size)

  # Calculate credible set size (assuming it's in column 4 of the dataset)


  power <-      sum(temp$n_true_cs )/  sum(temp$n_effect)


  return(list(obs_cov = obs_cov, cs_size = cs_size, power= power))
}

# Function to calculate error bars for each observation
calculate_error <- function(est_cov, n = 10) {
  1.96 * sqrt((est_cov * (1 - est_cov)) / n)
}

# Load and process all datasets
h2_values <- c(25, 30, 50, 75)
n_values <- c( 20, 30, 50, 70, 100)
bf_labels <- c("SER Gaus", "SER SS" )

data_list <- list()

for (n in n_values) {
  print(paste('n=', n))
  for (h2 in h2_values) {
    print(paste('h2=', h2))
    susie_data <- load_and_calculate_cov_and_cs(paste0( getwd(),"/simulations/small_sample_susie" , n, '_h', h2, "_unfilter_L1.rds"))
    cor_data <- load_and_calculate_cov_and_cs(paste0( getwd(),"/simulations/cor_small_sample_susie" , n, '_h', h2,  "_unfilter_L1.rds"))
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


    # Calculate error bars

    data_list[[paste0("n", n, "_h2_", h2)]] <- df
  }
}
# Proper custom labeller for LaTeX-style labels
custom_labeller <- labeller(
  n = label_both,
  h2 = function(h2) {
    paste0("h2 = ", h2, "%")
  }
)



combined_data_L1 <- bind_rows(data_list)
ggplot(combined_data_L1, aes(y = obs_cov, x =  BF , col = BF)) +
  geom_point () +
  #geom_errorbar(aes(ymin = obs_cov - error, ymax = obs_cov + error), width = 0.2, position = position_dodge) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y") +  # Switch `h2` labels to the right-hand side
  ylim(0. , 1.5) +
  xlab(" Number of effect ") +
  ylab(expression(h^2)) +
  ggtitle("Coverage")+
  theme_cowplot() +
  theme(
    strip.background = element_blank(),  # Remove grey boxes
    strip.placement = "outside",         # Place facet labels outside plot area
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.right = element_text(size = 12, face = "bold"),  # Right-hand side for `h^2`
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),      # Remove `h^2` on the left
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(col = "Model")















#### CS coverage -----
library(ggplot2)
P11 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==25),], aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  xlab(   "" )+
  ylab(expression(h^2 == 25*"%"))+
  ggtitle(  expression(n == 10)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P11
library(ggplot2)
P12 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==30),],
               aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 30*"%"))+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P12

library(ggplot2)
P13 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==50),],
               aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 50*"%"))+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P13
library(ggplot2)
P14 <- ggplot( combined_data_L1[which( combined_data_L1$n==10 & combined_data_L1$h2==75),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 75*"%"))+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P14
library(ggplot2)
P21 <- ggplot( combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==25),],
               aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(expression(h^2 == 25*"%"))+
  xlab(' ')+
  # ggtitle(  expression(n == 20)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P21
library(ggplot2)
P22 <- ggplot( combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==30),], aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(expression(h^2 == 30*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P22
library(ggplot2)
P23 <- ggplot(  combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==50),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(expression(h^2 == 50*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P23
library(ggplot2)
P24 <- ggplot(  combined_data_L1[which( combined_data_L1$n==20 & combined_data_L1$h2==75),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(expression(h^2 == 75*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P24
library(ggplot2)
P31 <- ggplot(  combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==25),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  # ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P31
library(ggplot2)
P32 <- ggplot(  combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==30),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  #ylab(expression(h^2 == 30*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P32
library(ggplot2)
P33 <- ggplot( combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==50),],
               aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+

  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P33
library(ggplot2)
P34 <- ggplot(  combined_data_L1[which( combined_data_L1$n==30 & combined_data_L1$h2==75),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  # ylab(expression(h^2 == 75*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P34
library(ggplot2)
P41 <- ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==25),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  # ggtitle(  expression(n == 50)  )+


  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P41
P42 <- ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==30),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P42
P43 <-  ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==50),],
                 aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P43

library(ggplot2)
P44 <- ggplot(  combined_data_L1[which( combined_data_L1$n==50 & combined_data_L1$h2==75),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P44
library(ggplot2)
P51 <- ggplot(  combined_data_L1[which( combined_data_L1$n==70 & combined_data_L1$h2==25),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle(  expression(n == 75)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P51
library(ggplot2)
P52 <- ggplot(  combined_data_L1[which( combined_data_L1$n==70 & combined_data_L1$h2==30),],
                aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P52

library(ggplot2)
P53 <- ggplot( combined_data_L1[which( combined_data_L1$n==70 & combined_data_L1$h2==50),],
               aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P53
library(ggplot2)
P54 <- ggplot(  combined_data_L1[which( combined_data_L1$n==70 & combined_data_L1$h2==75),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P54

library(ggplot2)
P61 <- ggplot(  combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==25),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  # ggtitle(  expression(n == 100)  )+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P61
library(ggplot2)
P62 <- ggplot(  combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==30),],  aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P62
library(ggplot2)
P63 <- ggplot(combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==50),], aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P63

library(ggplot2)
P64 <- ggplot( combined_data_L1[which( combined_data_L1$n==100 & combined_data_L1$h2==75),], aes(y = obs_cov, x =  BF, col=BF))+
  geom_point(size=3
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+geom_hline(yintercept = 1, linetype=2)+
  ylim( c(0.9,1))+theme_cowplot()+theme(legend.position = "none")

P64
library(gridExtra)


P_coverage =grid.arrange(
  arrangeGrob(grobs = titles, ncol = 5),
  arrangeGrob(  P21, P31,P41,P51,P61,
                P22, P32,P42,P52,P62,
                P23, P33,P43,P53,P63,
                P24, P34,P44,P54,P64,
                ncol=5),
  heights = c(0.03, 1),
  top = textGrob("Observed coverage for different SER (L=1) no purity filter",
                                      gp = gpar(fontsize = 14, fontface = "bold"),
                                      just = "left",
                                      x = 0.02)   # Adjust height ratio to bring titles closer
) # Adjust height ratio to bring titles closer


P_coverage
ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_coverage_L1_unfilter.pdf",
       plot = P_coverage,
       width = 320,
       height = 210,
       units = "mm")

