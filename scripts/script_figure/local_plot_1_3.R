library(ggplot2)
library(cowplot)
load("D:/Document/Serieux/Travail/Package/susie_small_sample/simulations/summary_L1_3.RData")
combined_data= combined_data[-which(combined_data$n==10),]


# Proper custom labeller for LaTeX-style labels
custom_labeller <- labeller(
  n = label_both,
  h2 = function(h2) {
    paste0("h2 = ", h2, "%")
  }
)

# Add position dodge for separating points and error bars
position_dodge <- position_dodge(width = 0.4)

# Create the ggplot using facet_grid with enhanced labels and layout
plot <- ggplot(combined_data, aes(y = obs_cov, x = as.factor(L), col = BF)) +
  geom_point(position = position_dodge, size = 2) +
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

print(plot)


ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_coverage_1_3.pdf",
       plot =plot,
       width = 320,
       height = 210,
       units = "mm")


# Create the ggplot using facet_grid with enhanced labels and layout
plot <- ggplot(combined_data, aes(y = cs_size, x = as.factor(L), col = BF)) +
  geom_point(position = position_dodge, size = 2) +


  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y",scales = "free_y") +  # Switch `h2` labels to the right-hand side
  ggtitle("CS size")+
  xlab(" Number of effect ") +
  ylab(expression(h^2)) +
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

print(plot)


ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_CSsize_1_3.pdf",
       plot =plot,
       width = 320,
       height = 210,
       units = "mm")




plot <- ggplot(combined_data, aes(y = power, x = as.factor(L), col = BF)) +
  geom_point(position = position_dodge, size = 2) +


  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y",scales = "free_y") +  # Switch `h2` labels to the right-hand side
  ggtitle("CS size")+
  xlab(" Number of effect ") +
  ylab(expression(h^2)) +
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

print(plot)




#### CS coverage -----
library(ggplot2)
P11 <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==25),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  xlab(   "" )+
  ylab(expression(h^2 == 25*"%"))+
  ggtitle(  expression(n == 10)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P11
library(ggplot2)
P12 <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==30),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 30*"%"))+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P12

library(ggplot2)
P13 <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==50),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 50*"%"))+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P13
library(ggplot2)
P14 <- ggplot(combined_data[which(combined_data$n==10 & combined_data$h2==75),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 75*"%"))+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P14
library(ggplot2)
P21 <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 25*"%"))+
  xlab(' ')+
  ggtitle(  expression(n == 20)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P21
library(ggplot2)
P22 <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==30),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 30*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P22
library(ggplot2)
P23 <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==50),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 50*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P23
library(ggplot2)
P24 <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==75),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 75*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P24
library(ggplot2)
P31 <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P31
library(ggplot2)
P32 <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==30),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  #ylab(expression(h^2 == 30*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P32
library(ggplot2)
P33 <- ggplot( combined_data[which(combined_data$n==30 & combined_data$h2==50),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+

  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P33
library(ggplot2)
P34 <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==75),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ylab(expression(h^2 == 75*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P34
library(ggplot2)
P41 <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 50)  )+


  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P41
P42 <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==30),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P42
P43 <-  ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==50),],
                 aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P43

library(ggplot2)
P44 <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==75),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P44
library(ggplot2)
P51 <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==25),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 75)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P51
library(ggplot2)
P52 <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==30),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P52

library(ggplot2)
P53 <- ggplot( combined_data[which(combined_data$n==75 & combined_data$h2==50),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P53
library(ggplot2)
P54 <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==75),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P54

library(ggplot2)
P61 <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==25),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 100)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P61
library(ggplot2)
P62 <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==30),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P62
library(ggplot2)
P63 <- ggplot(combined_data[which(combined_data$n==100 & combined_data$h2==50),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P63

library(ggplot2)
P64 <- ggplot( combined_data[which(combined_data$n==100 & combined_data$h2==75),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none")

P64
library(gridExtra)

P_coverage = grid.arrange(  P21, P31,P41,P51,P61,
                            P22, P32,P42,P52,P62,
                            P23, P33,P43,P53,P63,
                            P24, P34,P44,P54,P64,
                          ncol=5)

P_coverage
ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_coverage_1_3.pdf",
       plot = P_coverage,
       width = 320,
       height = 210,
       units = "mm")




#### CS_size  ----




library(ggplot2)
P11_cs <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==25),], aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  xlab(  expression(n == 10)  )+
  ylab(expression(h^2 == 25*"%"))+
  ggtitle(  expression(n == 10)  )+

  theme_cowplot()+theme(legend.position = "none")

P11_cs
library(ggplot2)
P12_cs  <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==30),],
                   aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 30*"%"))+

  theme_cowplot()+theme(legend.position = "none")

P12_cs

library(ggplot2)
P13_cs  <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==50),],
                   aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 50*"%"))+

  theme_cowplot()+theme(legend.position = "none")

P13_cs
library(ggplot2)
P14_cs  <- ggplot(combined_data[which(combined_data$n==10 & combined_data$h2==75),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 75*"%"))+

  theme_cowplot()+theme(legend.position = "none")

P14_cs
library(ggplot2)
P21_cs  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
                   aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 25*"%"))+
  xlab(' ')+
  ggtitle(  expression(n == 20)  )+

  theme_cowplot()+theme(legend.position = "none")

P21_cs
library(ggplot2)
P22_cs  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==30),], aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 30*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+

  theme_cowplot()+theme(legend.position = "none")

P22_cs
library(ggplot2)
P23_cs  <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==50),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 50*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P23_cs
library(ggplot2)
P24_cs  <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==75),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 75*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P24_cs
library(ggplot2)
P31_cs  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+

  theme_cowplot()+theme(legend.position = "none")

P31_cs
library(ggplot2)
P32_cs  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==30),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  #ylab(expression(h^2 == 30*"%"))+
  xlab("")+
  ylab(' ')+

  theme_cowplot()+theme(legend.position = "none")

P32_cs
library(ggplot2)
P33_cs  <- ggplot( combined_data[which(combined_data$n==30 & combined_data$h2==50),],
                   aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+

  xlab("")+
  ylab(' ')+

  theme_cowplot()+theme(legend.position = "none")

P33_cs
library(ggplot2)
P34_cs  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==75),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ylab(expression(h^2 == 75*"%"))+
  xlab("")+
  ylab(' ')+

  theme_cowplot()+theme(legend.position = "none")

P34_cs
library(ggplot2)
P41_cs  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 50)  )+



  theme_cowplot()+theme(legend.position = "none")

P41_cs
P42_cs  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==30),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+

  theme_cowplot()+theme(legend.position = "none")

P42_cs
P43_cs  <-  ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==50),],
                     aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P43_cs

library(ggplot2)
P44_cs  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==75),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P44_cs
library(ggplot2)
P51_cs  <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==25),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 75)  )+

  theme_cowplot()+theme(legend.position = "none")

P51_cs
library(ggplot2)
P52_cs  <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==30),],
                    aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.3")+

  theme_cowplot()+theme(legend.position = "none")

P52_cs

library(ggplot2)
P53_cs  <- ggplot( combined_data[which(combined_data$n==75 & combined_data$h2==50),],
                   aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P53_cs
library(ggplot2)
P54_cs  <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==75),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P54_cs

library(ggplot2)
P61_cs  <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==25),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 100)  )+

  theme_cowplot()+theme(legend.position = "none")

P61_cs
library(ggplot2)
P62_cs  <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==30),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+

  theme_cowplot()+theme(legend.position = "none")

P62_cs
library(ggplot2)
P63_cs  <- ggplot(combined_data[which(combined_data$n==100 & combined_data$h2==50),], aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P63_cs

library(ggplot2)
P64_cs  <- ggplot( combined_data[which(combined_data$n==100 & combined_data$h2==75),], aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P64_cs

library(gridExtra)
P_cs_size = grid.arrange( P21_cs , P31_cs ,P41_cs ,P51_cs ,P61_cs ,
                           P22_cs , P32_cs ,P42_cs ,P52_cs ,P62_cs ,
                           P23_cs , P33_cs ,P43_cs ,P53_cs ,P63_cs ,
                          P24_cs , P34_cs ,P44_cs ,P54_cs ,P64_cs ,
                         ncol=5)


P_cs_size
ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_cs_size_1_3.pdf",
       plot = P_cs_size ,
       width = 320,
       height = 210,
       units = "mm")



P_L1_cov_L10 = ggplot(combined_data[which(combined_data$L==1),], aes( x= BF , y=obs_cov,
                                                                      color= BF ))+
  facet_grid(h2~n,labeller = custom_labeller)+
  geom_point()+ theme_minimal()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0.95)+
  ylab("Observed coverage")+
  xlab("")+
  ggtitle("Observed coverage for L=1 \n estimated L")

ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_L_1_cov_L3.pdf",
       plot = P_L1_cov_L10 ,
       width = 320,
       height = 210,
       units = "mm")
combined_data$L = as.factor(combined_data$L)

P_power <- ggplot(combined_data, aes(y = power, x = as.factor(L), col = BF)) +
  geom_point(  size = 2) +
  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y") +
  theme(legend.title = element_blank(),
        panel.grid.major = element_line(color = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25)
  )+scale_y_log10()
P_power

ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_power_1_3.pdf",
       plot = P_power ,
       width = 320,
       height = 210,
       units = "mm")







#### purity  ----




library(ggplot2)
P11_purity <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==25),], aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  xlab(  expression(n == 10)  )+
  ylab(expression(h^2 == 25*"%"))+
  ggtitle(  expression(n == 10)  )+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P11_purity
library(ggplot2)
P12_purity  <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==30),],
                       aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 30*"%"))+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P12_purity

library(ggplot2)
P13_purity  <- ggplot( combined_data[which(combined_data$n==10 & combined_data$h2==50),],
                       aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 50*"%"))+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P13_purity
library(ggplot2)
P14_purity  <- ggplot(combined_data[which(combined_data$n==10 & combined_data$h2==75),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 75*"%"))+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P14_purity
library(ggplot2)
P21_purity  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
                       aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 25*"%"))+
  xlab(' ')+
  ggtitle(  expression(n == 20)  )+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P21_purity
library(ggplot2)
P22_purity  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==30),], aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 30*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P22_purity
library(ggplot2)
P23_purity  <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==50),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 50*"%"))+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P23_purity
library(ggplot2)
P24_purity  <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==75),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression(h^2 == 75*"%"))+
  xlab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P24_purity
library(ggplot2)
P31_purity  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P31_purity
library(ggplot2)
P32_purity  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==30),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  #ylab(expression(h^2 == 30*"%"))+
  xlab("")+
  ylab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P32_purity
library(ggplot2)
P33_purity  <- ggplot( combined_data[which(combined_data$n==30 & combined_data$h2==50),],
                       aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+

  xlab("")+
  ylab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P33_purity
library(ggplot2)
P34_purity  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==75),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ylab(expression(h^2 == 75*"%"))+
  xlab("")+
  ylab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P34_purity
library(ggplot2)
P41_purity  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 50)  )+

  ylim(0.9, 1) +

  theme_cowplot()+theme(legend.position = "none")

P41_purity
P42_purity  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==30),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P42_purity
P43_purity  <-  ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==50),],
                         aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P43_purity

library(ggplot2)
P44_purity  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==75),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P44_purity
library(ggplot2)
P51_purity  <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==25),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 75)  )+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P51_purity
library(ggplot2)
P52_purity  <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==30),],
                        aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.3")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P52_purity

library(ggplot2)
P53_purity  <- ggplot( combined_data[which(combined_data$n==75 & combined_data$h2==50),],
                       aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P53_purity
library(ggplot2)
P54_purity  <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==75),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P54_purity

library(ggplot2)
P61_purity  <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==25),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 100)  )+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P61_purity
library(ggplot2)
P62_purity  <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==30),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P62_purity
library(ggplot2)
P63_purity  <- ggplot(combined_data[which(combined_data$n==100 & combined_data$h2==50),], aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P63_purity

library(ggplot2)
P64_purity  <- ggplot( combined_data[which(combined_data$n==100 & combined_data$h2==75),], aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none")

P64_purity

library(gridExtra)
P_purity = grid.arrange(  P21_purity , P31_purity ,P41_purity ,P51_purity ,P61_purity ,
                          P22_purity , P32_purity ,P42_purity ,P52_purity ,P62_purity ,
                          P23_purity , P33_purity ,P43_purity ,P53_purity ,P63_purity ,
                          P24_purity , P34_purity ,P44_purity ,P54_purity ,P64_purity ,
                        ncol=5)




ggsave("D:/Document/Serieux/Travail/Package/susie_small_sample/plots/P_purity_1_3.pdf",
       plot =
         P_purity ,
       width = 320,
       height = 210,
       units = "mm")

