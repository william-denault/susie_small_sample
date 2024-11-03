library(ggplot2)
library(cowplot)
library(dplyr)

# Function to load data and calculate observed coverage
load_and_calculate_cov_and_cs <- function(path, num_reps = 10) {
  load(path)
  temp= temp[- which(temp$is.dummy==1),]
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
bf_labels <- c("SER_Gaus", "SER_SS" )

data_list <- list()

for (n in n_values) {
  for (h2 in h2_values) {

    susie_data <- load_and_calculate_cov_and_cs(paste0("~/susie_small_sample/simulations/small_sample_susie", n, "_h", h2, ".RData"))
    cor_data <- load_and_calculate_cov_and_cs(paste0("~/susie_small_sample/simulations/cor_small_sample_susie", n, "_h", h2, ".RData"))
  #  carma_data <- load_and_calculate_cov_and_cs(paste0("~/susie_small_sample/simulations/small_sample_CARMA", n, "_h", h2, ".RData"))

    # Combine data into a single data frame for this combination of n and h2
    df <- data.frame(
      obs_cov = c(susie_data$obs_cov, cor_data$obs_cov ),
      cs_size = c(susie_data$cs_size, cor_data$cs_size ),
      power= c(susie_data$power, cor_data$power),
      BF = factor(rep(bf_labels, each = length(1:10))),
      L = rep(1:10, 2),
      n = n,
      h2 = h2
    )

    my_n = rep(NA, nrow(df))
    for ( i in 1:2){
      for ( l in 1:10){
        if(i ==i){
          load(paste0("~/susie_small_sample/simulations/small_sample_susie", n, "_h", h2, ".RData"))
          if(length(table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ])>0){
            my_n[which(df $BF =="SER_Gaus" & df$L==l )]=table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ]
          }

        }
        if(i ==2){
          load(paste0("~/susie_small_sample/simulations/cor_small_sample_susie", n, "_h", h2, ".RData"))
          if(length(table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ])>0){
            my_n[which(df $BF =="SER_SS" & df$L==l )]=table(temp$n_effect)[which(as.numeric( names(table(temp$n_effect)) ) ==l ) ]
          }
        }

      }



    }


    # Calculate error bars
    df$error <- calculate_error(df$obs_cov, n =   my_n)

    data_list[[paste0("n", n, "_h2_", h2)]] <- df
  }
}

# Combine all data into a single dataframe
combined_data <- bind_rows(data_list)

# Proper custom labeller for LaTeX-style labels
custom_labeller <- labeller(
  n = label_both,
  h2 = function(h2) {
    paste0("h^2 = ", h2, "%")
  }
)

# Add position dodge for separating points and error bars
position_dodge <- position_dodge(width = 0.4)

# Create the ggplot using facet_grid with enhanced labels and layout
plot <- ggplot(combined_data, aes(y = obs_cov, x = as.factor(L), col = BF)) +
  geom_point(position = position_dodge, size = 2) +
  geom_errorbar(aes(ymin = obs_cov - error, ymax = obs_cov + error), width = 0.2, position = position_dodge) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y") +  # Switch `h2` labels to the right-hand side
  ylim(0. , 1.5) +
  xlab("Replicate (L)") +
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




# Create the ggplot using facet_grid with enhanced labels and layout
plot <- ggplot(combined_data, aes(y = cs_size, x = as.factor(L), col = BF)) +
  geom_point(position = position_dodge, size = 2) +


  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y",scales = "free_y") +  # Switch `h2` labels to the right-hand side

  xlab("Replicate (L)") +
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
  xlab(  expression(n == 10)  )+
  ylab(expression(h^2 == 25*"%"))+
  ggtitle(  expression(n == 10)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

P13
library(ggplot2)
P14 <- ggplot(combined_data[which(combined_data$n==10 & combined_data$h2==75),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ylab(expression(h^2 == 75*"%"))+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

P14
library(ggplot2)
P21 <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 20)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

P21
library(ggplot2)
P22 <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==30),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

P22
library(ggplot2)
P23 <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==50),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

P23
library(ggplot2)
P24 <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==75),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P32
library(ggplot2)
P33 <- ggplot( combined_data[which(combined_data$n==30 & combined_data$h2==50),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+

  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P41
P42 <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==30),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P42
P43 <-  ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==50),],
                 aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=50, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

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
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P53
library(ggplot2)
P54 <- ggplot(  combined_data[which(combined_data$n==75 & combined_data$h2==75),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=75, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P54

library(ggplot2)
P61 <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==25),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 100)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P61
library(ggplot2)
P62 <- ggplot(  combined_data[which(combined_data$n==100 & combined_data$h2==30),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.3")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P62
library(ggplot2)
P63 <- ggplot(combined_data[which(combined_data$n==100 & combined_data$h2==50),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P63

library(ggplot2)
P64 <- ggplot( combined_data[which(combined_data$n==100 & combined_data$h2==75),], aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=100, h=0.5")+
  geom_hline(yintercept = 0.95)+
  ylim( c(0.4,1))+theme_cowplot()+theme(legend.position = "none")

P64
library(gridExtra)

grid.arrange(P11, P21, P31,P41,P51,P61,
             P12, P22, P32,P42,P52,P62,
             P13, P23, P33,P43,P53,P63,
             P14, P24, P34,P44,P54,P64,
             ncol=6)




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
  ylab(' ')+
  xlab(' ')+
  ggtitle(  expression(n == 20)  )+

  theme_cowplot()+theme(legend.position = "none")

P21_cs
library(ggplot2)
P22_cs  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==30),], aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.3")+

  theme_cowplot()+theme(legend.position = "none")

P22_cs
library(ggplot2)
P23_cs  <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==50),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle("n=20, h=0.5")+

  theme_cowplot()+theme(legend.position = "none")

P23_cs
library(ggplot2)
P24_cs  <- ggplot(  combined_data[which(combined_data$n==20 & combined_data$h2==75),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
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
grid.arrange(P11_cs , P21_cs , P31_cs ,P41_cs ,P51_cs ,P61_cs ,
             P12_cs , P22_cs , P32_cs ,P42_cs ,P52_cs ,P62_cs ,
             P13_cs , P23_cs , P33_cs ,P43_cs ,P53_cs ,P63_cs ,
             P14_cs , P24_cs , P34_cs ,P44_cs ,P54_cs ,P64_cs ,
             ncol=6)




ggplot(combined_data[which(combined_data$L==1),], aes( x= BF , y=obs_cov,
                                                       color= BF ))+
  facet_grid(h2~n)+
  geom_point()+ theme_minimal()+
  geom_hline(yintercept = 0.95)+
  ggtitle("Observed coverage for L=1")


ggplot(combined_data, aes(y =power, x = as.factor(L), col = BF)) +
  geom_point(position = position_dodge, size = 2) +
  facet_grid(h2 ~ n, labeller = custom_labeller, switch = "y")
