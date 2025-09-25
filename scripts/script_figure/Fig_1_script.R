library(susieR)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(cowplot)
library(dplyr)
library(gridExtra)
colors <- c( "#D41159","#1A85FF" )


your_path  ="C:/Document/Serieux/Travail/Package/susie_small_sample/data_gao/"
small_data <- readRDS(paste0(your_path ,"MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"))

y= small_data$ENSG00000151694$residual_Y[[3]]
X= small_data$ENSG00000151694$residual_X[[3]]
res_susie =susieR::susie(X = X,y=y,L=20   )


res_susie_small = susie(X = X,y=y,L=10,small=TRUE)


var( y -predict(res_susie_small))/var(y)
var( y -predict(res_susie))/var(y)

df_plot_pred=  data.frame (y= rep( y,2),
                           x= c( predict(res_susie_small , X),
                                 predict(res_susie , X)),
                           col= factor(c(
                             rep( "SS SER",
                                  length( predict(res_susie_small , X))) ,
                             rep( "default SER",
                                  length( predict(res_susie  , X)))
                           )
                           )
)

P_pred = ggplot(df_plot_pred , aes( y=y, x=x, col= col))+
  geom_abline(slope=1, intercept=0)+
  geom_point()+
  ylab("Normalized gene expression")+
  xlab("predicted gene expression")+
  theme_cowplot()+
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "gray80"))+
  scale_color_manual(values = colors,
                     name = "SuSiE:", labels = c("Default SER", "Servin and Stephens SER"))

##### PRed plot  -----
P_pred



#### Pip  plot -----


color <- c("dodgerblue2", "green4", "#6A3D9A", "#FF7F00", "gold1", "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6",
           "#FDBF6F", "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
           "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown")
model = res_susie
is_susie <- inherits(model, "susie")
y <- "PIP"
pos <- NULL
b <- NULL
max_cs <- 400
add_bar <- FALSE

if (y == "PIP") {
  if (is_susie) {
    p <- model$pip
  } else {
    p <- model
  }
}

ylab <- "PIP"
if (is.null(b)) b <- rep(0, length(p))
if (is.null(pos)) pos <- 1:length(p)

df <- data.frame(pos = pos, p = p, b = b)

# Generate the plot using ggplot2
gg <- ggplot(df, aes(x = pos, y = p)) +
  geom_point(aes(color = b != 0)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "SNP", y = ylab) +
  theme_cowplot()+
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "gray80"))

for (i in rev(1:nrow(model$alpha))) {
  if (!is.null(model$sets$cs_index) && !(i %in% model$sets$cs_index)) next
  purity <- model$sets$purity[which(model$sets$cs_index == i), 1]
  if (!is.null(model$sets$purity) && max_cs < 1 && purity >= max_cs) {
    x0 <- intersect(pos, model$sets$cs[[which(model$sets$cs_index == i)]])
    y1 <- p[x0]
  } else if (susieR:::n_in_CS(model, model$sets$requested_coverage)[i] < max_cs) {
    x0 <- intersect(pos, which(susieR:::in_CS(model, model$sets$requested_coverage)[i, ] > 0))
    y1 <- p[x0]
  } else {
    x0 <- NULL
    y1 <- NULL
  }
  if (is.null(x0)) next
  gg <- gg + geom_point(data = data.frame(pos = x0, p = y1),
                        aes(x = pos, y = p),
                        color = head(color, 1),
                        shape = 1 ,stroke=1.4,
                        size = 2)
  color <- c(color[-1], color[1])
}

pip_plot_susie=gg+
  ggtitle( "SuSiE default SER, n=75 ")+
  theme(plot.title =element_text(size =16,  face = "plain"))


color <- c("dodgerblue2", "green4", "#6A3D9A", "#FF7F00", "gold1", "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6",
           "#FDBF6F", "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
           "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown")

model = res_susie_small
is_susie <- inherits(model, "susie")
y <- "PIP"
pos <- NULL
b <- NULL
max_cs <- 400
add_bar <- FALSE

if (y == "PIP") {
  if (is_susie) {
    p <- model$pip
  } else {
    p <- model
  }
}

ylab <- "PIP"
if (is.null(b)) b <- rep(0, length(p))
if (is.null(pos)) pos <- 1:length(p)

df <- data.frame(pos = pos, p = p, b = b)

# Generate the plot using ggplot2
gg <- ggplot(df, aes(x = pos, y = p)) +
  geom_point(aes(color = b != 0)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "SNP", y = ylab) +
  theme_cowplot()+
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "gray80"))

for (i in rev(1:nrow(model$alpha))) {
  if (!is.null(model$sets$cs_index) && !(i %in% model$sets$cs_index)) next
  purity <- model$sets$purity[which(model$sets$cs_index == i), 1]
  if (!is.null(model$sets$purity) && max_cs < 1 && purity >= max_cs) {
    x0 <- intersect(pos, model$sets$cs[[which(model$sets$cs_index == i)]])
    y1 <- p[x0]
  } else if (susieR:::n_in_CS(model, model$sets$requested_coverage)[i] < max_cs) {
    x0 <- intersect(pos, which(susieR:::in_CS(model, model$sets$requested_coverage)[i, ] > 0))
    y1 <- p[x0]
  } else {
    x0 <- NULL
    y1 <- NULL
  }
  if (is.null(x0)) next
  gg <- gg + geom_point(data = data.frame(pos = x0, p = y1),
                        aes(x = pos, y = p),
                        color = head(color, 1),
                        shape = 1 ,stroke=1.4,
                        size = 2)
  color <- c(color[-1], color[1])
}

pip_plot_susie_small=gg+
  ggtitle( "SuSiE Servin Stephens SER ")+
  theme(plot.title =element_text(size =16,  face = "plain"))


grid.arrange(pip_plot_susie, P_pred, ncol=2)


load(paste0(your_path  ,"summary_L1_3.RData"))
combined_data=combined_data[- which(combined_data$n==10),]
library(ggplot2)
P11 <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
               aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(expression("Coverage"))+
  xlab(' ')+
  # ggtitle(  expression(n == 20)  )+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)

P12 <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)



library(ggplot2)
P13 <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],
                aes(y=obs_cov, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  # ggtitle(  expression(n == 50)  )+


  geom_hline(yintercept = 0.95)+
  ylim( c(min(combined_data$obs_cov-0.02),1))+theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)




P21   <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
                 aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab( "CS size")+
  xlab(' ')+
  #ggtitle(  expression(n == 20)  )+
  ylim(c(min( combined_data$cs_size) ,max(combined_data$cs_size) ))+
  theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)



P22  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  #  ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+

  ylim(c(min( combined_data$cs_size) ,max(combined_data$cs_size) ))+
  theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)



P23  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],
                 aes(y=cs_size, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  # ggtitle(  expression(n == 50)  )+


  ylim(c(min( combined_data$cs_size) ,max(combined_data$cs_size) ))+

  theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)





P31  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
                aes(y=power, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab( "Power")+
  xlab(' ')+
  #ggtitle(  expression(n == 20)  )+
  scale_y_log10(     limits = c(min(combined_data$power), 1),    breaks = c(0.01,0.05,0.1, 0.25,0.5, 1))+
  theme_cowplot()+theme(    legend.position = "none",    panel.grid.major = element_line(color = "gray80")   )+
  scale_color_manual(values = colors)

P31


P32 <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=power, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  scale_y_log10(     limits = c(min(combined_data$power), 1),    breaks = c(0.01,0.05,0.1, 0.25,0.5, 1))+
  theme_cowplot()+theme(    legend.position = "none",    panel.grid.major = element_line(color = "gray80")   )+
  scale_color_manual(values = colors)

P32



library(ggplot2)
P33  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],
                 aes(y=power, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab(' ')+
  xlab(' ')+
  #ggtitle(  expression(n == 50)  )+

  scale_y_log10(     limits = c(min(combined_data$power), 1),    breaks = c(0.01,0.05,0.1, 0.25,0.5, 1))+

  theme_cowplot()+theme(    legend.position = "none",    panel.grid.major = element_line(color = "gray80")   )+
  scale_color_manual(values = colors)

P33



P41  <- ggplot( combined_data[which(combined_data$n==20 & combined_data$h2==25),],
                aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  ylab("Purity")+
  xlab(' ')+
  # ggtitle(  expression(n == 20)  )+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)

P41


P42  <- ggplot(  combined_data[which(combined_data$n==30 & combined_data$h2==25),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)




P43  <- ggplot(  combined_data[which(combined_data$n==50 & combined_data$h2==25),],  aes(y=purity, x=as.factor(L), col=BF))+
  geom_point(
  )+
  # ggtitle(  expression(n == 30)  )+
  #ylab(expression(h^2 == 25*"%"))+
  xlab("")+
  ylab(' ')+
  ylim(0.9, 1) +
  theme_cowplot()+theme(legend.position = "none",panel.grid.major = element_line(color = "gray80"))+scale_color_manual(values = colors)

titles <- lapply(c(20, 30, 50 ), function(n) {
  textGrob(label = bquote(N == .(n)), gp = gpar(fontsize =16, fontface = "bold"))
})

P_perf= grid.arrange(
  arrangeGrob(grobs = titles, ncol = 3),
  arrangeGrob(  P11, P12,P13 ,
                P21, P22,P23,
                P31, P32,P33,
                P41, P42,P43,
                ncol=3),
  heights = c(0.03, 1))




#Miga results -----


mat <- matrix(c(# 12232
  0, 12, 1, 0, 0,
  2634, 858, 9, 0, 0,
  647, 284, 64, 0, 0,
  140, 68, 27, 8, 0,
  50, 33, 11, 3, 1,
  15, 8, 5, 1, 0,
  3, 2, 2, 0, 0,
  4, 1, 1, 0, 0,
  1, 1, 1, 0, 0,
  0, 1, 0, 0, 0,
  0, 0, 1, 0, 0,
  1, 0, 0, 0, 0,
  0, 1, 0, 0, 0
), nrow = 13, byrow = TRUE)


library(ggplot2)
library(tidyr)
library(dplyr)
df <- as.data.frame(mat)
colnames(df) <- 0:4
df$fSuSiE <- 0:12

# Pivot longer to tidy format
df_long <- df %>%
  pivot_longer(cols = -fSuSiE, names_to = "SuSiE_topPC", values_to = "count") %>%
  mutate(
    SuSiE_topPC = as.integer(SuSiE_topPC),
    fSuSiE = as.integer(fSuSiE)
  ) %>%
  filter(count > 0)

# Plot




P_Miga= ggplot(df_long, aes(y = SuSiE_topPC, x = fSuSiE, size =    (count))) +
  geom_point(alpha = 0.9, color = "darkblue") +

  # Scale size legend with better breaks (square root scale → rescale back)
  scale_size_continuous(
    name = "Number of genes",
    breaks =   (c(10, 50, 100 , 1000)),
    labels = c(10, 50, 100 , 1000),
    range = c(1, 10)
  ) +
  # Color scale (log scale with nice gradient and rounded breaks)


  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = 0:20) +
  labs(y = "CSs SS SER", x = "CSs  Default SER") +
  theme_cowplot()+theme( panel.grid.major = element_line(color = "gray80"))





library(ggplot2)
library(cowplot)

P_Miga = ggplot(df_long, aes(y = SuSiE_topPC, x = fSuSiE, size = count)) +
  geom_point(alpha = 0.9, color = "darkblue") +

  # Adjust size scale for bubble legend
  scale_size_continuous(
    name = "Number of genes",
    breaks = c(10, 50, 100, 1000),
    labels = c(10, 50, 100, 1000),
    range = c(1, 10)
  ) +

  # Diagonal reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +

  # Expand axis limits a bit to avoid clipping large bubbles
  coord_cartesian(xlim = c(0, max(df_long$fSuSiE)  ),
                  ylim = c(-0.2, max(df_long$SuSiE_topPC)  )) +

  # Set axis breaks
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = 0:20) +

  # Axis labels
  labs(y = "CSs SS SER", x = "CSs Default SER") +

  # Styling
  theme_cowplot() +
  theme(panel.grid.major = element_line(color = "gray80"))

print(P_Miga)



library(cowplot)


legend_plot <- get_legend(P_pred + theme(legend.position = "bottom"))

legend_plot_miga <- get_legend(P_Miga+ theme(legend.position = "bottom"))

grid_plot <- ggdraw()+

  draw_plot(P_perf        ,
            x = 0.  , y = .0 , width = .47, height = 1)+
  draw_plot(pip_plot_susie+
              theme(legend.position = "none")         ,
            x = .48, y = .5, width = .25, height = .5)+

  draw_plot(P_pred     ,
            x = .48, y = .1, width = .25, height = .35)+

  draw_plot(legend_plot  ,
            x = .5, y = .0, width = .25, height = .1)+
  draw_plot(pip_plot_susie_small +
              theme(legend.position = "none") +
              ylim(c(0,1)),
            x = .75, y = .5, width = .25, height = .5)+

  draw_plot(P_Miga +
              theme(legend.position = "none")    ,
            x = .75, y = .1, width = .25, height = .35)+
  draw_plot(legend_plot_miga    ,
            x = .77, y = 0, width = .25, height = .1)+
  draw_label("A",fontface = "bold",
             size = 20,
             x = 0.01, y = 0.99, vjust = 1  ) +

  draw_label("B", fontface = "bold",
             size = 20, x = 0.5 , y = 0.99, vjust = 1  ) +
  draw_label("C",fontface = "bold",
             size = 20,
             x = 0.5 , y = 0.48, vjust = 1  )+
  draw_label("D",fontface = "bold",
             size = 20,
             x = 0.75 , y = 0.48, vjust = 1  )
plot_grid(grid_plot )

ggsave("C:/Document/Serieux/Travail/Package/susie_small_sample/plots/Figure1.pdf",
         plot = grid_plot,
         width = 500,
         height = 280,
         units = "mm")

