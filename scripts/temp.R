library(ggplot2)
library(cowplot)
load("../output/small_sim_out_n=40_maf=0.50_v0.15.54.RData")
x <- sapply(causal_snps,length)
i <- which(x == 0)
j <- which(x == 1)

lbf0 <- sapply(res_susie[i],function (x) x$lbf)
lbf1 <- sapply(res_susie[j],function (x) x$lbf)
pdat1 <- rbind(data.frame(causal = FALSE,lbf = lbf0),
              data.frame(causal = TRUE,lbf = lbf1))
pdat1 <- transform(pdat1,lbf = log10(pmax(0,lbf)))
p1 <- ggplot(pdat1,aes(x = lbf,color = causal,fill = causal)) +
  geom_histogram(position = "stack") +
  scale_color_manual(values = c("darkblue","red")) +
  scale_fill_manual(values = c("darkblue","red")) +
  labs(x = "log10 LBF",title = "susie") +
  xlim(c(-4,2)) +
  theme_cowplot(font_size = 12)

lbf0 <- sapply(res_susie_small[i],function (x) x$lbf)
lbf1 <- sapply(res_susie_small[j],function (x) x$lbf)
pdat2 <- rbind(data.frame(causal = FALSE,lbf = lbf0),
              data.frame(causal = TRUE,lbf = lbf1))
pdat2 <- transform(pdat2,lbf = log10(pmax(0,lbf)))
p2 <- ggplot(pdat2,aes(x = lbf,color = causal,fill = causal)) +
  geom_histogram(position = "stack") +
  scale_color_manual(values = c("darkblue","red")) +
  scale_fill_manual(values = c("darkblue","red")) +
  labs(x = "log10 LBF",title = "susie_small") +
  xlim(c(-4,2)) +
  theme_cowplot(font_size = 12)

print(plot_grid(p1,p2,nrow = 2,ncol = 1))
