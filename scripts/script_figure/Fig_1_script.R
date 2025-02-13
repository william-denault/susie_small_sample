library(susieR)
library(cowplot)
library(ggplot2)

path= getwd()
small_data <- readRDS(paste0(path,"/data/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds"))

y= small_data$ENSG00000151694$residual_Y[[3]]
X= small_data$ENSG00000151694$residual_X[[3]]
res_susie =susieR::susie(X = X,y=y,L=20   )


res_susie_small =susieRsmall::susie(X = X,y=y,L=10, max_iter = 20 , estimate_prior_method = "EM")



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

P_pred = ggplot(df_plot_pred, aes( y=y, x=x, col= col))+
  geom_point()+
  ylab("Normalized gene expression")+
  xlab("predicted gene expression")+
  theme_cowplot()

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
  labs(x = "variable", y = ylab) +
  theme_minimal()

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

pip_plot_susie=gg




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
  labs(x = "variable", y = ylab) +
  theme_minimal()

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

pip_plot_susie_small=gg




susie_plot( res_susie_small, y="PIP",
            main=paste( "SuSiE Servin Stephens SER "))
par(mfrow=c(1,1))
