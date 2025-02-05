small_data <- readRDS("C:/Users/WD/Downloads/MiGA_eQTL.chr2_ENSG00000151694.univariate_data.rds")


y= small_data$ENSG00000151694$residual_Y[[2]]
X= small_data$ENSG00000151694$residual_X[[2]]
res_susie =susieR::susie(X = X,y=y,L=20)
res_susie$sets
y= small_data$ENSG00000151694$residual_Y[[3]]
X= small_data$ENSG00000151694$residual_X[[3]]
res_susie =susieR::susie(X = X,y=y,L=20)
res_susie$sets
 
remotes::install_github("william-denault/susieRsmall@49364ab74fdf449eeb8192b271caa04d2ddf4b9a")

library(susieRsmall)

sessionInfo()
res_susie_small =susieRsmall::susie(X = X,y=y,L=20  )
res_susie_small$sets
plot(res_susie_small$pip)

res_susie_small$lbf
