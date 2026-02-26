library(ggplot2)
library(cowplot)
set.seed(1)

compute_power_and_coverage <- function (res, causal) {
  N        <- length(res)
  power    <- 0
  coverage <- 0
  V_true  <- NULL
  V_false <- NULL
  for (i in 1:N) {
    V        <- res[[i]]$V
    cs       <- res[[i]]$sets$cs
    L        <- length(V)
    all_cs   <- paste0("L",seq(1,L))
    names(V) <- all_cs
    if (length(cs) == 0) {
      x <- NULL
      y <- NULL
    } else {
      x <- intersect(unique(unlist(cs)),causal[[i]])
      y <- names(which(sapply(cs, 
             function (x) length(intersect(causal_snps[[i]],x)) > 0)))
    }
    V_true   <- c(V_true,V[y])
    V_false  <- c(V_false,V[setdiff(all_cs,y)])
    power    <- power    + length(x)
    coverage <- coverage + length(y)
  }
  num_true <- sum(sapply(causal,length))
  num_pos  <- sum(sapply(res,function (x) length(x$sets$cs)))
  return(list(power    = power/num_true,
              coverage = coverage/num_pos,
              V_true   = V_true,
              V_false  = V_false))
}

load("../output/small_sim_out_n=200_maf=0.05_v0.15.3.RData")
hist(pve,breaks = 32,col = "black",xlab = "PVE",main = "")
stats_susie    <- compute_power_and_coverage(res_susie,causal_snps)
stats_susie_ss <- compute_power_and_coverage(res_susie_small,causal_snps)
print(data.frame(method   = c("susie","susie_ss"),
                 power    = c(stats_susie$power,stats_susie_ss$power),
                 coverage = c(stats_susie$coverage,stats_susie_ss$coverage)))
