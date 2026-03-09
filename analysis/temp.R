load("../output/small_sim_out_n=40_maf=0.50_v0.15.55.RData")
stats_susie    <- compute_power_and_coverage(res_susie,causal_snps)
stats_susie_ss <- compute_power_and_coverage(res_susie_small,causal_snps)
print(data.frame(method   = c("susie","susie_ss"),
                 power    = c(stats_susie$power,stats_susie_ss$power),
                 coverage = c(stats_susie$coverage,stats_susie_ss$coverage)))
sizes_susie    <- get_cs_sizes(res_susie)
sizes_susie_ss <- get_cs_sizes(res_susie_small)
print(compare_cs_sizes(sizes_susie,sizes_susie_ss,max_size = 40))
print(compare_prior_variances(stats_susie$V_true,stats_susie$V_false,
                              stats_susie_ss$V_true,stats_susie_ss$V_false,
                              ymax = 200))
