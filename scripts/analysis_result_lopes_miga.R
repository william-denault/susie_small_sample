getwd()

if(file.exists("C:/Document/Serieux/Travail/Package/susie_small_sample/res_SuSiE_MIGA.RData")){
  load("C:/Document/Serieux/Travail/Package/susie_small_sample/res_SuSiE_MIGA.RData")
}else{

  path= "C:/Document/Serieux/Travail/Data_analysis_and_papers/xQTL_data/MiGa_data_Gao_group/"
  list.files(path)
  lf=    list.files(paste0(path, "MiGA_SuSiE"))
  i =1


  lf2=    list.files(paste0(path, "MiGA_SuSiESmall"))
  i =1


  extract_gene_id <- function(name) {
    sub(".*(ENSG[0-9]+).*", "\\1", name)
  }

  lf_genes <- sapply(lf, extract_gene_id)
  lf2_genes <- sapply(lf2, extract_gene_id)

  # Find matches based on extracted gene IDs
  matches <- lf[lf_genes %in% lf2_genes]




  n_cs=  list()
  n_cs_small =list()
  overlap_list= list()
  # Find common genes
  common_genes <- intersect(lf_genes, lf2_genes)
  i=1
  # Load files for each common gene
  for (gene in common_genes) {
    lf_file <- lf[lf_genes == gene]  # Get the filename in lf
    susie_res  <- readRDS(paste0(path, "MiGA_SuSiE/",lf_file))

    n_cs[[i]]=  length(susie_res [[1]][[1]]$susie_result_trimmed$sets$cs)

    lf2_file <- lf2[lf2_genes == gene][2]  # Get the filename in lf2
    susie_small_res  <-readRDS(paste0(path, "MiGA_SuSiESmall/",lf2_file))
    # Get the filename in lf2
    n_cs_small[[i]]=  length(susie_small_res  [[1]][[1]]$susie_result_trimmed$sets$cs)

    print(n_cs_small[[i]])


    if (n_cs_small[[i]]>0){
      count_overlapp=0
      for (l in 1:n_cs_small[[i]]){

        for (k in 1:n_cs[[i]] )

          length_inter <-  intersect(susie_small_res  [[1]][[1]]$susie_result_trimmed$sets$cs [[l]],
                                     susie_res [[1]][[1]]$susie_result_trimmed$sets$cs[[k]])
        count_overlapp= count_overlapp+   ifelse(length( length_inter)>0,1,0)


      }

    }else{
      count_overlapp=0
    }
    overlap_list[[i]]=count_overlapp
    print(i)
    i=i+1
    # Do something with the loaded data (e.g., store or compare them)
  }

  #10000



  lapply(1:length(overlap_list), function(i){
    sum(overlap_list[[i]])
  })

  summary_res= cbind(do.call(c, n_cs),
                     do.call(c, n_cs_small),
                     do.call(c,lapply(1:length(overlap_list), function(i){
                       sum(overlap_list[[i]])
                     })
                     ))

}

save(summary_res,file="res_SuSiE_MIGA.RData")
table(summary_res[,1],
      summary_res[,2],
      ifelse(summary_res[,3]>0,0,1))


table(summary_res[,1],
      summary_res[,2] )
table(summary_res[,1],
      summary_res[,2])
apply(summary_res,2,sum)
table(summary_res[,1])



res= summary_res

sum(ifelse(res[,3]>0,1,0)*res[,2 ])

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




ggplot(df_long, aes(y = SuSiE_topPC, x = fSuSiE, size =   (count))) +
  geom_point(alpha = 0.9, color = "darkblue") +

  # Scale size legend with better breaks (square root scale → rescale back)
  scale_size_continuous(
    name = "number of CS",
    breaks =  (c(10, 50, 100, 500, 1000)),
    labels = c(10, 50, 100, 500, 1000),
    range = c(1, 10)
  ) +

  # Color scale (log scale with nice gradient and rounded breaks)


  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = 0:20) +
  labs(y = "SS SER", x = "Default SER") +
  theme_minimal(base_size = 13)





ggplot(df_long, aes(x = SuSiE_topPC, y = fSuSiE,
                    size = log1p(count)),
       color=log1p( count)) +
  geom_point(  alpha = 0.8) +

  scale_size(range = c(1, 10)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = 0:20) +
  labs(x = "SS SER", y = "Default SER", size = "number of CS") +
  theme_minimal()






df <- as.data.frame(mat)
colnames(df) <- 0:4
df$fSuSiE <- 0:12

# Pivot to long format
df_long <- df %>%
  pivot_longer(cols = -fSuSiE, names_to = "SuSiE_topPC", values_to = "count") %>%
  mutate(
    SuSiE_topPC = as.integer(SuSiE_topPC),
    fSuSiE = as.integer(fSuSiE)
  ) %>%
  filter(count > 0)

# Plot with color scale
ggplot(df_long, aes(x = SuSiE_topPC, y = fSuSiE, size =  (count), color = log1p(count))) +
  geom_point(alpha = 0.9) +
  scale_size(range = c(1, 10)) +
  scale_color_gradient(low = "red", high = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = 0:20) +
  labs(x = "SuSiE-topPC", y = "fSuSiE", size = "number of TADs", color = "number of TADs") +
  theme_minimal()





library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Your existing data transformation here...
# (Assume df_long is already created and filtered as before)
