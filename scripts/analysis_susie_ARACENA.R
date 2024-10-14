library(vcfR)


window=200000
library(data.table)
ge_MAT=fread("/project2/mstephens/wdenault/EU_AF_ancestry_flu/full_data/RNAseq_NI.expression.txt.gz")

info = ge_MAT[,1:4]
ge_MAT=as.matrix(ge_MAT[,-c(1:4)])
info$chr = ( gsub("chr", "",info$chr))
name_g_file <- "imputed_ALL.chrs.annotated.v2.renamed.vcf.gz"
file_path <- paste( "/project2/mstephens/wdenault/EU_AF_ancestry_flu/full_data/",name_g_file, sep="")
###################
# Load the VCF data
###################
vcf_data <- read.vcfR(file_path)
# Extract metadata (positions, chromosomes, etc.)
vcf_meta <- getFIX(vcf_data)  # Extract fixed data (like CHROM, POS, etc.)
vcf_genotypes <- extract.gt(vcf_data)  # Extract genotypes

for( j in 1:ncol( ge_MAT)){


  # Filter by chromosome and position range
  filtered_indices <- which(vcf_meta[, "CHROM"] == info$chr[j] &
                              as.numeric(vcf_meta[, "POS"]) >= info$start[j] -window &
                              as.numeric(vcf_meta[, "POS"]) <= info$end[j] +window)

  # Extract the corresponding genotypes for the filtered SNPs
  filtered_genotypes <- vcf_genotypes[filtered_indices, ]
  info_SNP =  data.frame(vcf_meta[filtered_indices, ])

  filtered_genotypes <- filtered_genotypes[, which(colnames(filtered_genotypes)%in% colnames(ge_MAT))]
  filtered_genotypes = filtered_genotypes[ , order(colnames(filtered_genotypes))]

  # Function to transform genotypes
  transform_genotype <- function(genotype) {
    if (genotype == "0/0") {
      return(0)
    } else if (genotype == "0/1" || genotype == "1/0") {
      return(1)
    } else if (genotype == "1/1") {
      return(2)
    } else {
      return(NA)  # Handle missing or unexpected values
    }
  }

  # Apply the transformation across the whole dataframe
  transformed_genotypes <- apply(filtered_genotypes, 2, function(col) {
    sapply(col, transform_genotype)
  })


  Y = as.vector( ge_MAT[j, which(colnames(ge_MAT) %in% colnames( filtered_genotypes))])
  id_perm = sample ( 1:ncol(transformed_genotypes))

  res_susie= susieR::susie(X= t( transformed_genotypes), y=Y)
  res_susie_perm= susieR::susie(X= t( transformed_genotypes), y=Y[id_perm])

  res_susie_small      = susieRsmall::susie(X= t( transformed_genotypes), y=Y)
  res_susie_small_perm = susieRsmall::susie(X= t( transformed_genotypes), y=Y[id_perm])

  out= list(res_susie            = res_susie,
            res_susie_perm       = res_susie_perm,
            res_susie_small      = res_susie_small,
            res_susie_small_perm = res_susie_small_perm)


 save(out,file=
      paste0( "/project2/mstephens/wdenault/genome_wide_analysis_results/susie_small/",
            info[j,1],
          ".RData")
    )

}






