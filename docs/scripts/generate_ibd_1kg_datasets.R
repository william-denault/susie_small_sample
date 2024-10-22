# TO DO: Explain what this script is for, and how to use it.
#
# The file 20130606_g1k.ped containing the population labels was
# downloaded from here:
#
#   https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/working/
#   20130606_sample_info/
#
# See Supplementary Table 1 from the Nature paper
# (doi:10.1038/nature15393) for an explanation of the population
# labels.
#
# The genotype data were downloaded from here:
#
#   https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/data_collections/
#   1000_genomes_project/release/20181203_biallelic_SNV/
#
# Then PLINK command like this were used to convert the VCF files
# to PLINK "pgen" files:
#
#   plink2 --vcf ALL.chr1.shapeit2_integrated_v1a.GRCh38.20181129.phased.vcf.gz \
#     --make-bed --keep-allele-order --out 1kg_chr1
#   
library(data.table)

# Read an n x p genotype matrix from a .raw file, where n is the
# number of samples and p is the number of genetic markers (SNPs). See
# http://www.cog-genomics.org/plink2/formats#raw for more information
# about this file format.
read_geno_raw <- function (filename) {
  geno <- fread(filename,sep = "\t",header = TRUE,stringsAsFactors = FALSE,
                showProgress = FALSE)
  class(geno)    <- "data.frame"
  ids            <- geno$IID
  geno           <- geno[-(1:6)]
  geno           <- as.matrix(geno)
  storage.mode(geno) <- "double"
  rownames(geno) <- ids
  colnames(geno) <- NULL
  return(geno)
}

# Load the 1kg population labels.
ped <- read.table("../data/1kg/20130606_g1k.ped",sep = "\t",header = TRUE,
                  stringsAsFactors = FALSE)
ped <- transform(ped,Population = factor(Population))

# Load the genotype ids.
fam <- read.table("../data/1kg/1kg_chr1.fam",sep = "\t",header = FALSE,
                  stringsAsFactors = FALSE)
ids <- fam[,2]

# Keep only the individuals for which we have genotype data.
ped <- subset(ped,is.element(Individual.ID,ids))

# Keep only the individuals that are (a) of European ancestry and (b)
# have no (known) relationships to other individuals.
ped <- subset(ped,
              is.element(Population,c("CEU","GBR","FIN","IBS","TSI")) &
              !duplicated(Family.ID) &
              Siblings == "0" &
              Second.Order == "0" &
              Third.Order == "0")

# Load the IBD fine-mapping regions.
regions <- read.csv("../data/ibd_finemap_huang2017.csv",header = TRUE,
                    comment.char = "#",stringsAsFactors = FALSE)

setwd("../data/1kg")

# Save the IDs of the individuals to keep.
write.table(ped["Individual.ID"],"ids.txt",row.names = FALSE,
            col.names = FALSE,quote = FALSE)

# Repeat for each fine-mapping region.
n <- nrow(regions)
for (i in 1:n) {
  id    <- regions[i,"HD"]
  chr   <- regions[i,"chr"]
  start <- regions[i,"region_start_hg38"]
  end   <- regions[i,"region_end_hg38"]

  # Extract the desired samples and SNPs.
  command_str <- sprintf(paste("./plink2 --bfile 1kg_chr%d --make-bed",
                               "--keep ids.txt --chr %d --from-bp %d",
                               "--to-bp %d --snps-only --out ibd_%d"),
                         chr,chr,start,end,id)
  system(command_str)

  # Convert the genotype data to a matrix in an .rds file. 
  command_str <- sprintf("./plink2 --bfile ibd_%d --export A --out ibd_%d",
                         id,id)
  system(command_str)
  bim  <- read.table(sprintf("ibd_%d.bim",id),sep = "\t",header = FALSE,
                     stringsAsFactors = FALSE)
  geno <- read_geno_raw(sprintf("ibd_%d.raw",id))
  geno <- 2 - geno
  colnames(geno) <- sprintf("chr%d_%d_%s_%s",bim[,1],bim[,4],bim[,5],bim[,6])
  saveRDS(geno,sprintf("ibd_%d.rds",id))
}
