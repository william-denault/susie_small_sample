# Notes:
#
# https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/data_collections/1000_genomes_project/release/20181203_biallelic_SNV/
# https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/supporting/hd_genotype_chip/omni_samples.20141118.panel
#
regions <- read.csv("../data/ibd_finemap_huang2017.csv",header = FALSE,
                    comment.char = "#",stringsAsFactors = FALSE)
names(regions) <- c("HD","chr","signal","region_start_hg19","region_end_hg19",
                    "region_len_hg19","region_start_hg38","region_end_hg38")
