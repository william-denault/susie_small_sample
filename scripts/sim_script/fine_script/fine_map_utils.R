#' FINEMAP I/O
write_finemap_sumstats_v1.4 <- function(bhat, se, allele_freq, LD_file, n, k, prefix) {
  cfg = list(z=paste0(prefix,".z"),
             ld=LD_file,
             snp=paste0(prefix,".snp"),
             config=paste0(prefix,".config"),
             k=paste0(prefix,".k"),
             log=paste0(prefix,".log"),
             meta=paste0(prefix,".master"),
             cred=paste0(prefix, '.cred'))
  # get dataset.z
  J = length(bhat)
  # FIXME: using minor allele frequency does not make sense
  z = cbind(1:J, rep(1,J), rep(1,J), rep('A',J), rep('C',J),
            pmin(allele_freq, 1-allele_freq), bhat, se)
  colnames(z) = c('rsid', 'chromosome', 'position', 'allele1', 'allele2',
                  'maf', 'beta', 'se')
  write.table(z,cfg$z,quote=F,col.names=T, row.names=F)
  if (!is.null(k)) {
    write.table(t(k),cfg$k,quote=F,col.names=F,row.names=F)
    write("z;ld;snp;config;k;log;cred;n_samples",file=cfg$meta)
    write(paste(cfg$z, cfg$ld, cfg$snp, cfg$config, cfg$k, cfg$log, cfg$cred, n, sep=";"),
          file=cfg$meta,append=TRUE)
  } else {
    write("z;ld;snp;config;cred;log;n_samples",file=cfg$meta)
    write(paste(cfg$z, cfg$ld, cfg$snp, cfg$config, cfg$cred, cfg$log, n, sep=";"),
          file=cfg$meta,append=TRUE)
  }
  return(cfg)
}

#' Run FINEMAP version 1.4
#' http://www.christianbenner.com

run_finemap_v1.4 <- function(bhat, se, allele_freq, LD_file, n, k, method,args = "", prefix="data")
{
  cfg = write_finemap_sumstats_v1.4(bhat, se, allele_freq, LD_file, n, k, prefix)
  if(method == 'sss'){
    cmd = paste("finemap_v1.4 --sss --log", "--in-files", cfg$meta, args)
  }else{
    cmd = paste("finemap_v1.4 --cond --log", "--in-files", cfg$meta, args)
  }

  out <- dscrutils::run_cmd(cmd, timeout=900, quit_on_error=FALSE)
  if (out != 0) {
    return(list(snp=NULL, config=NULL, set=NULL, ncausal=NULL))
  }

  # remove unused files
  file.remove(cfg$z)

  # read output tables
  snp = read.table(cfg$snp,header=TRUE,sep=" ")[, c("rsid", "prob", "log10bf")]
  colnames(snp) = c("snp", "snp_prob", "snp_log10bf")
  snp$snp = as.character(snp$snp)
  snp = rank_snp_v1.4(snp)

  config = read.table(cfg$config,header=TRUE,sep=" ")[, 1:4]
  colnames(config) = c('rank', 'config', 'config_prob', 'config_log10bf')

  # Only keep configurations with cumulative 95% probability
  config = within(config, config_prob_cumsum <- cumsum(config_prob))
  config = config[config$config_prob_cumsum <= 0.95,]

  tmp = unlist(strsplit(cfg$cred, '/'))
  path = paste(tmp[-length(tmp)], collapse = '/')
  pattern = tmp[length(tmp)]
  files = list.files(path=path,pattern=pattern)
  prob = numeric(length(files))
  for(i in 1:length(files)){
    first_line <- readLines(paste(c(path, files[i]), collapse = '/'),n=1)
    prob[i] = as.numeric(unlist(strsplit(first_line, '= '))[2])
  }

  cred = read.table(paste(c(path, files[which.max(prob)]), collapse = '/'),header = T, sep='')
  cred = cred[,grep('cred',colnames(cred)), drop=FALSE]
  cs = lapply(1:ncol(cred), function(i) cred[,i][!is.na(cred[,i])] )
  purity = matrix(unlist(strsplit(readLines(paste(c(path, files[which.max(prob)]), collapse = '/'),n=5)[3:5],' ')),
                  nrow=3, byrow = T)
  purity = purity[,2*(1:length(cs)), drop=FALSE]
  class(purity) <- "numeric"
  purity = t(purity)
  colnames(purity) = c('min.abs.corr', 'mean.abs.corr','median.abs.corr')

  # extract number of causal
  if(method == 'sss'){
    ncausal = finemap_extract_ncausal_v1.4(paste0(cfg$log, '_sss'))
  }else{
    ncausal = finemap_extract_ncausal_v1.4(paste0(cfg$log, '_cond'))
  }

  return(list(snp=snp, config=config, set=cs, ncausal=ncausal, purity = purity))
}

rank_snp_v1.4 <- function(snp) {
  snp <- arrange(snp, -snp_prob) %>%
    mutate(
      rank = seq(1, n()),
      snp_prob_cumsum = cumsum(snp_prob) / sum(snp_prob)) %>%
    select(rank, snp, snp_prob, snp_prob_cumsum, snp_log10bf)
  return(snp)
}

finemap_extract_ncausal_v1.4 <- function(logfile)
{
  lines <- grep("->", readLines(logfile), value = TRUE)
  lines <- gsub("\\(|\\)|>", "", lines)
  splits <- strsplit(lines, "\\s+")
  tab <- data.frame(
    ncausal_num = sapply(splits, function(x) as.integer(x[2])),
    ncausal_prob = sapply(splits, function(x) as.double(x[4])))
  tab <- mutate(tab, type = ifelse(duplicated(ncausal_num), "post", "prior"))
  return(tab)
}

finemap_mvar_v1.4 <- function(bhat, se, allele_freq, LD_file, n, k, method, args, prefix,
                              parallel = TRUE) {
  if (is.null(dim(bhat))) {
    bhat = matrix(ncol=1,bhat)
  }
  if (is.null(dim(se))) {
    se = matrix(ncol=1,se)
  }
  single_core = function(r)
    run_finemap_v1.4(bhat[,r], se[,r], allele_freq, LD_file, n, k, method, args,
                     paste0(prefix, '_condition_', r))
  if (parallel)
    return(parallel::mclapply(1:ncol(bhat), function(r) single_core(r),
                              mc.cores = min(8, ncol(bhat))))
  else
    return(lapply(1:ncol(bhat), function(r) single_core(r)))
}
