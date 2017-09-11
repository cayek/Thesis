source("setup.R")
library(data.table)

################################################################################
# load data
at.dir <- paste0(data.dir,"/AthalianaGegMapLines/")

TAIR9.snps <- t(fread(
  paste0(at.dir,"call_method_75/call_method_75_TAIR9.csv"), sep = ",",
  header=TRUE, skip = 1, data.table = FALSE))
aux <- apply(TAIR9.snps[-(1:2),], 2, unique)
# find col0, the reference genome
call_method_75_info <- fread(
  paste0(at.dir,"call_method_75/call_method_75_info.tsv"))
col0 <- call_method_75_info[grepl("Col", call_method_75_info$nativename)]
ancestral.allele <- TAIR9.snps[as.character(col0$ecotype_id),]
# find variants allele
variant.allele <- sapply(seq_along(ancestral.allele), function(i) aux[which(!(aux[,i] %in% ancestral.allele[i])),i])
alleles <- data.frame(variant.allele = variant.allele, ancestral.allele = ancestral.allele, chr = as.numeric(TAIR9.snps[1,]), pos = as.numeric(TAIR9.snps[2,]))
# load data for colnames
data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)

################################################################################
# vep
vep.input <- alleles %>% mutate(start = pos, end = pos, allele = paste0(ancestral.allele,"/",variant.allele), strand = NA, identifier = colnames(call_method_75_TAIR9.europe$X)) %>% select(chr, pos, start, end, allele, strand, identifier)

# check if no error
head(vep.input)
tail(vep.input)

.Options$vep = "variant_effect_predictor.pl"
runVEP <- function(vep.input, vep = .Options$vep) {
  input <- tempfile()
  write.table(vep.input, file = input, row.names = FALSE, col.names = FALSE, na = "", quote = FALSE)
  output <- tempfile()
  cmd <- paste0("variant_effect_predictor.pl -i ", input, " -o ", output, " --cache --dir ../Data/vep/ --species arabidopsis_thaliana --format ensembl --genomes --cache_version 31")
  system(cmd)
  vep.output <- data.table::fread(output, skip = "#Uploaded_variation", data.table = FALSE, na.strings = "-")
  return(vep.output)
}

vep.res <- runVEP(vep.input)

save(vep.res, file = paste0(res.dir,"snpsTAIR9vepTAIR10.RData"))
