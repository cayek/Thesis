source("setup.R")
library(LEA)
# load data
data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)

# create tmp file
file.geno <- tempfile(fileext = ".geno")
write.geno(R = call_method_75_TAIR9.europe$X, output.file = file.geno)

# Run sNMF
cat(green(paste("== Runing sNMF \n")))
snmf.proj <- snmf(file.geno, K=6, entropy = FALSE, repetitions = 1,
         project = "new", ploidy = 1)

snmf.obj <- list()
snmf.obj$Q <- Q(snmf.proj, K = 6, run = 1)
snmf.obj$G <- G(snmf.proj, K = 6, run = 1)

cat(green(paste("== Save result\n")))
save(snmf.obj, file = paste0(res.dir,"snmfK6.RData"))
