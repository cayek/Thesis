source("setup.R")

# load data
data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)

# compute W
W <- ComputeHeatKernelWeight(call_method_75_TAIR9.europe$coord,
                             sigma = 1.5)

# Run tess3
cat(green(paste("== Runing TESS3 \n")))
tess3Main.obj <- tess3Main(X = call_method_75_TAIR9.europe$X,
                         XBin = NULL,
                         coord = call_method_75_TAIR9.europe$coord,
                         K = 6,
                         ploidy = 1,
                         lambda = 1.0,
                         W = W,
                         method = "MCPA",
                         max.iteration = 200,
                         tolerance = 1e-05,
                         openMP.core.num = 16)


cat(green(paste("== Save result\n")))
save(tess3Main.obj, file = paste0(res.dir,"tess3K6.sigmaHeat1.5.RData"))
