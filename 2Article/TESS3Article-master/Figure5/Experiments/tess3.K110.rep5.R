source("setup.R")

# load data
data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)

XBin <- matrix(0.0, nrow(call_method_75_TAIR9.europe$X),
               ncol(call_method_75_TAIR9.europe$X) * 2)
X2XBin(call_method_75_TAIR9.europe$X, 1, XBin)
coord <- call_method_75_TAIR9.europe$coord
rm(call_method_75_TAIR9.europe) # free memory
gc()

# Run tess3
cat(green(paste("== Runing TESS3 \n")))
tess3.obj <- tess3(X = NULL,
                         XBin = XBin,
                         coord = coord,
                         K = 1:10,
                         ploidy = 1,
                         lambda = 1.0,
                         rep = 5,
                         W = NULL,
                         method = "MCPA",
                         max.iteration = 200,
                         tolerance = 1e-05,
                         openMP.core.num = 16,
                         Q.init = NULL,
                         mask = 0.05,
                         keep = "best",
                         copy = FALSE,
                         algo.copy = TRUE)


cat(green(paste("== Save result\n")))
save(tess3.obj, file = paste0(res.dir,"tess3.K110.rep5.RData"))

# keep only rmse
err.df <- data.frame()
for (t in tess3.obj) {
  err.df <- rbind(err.df,
    data.frame(rmse = t$rmse,
                crossvalid.rmse = t$crossvalid.rmse,
                crossentropy = t$crossentropy,
                crossvalid.crossentropy = t$crossvalid.crossentropy,
                K = t$K,
                rep = seq_along(t$rmse)))

}
save(err.df, file = paste0(res.dir,"err.K110.rep5.RData"))
