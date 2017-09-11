source("setup.R")


# load data
data.file <- paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)

# variogram
vario.gen <- CalculateEmpiricalGenSemivariogram(call_method_75_TAIR9.europe$X, 1,  call_method_75_TAIR9.europe$coord)
save(vario.gen, file = paste0(res.dir,"variogram.RData"))
