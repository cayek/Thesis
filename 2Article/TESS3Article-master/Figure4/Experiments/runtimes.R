source("setup.R")

data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR8.RData")
load(data.file)

sample.data.at <- sampler.from.data(call_method_75_TAIR8.europe$X,
  call_method_75_TAIR8.europe$coord)

data.list <- sample.data.at(50, 5000)

cores = 16
registerDoParallel(cores = cores)

ns = c(1e2, 2e2, 3e2, 4e2, 5e2, 6e2)
Ls = c(1e3, 5e3, 1e4, 5e4, 1e5, 2e5)
rep = 5
L = 50000
n = 150
K = 6
tess3Old.alpha = 0.03

df.n <- data.frame()
df.n <- rbind(fig4.exp.n(sample.data = sample.data.at, ns = ns, rep = rep, L = L, K = K, tess3Old.alpha = tess3Old.alpha), df.n)

df.L <- data.frame()
df.L <- rbind(fig4.exp.L(sample.data = sample.data.at, Ls = Ls, rep = rep, n = n, K = K, tess3Old.alpha = tess3Old.alpha), df.L)
save(df.L,df.n, file = paste0(res.dir,"runtimes.R"))
