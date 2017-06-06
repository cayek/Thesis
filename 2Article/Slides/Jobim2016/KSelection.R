################################################################################
# Setup
library(ggplot2)
library(tess3r)
library(reshape2)
library(dplyr)
library(tikzDevice)
library(tools)
source("PlotParams.R")

################################################################################
# load res and data
load(paste0(res.dir,"tess3project.obj.res"))

################################################################################
# Plot
med = seq_along(tess3project.obj)
for(i in seq_along(tess3project.obj)) {
  med[i] <- median(tess3project.obj[[i]]$rmse)
}
pl <- ggplot(data.frame(rmse = med, K = seq_along(tess3project.obj))) + geom_point(aes(x = as.factor(K), y = rmse)) + 
  geom_line(aes(x = K, y = rmse)) + 
  labs(y = "RMSE", x = "Number of ancestral population ($K$)") +
  theme_gray() + 
  theme(legend.position = "none")



tikzDevice::tikz(paste0(fig.dir,"KSelection.tex"), width = slide$width * 0.8,height = slide$heigth * 0.8,standAlone = TRUE)
pl
dev.off()
texi2dvi(paste0(fig.dir,"KSelection.tex"),pdf = TRUE)
