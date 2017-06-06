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
load(paste0(res.dir,"vario.gen.res"))

################################################################################
# Plot
pl <- ggplot(vario.gen, aes(x = h, y = semi.variance, size = size)) + 
  geom_point() + geom_vline(xintercept = 1.5, colour = "red") +
  labs(y = "Semivariance", x = "Geographic distance between individuals (km $\\times 100$)") +
  theme_gray(base_size = 12) + 
  scale_size_continuous(range = c(1,3)) +
  guides(size = guide_legend(title = "Bin size"))



tikzDevice::tikz(paste0(fig.dir,"variogram.tex"), width = slide$width * 0.9,
                 height = slide$heigth * 0.75,standAlone = TRUE)
pl
dev.off()
texi2dvi(paste0(fig.dir,"variogram.tex"),pdf = TRUE)


