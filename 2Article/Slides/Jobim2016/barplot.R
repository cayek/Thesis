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
# Data

n <- 200
K <- 2
ploidy <- 1
data.list <- SampleGenoOFWithMs(n = n,
                                nsites.neutral = 100000,
                                nsites.selected = 1000,
                                crossover.proba = 0.25 * 10 ^ -8,
                                m.neutral = 0.25 * 10 ^ -6,
                                m.selected = 0.25 * 10 ^ -7,
                                mutation.rate.per.site = 0.25 * 10 ^ -8,
                                N0 = 10 ^ 6,
                                k = 0.5,
                                min.maf = 0.05,
                                plot.debug = FALSE)

tess.res <- tess3(X = data.list$admixed.genotype, 
                  coord = data.list$coord, 
                  K = 2, 
                  ploidy = 1)

tikzDevice::tikz(paste0(fig.dir,"barplot.tex"), width = slide$width * 0.9,height = slide$heigth * 0.2,standAlone = TRUE)
par(mar = c(0,0,0,0))
barplot.default(t(tess.res$Q), border = NA, col = 2:3, axes=FALSE)
dev.off()
texi2dvi(paste0(fig.dir,"barplot.tex"),pdf = TRUE)
