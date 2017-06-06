################################################################################
# Setup
library(ggplot2)
library(tess3r)
library(reshape2)
library(dplyr)
library(tikzDevice)
source("PlotParams.R")
################################################################################
# load res and data

load(paste0(data.dir,"call_method_75_TAIR8.RData"))

load(paste0(res.dir,"tess3.run.K3.sigma0.5.weightheat.res"))
Q <- tess3.run.K3.sigma0.5.weightheat$Q
# northen cluster
id.northers <- which(apply(Q, 1, which.max) == 2)
Q.notnorthers <- Q[!(1:nrow(Q) %in% id.northers),]
coord.notnorthers <- call_method_75_TAIR8.europe$coord[!(1:nrow(Q) %in% id.northers),]
id <- sort(coord.notnorthers[,1], index.return = TRUE)
Q <- rbind(Q[id.northers,], Q.notnorthers[id$ix,])

# plot(rep(1,5),col = col.palette[[2]],pch=19,cex=3)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 3
cols = gg_color_hue(n)

col.palette = list(
  colorRampPalette(c("white",cols[1]))(9)[5:9],
  colorRampPalette(c("white",cols[2]))(9)[5:9],
  colorRampPalette(c("white",cols[3]))(9)[5:9]
)


################################################################################
# Barplot and map

pdf(paste0(fig.dir,"Q_K3.pdf"), width = slide$width * 0.9,height = slide$heigth * 0.9)
par(mar = c(0,0,0,0))
layout(matrix(1:2, nrow = 2, ncol = 1), heights = c(6,1))
par(mar = c(0,0,0.5,0))
plot(Q = tess3.run.K3.sigma0.5.weightheat$Q, 
     coord = call_method_75_TAIR8.europe$coord, plot.type = "max", 
     resolution = c(300, 300), window = c(-16,42,33,67), background = TRUE, 
     raster.filename = NULL, interpolation.function = kriging(), col = NULL, 
     col.palette = col.palette, map = TRUE, palette.step = 9, 
     axes = FALSE, xlab = '', ylab = '', cex = 0.25)

barplot.default(t(Q), border = NA, col = cols, axes = FALSE)
dev.off()


