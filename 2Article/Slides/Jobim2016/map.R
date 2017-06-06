################################################################################
# Setup
library(ggplot2)
library(tess3r)
library(ggmap)
library(tikzDevice)
library(tools)
library(ggthemes)
source("PlotParams.R")

################################################################################
# Data
load(paste0(data.dir,"call_method_75_TAIR8.RData"))

################################################################################
# Plot

sbbox <- c(min(call_method_75_TAIR8.europe$coord[,1]), min(call_method_75_TAIR8.europe$coord[,2]), 
           max(call_method_75_TAIR8.europe$coord[,1]), max(call_method_75_TAIR8.europe$coord[,2]))
toplot = data.frame(x = call_method_75_TAIR8.europe$coord[,1], 
                    y = call_method_75_TAIR8.europe$coord[,2], 
                    name = rownames(call_method_75_TAIR8.europe$coord))
sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
pl <- ggmap(sq_map) + geom_point(data = toplot, aes(x = x, y = y, col = "red"), size = 0.5) +
  coord_map() +
  theme_map() + 
  theme(legend.position = "none")


tikzDevice::tikz(paste0(fig.dir,"map.tex"), width = slide$width * 0.5,height = slide$heigth * 0.5,standAlone = TRUE)
#par(mar = c(0,0,0,0))
pl
dev.off()
texi2dvi(paste0(fig.dir,"map.tex"),pdf = TRUE)