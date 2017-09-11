source("setup.R")
library(ggplot2)
library(tikzDevice)
library(tools)
library(tess3r)
library(ggthemes)

################################################################################
# load res and data
res.file <- paste0(res.dir, "tess3K6.sigmaHeat1.5.RData")
load(res.file)

data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)
coord <- call_method_75_TAIR9.europe$coord
rm(call_method_75_TAIR9.europe)

################################################################################
# Q
Q <- tess3Main.obj$Q
# northen cluster
id.northers <- which(apply(Q, 1, which.max) == 2)
Q.notnorthers <- Q[!(1:nrow(Q) %in% id.northers),]
coord.notnorthers <- coord[!(1:nrow(Q) %in% id.northers),]
id <- sort(coord.notnorthers[,1], index.return = TRUE)
Q.ordered <- rbind(Q[id.northers,], Q.notnorthers[id$ix,]) 

################################################################################
# Color palette

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 6
cols = gg_color_hue(n)

col.palette = list(
  colorRampPalette(c("white",cols[1]))(9)[5:9],
  colorRampPalette(c("white",cols[2]))(9)[5:9],
  colorRampPalette(c("white",cols[3]))(9)[5:9],
  colorRampPalette(c("white",cols[4]))(9)[5:9],
  colorRampPalette(c("white",cols[5]))(9)[5:9],
  colorRampPalette(c("white",cols[6]))(9)[5:9]
)
# plot(rep(1,5),col = col.palette[[2]],pch=19,cex=3)


################################################################################
# Interpolation
library(sp)
library(raster)
library(rworldmap)
library(rgeos)
library(rasterVis)
library(fields)

## param 
window <- c(-16,42,33,67)
resolution <- c(300, 300)
theta <- 10

## get europe
newmap <- getMap(resolution = "low")
CP <- as(extent(window), "SpatialPolygons")
europe <- gIntersection(newmap, CP)
# Or
# europe <- crop(newmap, extent(window))
plot(europe)

## make grid
raster.grid <- raster(extent(window), ncol = resolution[1], nrow = resolution[2], vals = 1)
# plot(grid)

## interpolation with krig
interpol <- stack()
for (j in seq_along(Q[1,])) {
  model <- Krig(coord, Q[,j], theta = theta)
  interpol <- stack(interpolate(raster.grid, model), interpol)
}
interpol <- mask(interpol, europe)
# plot(interpol)
# levelplot(interpol)
## plot with tess3r package

# plot(Q = Q,
#      coord = coord, plot.type = "max",
#      resolution = c(300, 300), window = c(-16,42,33,67), background = TRUE,
#      raster.filename = NULL, interpolation.function = kriging(), col = NULL,
#      col.palette = col.palette, map = TRUE, palette.step = 9,
#      axes = FALSE, xlab = '', ylab = '', cex = 0.25)


################################################################################
# Plot map

toplot <- data.frame(rasterToPoints(interpol))
## compute breaks
col.breaks <- apply(toplot[3:8], 2, 
                    function(c) seq(min(c), 
                                    max(c), 
                                    length.out = length(col.palette[[1]]) + 1))
## compute color for each tile
color <- function(coef, col.palette, col.breaks) {
  max.i <- which.max(coef)
  c <- max(which(col.breaks[,max.i] - as.numeric(coef[max.i]) >= 0)[1] - 1,1)
  return(col.palette[[max.i]][c])
  # return(c)
}
toplot$color <- apply(toplot[3:8], 1, 
                      function(r) color(r, col.palette, col.breaks))
mappl <- ggplot() + 
  geom_tile(data = toplot, aes(x = x, y = y, fill = color)) + 
  scale_fill_identity() +
  geom_path(data = europe, aes(x = long, y = lat, group = group)) +
  coord_equal() + 
  geom_point(data = as.data.frame(coord), aes(x = long, y = lat), size = 0.1) + 
  gtheme +
  xlab("Longitute") +
  ylab("Latitude")

################################################################################
# barplot
toplot <- data.frame(Q.ordered, index = seq_along(Q.ordered[,1])) %>% melt(id = "index")
brplot <- ggplot(toplot, aes(x = index, y = value)) + 
  geom_bar(stat = "identity", aes(color = variable)) +
  gtheme +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Individual") +
  ylab("Admixture\n\ coefficient ($Q$)") +
  scale_y_continuous(breaks = c(0.0,0.5,1.0))

################################################################################
# Plot
tikzDevice::tikz(paste0(fig.dir,"map.tex"), width = fig.prop$width * page$width,
                 height = fig.prop$heigth * page$heigth, standAlone = TRUE)
plot_grid(mappl, brplot, ncol = 1, labels = c("A", "B"), rel_heights = c(3,1), vjust = c(1.5, -0.5))
dev.off()

# pdf(paste0(fig.dir,"map_R.pdf"),width = 0.85 * page$width,
#               height = 0.5 * page$heigth)
# plot_grid(mappl, brplot, ncol = 1, labels = c("A", "B"), rel_heights = c(3,1), vjust = c(0, 0.1))
# dev.off()

