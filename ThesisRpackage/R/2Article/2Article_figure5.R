#' @export
Article2_figure5 <- function() {

  TestRequiredPkg(c("cowplot"))

  exp <- retrieveExperiment(58)
  assertthat::assert_that(exp$name == "2Article_figure5_TAIR9")



  variogram.pl <- ggplot(exp$vario.gen, aes(x = h, y = semi.variance, size = size)) +
    geom_point(shape = 1) +
    geom_vline(xintercept = 1.5, colour = "red") +
    labs(y = "Semivariance",
         x = "Geographic distance($100$ km)") +
    theme_gray(base_size = 12) +
    scale_size_continuous(range = c(1,3)) +
    guides(size = guide_legend(title = "Bin size", nrow = 3)) +
    Article2.env$gtheme +
    theme(legend.position = c(0.6,0.25)) +
    scale_shape_discrete(solid = FALSE)



  ################################################################################
  # K selection
  toplot <- exp$err.df %>% group_by(K) %>%
    summarise(med = median(rmse), min = min(rmse), max = max(rmse),
              mean = mean(rmse), sd = sd(rmse), se = sd/sqrt(length(rmse)))

  selection.pl <- ggplot(toplot) +
    geom_point(aes(x = as.factor(K), y = med)) +
    geom_line(aes(x = K, y = med)) +
    #geom_errorbar(aes(x = K, y = med,
    #                  ymin=min, ymax=max), width=.1) +
    labs(y = "Cross validation error", x = "$K$") +
    theme_gray() +
    theme(legend.position = "none") +
    Article2.env$gtheme



  cowplot::plot_grid(variogram.pl, selection.pl, nrow = 1, labels = c("A", "B"))

}


#' @export
Article2_manhattanplot <- function() {


  exp <- retrieveExperiment(58)
  assertthat::assert_that(exp$name == "2Article_figure5_TAIR9")

  ################################################################################
  # load  data
  data.dir <- "~/Projects/Thesis/Data/"
  data.file <-
    paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
  load(data.file)

  ################################################################################
  # flowering genes
  # search on http://plants.ensembl.org/
  # SHORT VEGETATIVE PHASE (SVP), a MADS box gene that negatively regulates the transition to flowering (Differentiating Fennoscandia and Eastern Europe/Russia)
  flowering.gene <- exp$vep.res %>% dplyr::filter(Gene == "AT2G22540") %>% dplyr::mutate(label = "SVP")
  # COP1-interacting protein 4.1 (CIP4.1)
  flowering.gene <- rbind(flowering.gene, exp$vep.res %>% dplyr::filter(Gene == "AT4G00930") %>% dplyr::mutate(label = "CIP4.1"))
  # FRIGIDA (FRI)
  flowering.gene <- rbind(flowering.gene, exp$vep.res %>% dplyr::filter(Gene == "AT4G00650") %>% dplyr::mutate(label = "FRI"))
  # FLOWERING LOCUS C (FLC),
  flowering.gene <- rbind(flowering.gene, exp$vep.res %>% dplyr::filter(Gene == "AT5G10140") %>% dplyr::mutate(label = "FLC"))
  # DELAY OF GERMINATION 1 (DOG1)
  flowering.gene <- rbind(flowering.gene, exp$vep.res %>% dplyr::filter(Gene == "AT5G45830") %>% dplyr::mutate(label = "DOG1"))


  ################################################################################
  # Plot TESS3 manhattanplot
  toplot <- data.frame(fst = exp$tess3Main.obj$Fst,
                       pvalue = exp$tess3Main.obj$pvalue,
                       call_method_75_TAIR9.europe$locus.coord,
                       index = seq_along(exp$tess3Main.obj$Fst)) %>%
    dplyr::mutate(Location = paste0(Chromosome,":",Positions))

  alert <- merge(toplot, flowering.gene, by = c("Location"))
  label <- alert %>% group_by(Gene) %>% filter(row_number(index) == 1)
  label$index[2] = label$index[2] + 8000
  label$index[3] = label$index[3] - 8000
  ## plot with annotation
  pl <- ggplot(toplot, aes(x = index, y = -log(pvalue),
                           color = as.factor(Chromosome), fill = Chromosome)) +
    geom_point() +
    labs(y = "-log(pvalue)", x = "locus index") +
    theme_gray() +
    theme(legend.position = "none") +
    geom_point(data = alert, colour = "red") +
    geom_text(data = label, aes(x = index, y = 0, label = label), vjust = 1.8, check_overlap = FALSE)

  ## plot without annotation
  toplot <- toplot %>% dplyr::filter(pvalue != 0.0)
  pl <- ggplot(toplot, aes(x = index, y = -log(pvalue),
                           color = as.factor(Chromosome), fill = Chromosome)) +
    geom_point(size = 0.25) +
    labs(y = "-log(pvalue)", x = "locus index") +
    scale_y_continuous(limits = c(0,510)) +
    scale_x_continuous(breaks = sapply(1:5, function(i) mean(toplot[toplot$Chromosome == i, ]$index)),
                       labels = 1:5) +
    xlab("Chromosome") +
    ylab("log(p-value)") +
    Article2.env$gtheme +
    theme(legend.position = "none") +
    scale_color_manual(values = c(Article2.env$cbPalette[6], Article2.env$cbPalette[2],
                                  Article2.env$cbPalette[6], Article2.env$cbPalette[2],
                                  Article2.env$cbPalette[6]))


  ## rm data
  rm(call_method_75_TAIR9.europe)

  pl
}

#' @export
Article2_map <- function(cols = NULL) {

  TestRequiredPkg(c("sp", "raster",
                    "rworldmap", "rgeos", "rasterVis",
                    "fields", "reshape2", "cowplot"))

  exp <- retrieveExperiment(58)
  assertthat::assert_that(exp$name == "2Article_figure5_TAIR9")

  ################################################################################
  # load data
  data.dir <- "~/Projects/Thesis/Data/"
  data.file <-
    paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
  load(data.file)
  coord <- call_method_75_TAIR9.europe$coord
  rm(call_method_75_TAIR9.europe)

  ################################################################################
  # Q
  Q <- exp$tess3Main.obj$Q
  # northen cluster
  id.northers <- which(apply(Q, 1, which.max) == 2)
  Q.notnorthers <- Q[!(1:nrow(Q) %in% id.northers),]
  coord.notnorthers <- coord[!(1:nrow(Q) %in% id.northers),]
  id <- sort(coord.notnorthers[,1], index.return = TRUE)
  Q.ordered <- rbind(Q[id.northers,], Q.notnorthers[id$ix,])

  ################################################################################
  # Color palette
  if (is.null(cols)) {
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    n = 6
    cols = gg_color_hue(n)
  }

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
  require(sp)
  require(raster)
  require(rworldmap)
  require(rgeos)
  require(rasterVis)
  require(fields)

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
  # ## compute color for each tile
  # color <- function(coef, col.palette, col.breaks) {
  #   max.i <- which.max(coef)
  #   c <- max(which(col.breaks[,max.i] - as.numeric(coef[max.i]) >= 0)[1] - 1,1)
  #   return(col.palette[[max.i]][c])
  #   # return(c)
  # }
  # toplot$color <- apply(toplot[3:8], 1,
  #                       function(r) color(r, col.palette, col.breaks))

  ## with removed artefact
  color.rm.art <- function(r, col.palette, col.breaks) {
    coef <- r[3:8]
    pos <- r[1:2]
    if (pos[1] > 28 && pos[2] < 46) {
      max.i <- 5
    } else {
      max.i <- which.max(coef)
    }
    c <- max(which(col.breaks[,max.i] - as.numeric(coef[max.i]) >= 0)[1] - 1,1)
    return(col.palette[[max.i]][c])
    # return(c)
  }
  toplot$color <- apply(toplot[1:8], 1,
                        function(r) color.rm.art(r, col.palette, col.breaks))

  mappl <- ggplot() +
    geom_tile(data = toplot, aes(x = x, y = y, fill = color)) +
    scale_fill_identity() +
    geom_path(data = europe, aes(x = long, y = lat, group = group)) +
    coord_equal() +
    geom_point(data = as.data.frame(coord), aes(x = long, y = lat), size = 0.1) +
    Article2.env$gtheme +
    xlab("Longitute") +
    ylab("Latitude")

  ################################################################################
  # barplot
  toplot <- data.frame(Q.ordered, index = seq_along(Q.ordered[,1])) %>% reshape2::melt(id = "index")
  brplot <- ggplot(toplot, aes(x = index, y = value)) +
    geom_bar(stat = "identity", aes(color = variable)) +
    Article2.env$gtheme +
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("Individual") +
    ylab("Admixture\n\ coefficient (Q)") +
    scale_y_continuous(breaks = c(0.0,0.5,1.0)) +
    scale_color_manual(values = cols)

  ################################################################################
  # Plot


  pl <- cowplot::plot_grid(mappl, brplot, ncol = 1, labels = c("A", "B"), rel_heights = c(3,1), vjust = c(1.5, -0.5))
}
