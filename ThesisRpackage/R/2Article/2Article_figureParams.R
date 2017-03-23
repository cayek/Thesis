#############################################################
# color and linetype
Article2.env$color <- list(TESS3 = "orange",
              APLS = "blue",
              AQP = "darkgrey",
              snmf = "chartreuse4",
              before.admixure = "azure4")
Article2.env$linetype <- list(TESS3 = "twodash",
                              APLS = "solid",
                              AQP = "dashed",
                              snmf = "twodash",
                              before.admixure = "dashed")

################################################################################
# ggplot param
Article2.env$cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


Article2.env$scale.linetype <- scale_linetype_manual(values = c("APLS" = Article2.env$linetype$APLS,
                                                                "TESS3" = Article2.env$linetype$TESS3,
                                                                "AQP" = Article2.env$linetype$AQP,
                                                                "sNMF" = Article2.env$linetype$snmf,
                                                                "before-admixure" = Article2.env$linetype$before.admixure))
Article2.env$scale.color <-  scale_color_manual(values = c("APLS" = Article2.env$color$APLS,
                                                           "TESS3" = Article2.env$color$TESS3,
                                                           "AQP" = Article2.env$color$AQP,
                                                           "sNMF" = Article2.env$color$snmf,
                                                           "before-admixure" = Article2.env$color$before.admixure))

Article2.env$cb.scale.color <-  scale_color_manual(values = Article2.env$cbPalette)

Article2.env$gtheme <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = NA))

Article2.env$point.size = 2

Article2.env$errorbar.width.ratio = 0.02

################################################################################
# page size
Article2.env$page <- list(width = 8.26772, height = 11.69291)
Article2.env$text <- list(width = 5, height = 7,63889)
Article2.env$fig.prop <- list(width = 0.65, height = 0.35)
Article2.env$fig <- list(width = 1.0 * Article2.env$text$width, height = 0.6 * Article2.env$text$height)
################################################################################
# output
Article2.env$fig.dir <- "~/Projects/Thesis/2Article/Article/Figures"
