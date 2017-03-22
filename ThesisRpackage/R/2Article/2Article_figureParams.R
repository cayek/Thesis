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

Article2.env$gtheme <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = NA))

################################################################################
# page size
Article2.env$page <- list(width = 8.26772, heigth = 11.69291)
Article2.env$fig.prop <- list(width = 0.65, heigth = 0.35)

################################################################################
# output
Article2.env$fig.dir <- "~/Projects/Thesis/2Article/Article/Figures"
