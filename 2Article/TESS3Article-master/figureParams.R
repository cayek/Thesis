#############################################################
# color and linetype
color <- list(TESS3 = "blue",
              APLS = "orange",
              AQP = "darkgrey",
              snmf = "chartreuse4",
              before.admixure = "azure4")
linetype <- list(TESS3 = "solid",
                 APLS = "solid",
                 AQP = "dashed",
                 snmf = "solid",
                 before.admixure = "dashed")

################################################################################
# ggplot param
library(ggplot2)

scale.linetype <- scale_linetype_manual(values = c("APLS" = linetype$APLS,
                                                   "TESS3" = linetype$TESS3,
                                                   "AQP" = linetype$AQP,
                                                   "sNMF" = linetype$snmf,
                                                   "before-admixure" = linetype$before.admixure))
scale.color <-  scale_color_manual(values = c("APLS" = color$APLS,
                                              "TESS3" = color$TESS3,
                                              "AQP" = color$AQP,
                                              "sNMF" = color$snmf,
                                              "before-admixure" = color$before.admixure))

gtheme <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = NA))

################################################################################
# page size
page <- list(width = 8.26772, heigth = 11.69291)
fig.prop <- list(width = 0.65, heigth = 0.35)
