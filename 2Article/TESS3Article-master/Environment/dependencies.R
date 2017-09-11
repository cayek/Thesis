################################################################################
# CRAN packages
cran.pkg <- c("data.table",
              "maps",
              "ggplot2",
              "ggthemes",
              "reshape2",
              "dplyr",
              "gridExtra",
              "cowplot",
              "ggmap",
              "xtable",
              "devtools",
              "tikzDevice",
              "raster",
              "sp",
              "rgeos",
              "rgdal",
              "cartography",
              "leaflet",
              "foreach",
              "doParallel",
              "DescTools",
              "permute",
              "rworldmap",
              "rasterVis",
              "crayon",
              "MASS",
              "Rcpp",
              "lintr",
              "lint")
new.packages <- cran.pkg[!(cran.pkg %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  install.packages(new.packages, repos = "http://cran.rstudio.com/")
}

################################################################################
# Bioconductor packages

bio.pkg <- c("qvalue", "LEA")
source("https://bioconductor.org/biocLite.R")
new.packages <- bio.pkg[!(bio.pkg %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  biocLite(new.packages)
}


################################################################################
 # github package
 
 devtools::install_github("BioShock38/TESS3_encho_sen@8e4b4cc87e12ceeb21d3b768564ed3a7bd17737c")
 devtools::install_github("BioShock38/TESS3_encho_sen@e0fac131439ef856171d778f8ed94cfbc63f1c41")
 # devtools::install_github("cayek/TESS3/tess3r@093d62e68e376384cd6f6b6fc5790dcf0db7c2cd")
 # do not pass why ?