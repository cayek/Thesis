source("setup.R")
library(tikzDevice)
library(gridExtra)
library(cowplot)
library(dplyr)


load(paste0(res.dir,"auc.RData"))
toplot <- df %>%
  group_by(method, m.ms) %>%
  mutate(auc.mean = mean(auc), N = length(auc), sd = sd(auc), se = sd / sqrt(N)) %>%
  rename(Methods = method )
levels(toplot$Methods)[1] <- "APLS"
levels(toplot$Methods)[3] <- "before-admixure"

pl <- ggplot(toplot ,aes(x = m.ms, y = auc.mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_errorbar(aes(ymin = auc.mean - se, ymax = auc.mean + se), width = 0.0) +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Intensity of selection ($m/m_s$)") +
  ylab("AUC") +
  gtheme +
  theme(legend.position = c(0.8,0.2)) +
  scale.linetype +
  scale.color
pl



tikzDevice::tikz(paste0(fig.dir,"figure3.tex"), width = fig.prop$width * page$width,
                 height = fig.prop$heigth * page$heigth, standAlone = TRUE)
pl
dev.off()
# bup <- getwd()
# setwd(fig.dir)
# tools::texi2dvi("figure3.tex",pdf = TRUE)
# setwd(bup)
