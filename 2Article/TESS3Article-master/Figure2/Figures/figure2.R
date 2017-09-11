source("setup.R")
library(tikzDevice)
library(gridExtra)
library(cowplot)
library(dplyr)

load(paste0(res.dir,"rmse.fst.RData"))
toplot <- df %>%
  mutate(n = paste0("$n = ",n, "$")) %>%
  group_by(nsites.neutral) %>%
  mutate(L = round(mean(L)), L = paste0("$L \\sim 10^", floor(log(L,base = 10)), "$")) %>%
  group_by(method, m.neutral, n, L) %>%
  mutate(Fst = mean(Fst), rmse.mean = mean(rmseQ), N = length(rmseQ), sd = sd(rmseQ), se = sd / sqrt(N)) %>%
  rename(Methods = method )
levels(toplot$Methods)[1] <- "APLS"
pl <- ggplot(toplot ,aes(x = Fst.theorical, y = rmse.mean, col = Methods, shape = Methods)) +
  geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se), width = 0.0) +
  geom_line() +
  geom_point() +
  facet_grid(L ~ n) +
  theme_bw() +
  # xlab("$Fst = 1 / (1 + 4 N_0 m)$") +
  xlab("Fixation index $(F_{\\rm ST})$") +
  ylab("RMSE") +
  gtheme +
  theme(legend.position = c(0.85,0.3)) +
  scale.color
pl

tikzDevice::tikz(paste0(fig.dir,"figure2.tex"), width = fig.prop$width * page$width,
                 height = fig.prop$heigth * page$heigth, standAlone = TRUE)
pl
dev.off()
# bup <- getwd()
# setwd(fig.dir)
# tools::texi2dvi("figure2.tex",pdf = TRUE)
# setwd(bup)
