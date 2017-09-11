source("setup.R")
library(tikzDevice)
library(gridExtra)
library(cowplot)
library(dplyr)


g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

load(paste0(res.dir,"runtimes.RData"))

toplot <- df.n  %>% group_by(method, n) %>%
  mutate(mean = mean(it), N = length(it), sd = sd(it), se = sd / sqrt(N)) %>%
  rename(Methods = method)
pl.it.n <- ggplot(toplot ,aes(x = n, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.0) +
  theme_bw() +
  xlab("") +
  ylab("Number\nof iterations") +
  gtheme +
  theme(legend.position = "none") +
  scale.linetype +
  scale.color

toplot <- df.n  %>% group_by(n, method) %>%
  mutate(mean = mean(time.per.it.mean), N = length(time.per.it.mean), sd = sd(time.per.it.mean), se = sd / sqrt(N)) %>%
  rename(Methods = method)
pl.time.n <- ggplot(toplot ,aes(x = n, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  theme_bw() +
  scale_y_log10() +
  xlab("Number of individuals ($n$)") +
  ylab("Time per iteration \n(seconds)") +
  gtheme +
  theme(legend.position = "none") +
  scale.linetype +
  scale.color

toplot <- df.L  %>% group_by(method, L) %>%
  mutate(mean = mean(it), N = length(it), sd = sd(it), se = sd / sqrt(N)) %>%
  rename(Methods = method)
pl.it.L <- ggplot(toplot ,aes(x = L, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  theme_bw() +
  xlab("") +
  ylab("") +
  gtheme +
  theme(legend.position = "none") +
  scale.linetype +
  scale.color

toplot <- df.L  %>% group_by(L, method) %>%
  mutate(mean = mean(time.per.it.mean), N = length(time.per.it.mean), sd = sd(time.per.it.mean), se = sd / sqrt(N)) %>%
  rename(Methods = method)

pl.time.L <- ggplot(toplot ,aes(x = L, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  theme_bw() +
  scale_y_log10() +
  xlab("Number of loci ($L$)") +
  ylab("") +
  gtheme +
  theme(legend.position = c(0.75,0.3)) +
  scale.linetype +
  scale.color


tikzDevice::tikz(paste0(fig.dir,"figure4.tex"), width = fig.prop$width * page$width,
                 height = fig.prop$heigth * page$heigth, standAlone = TRUE)
plot_grid(pl.it.n, pl.it.L, pl.time.n, pl.time.L, ncol = 2, labels = c("A", "B", "C", "D"))
dev.off()
# bup <- getwd()
# setwd(fig.dir)
# tools::texi2dvi("figure4.tex",pdf = TRUE)
# setwd(bup)
