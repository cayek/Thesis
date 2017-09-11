source("setup.R")
library(tikzDevice)
library(gridExtra)
library(cowplot)

################################################################################
# plot
load(paste0(res.dir,"L.rmse.RData"))
load(paste0(res.dir,"n.rmse.RData"))
toplot <- df.n  %>%
  melt(id = c("n", "rep", "L", "method"), value.name = "rmse") %>%
  group_by(n, method, variable) %>%
  mutate(rmse.mean = mean(rmse), N = length(rmse), sd = sd(rmse), se = sd / sqrt(N)) %>%
  mutate(Methods = method)

pl.n.A <- ggplot(toplot %>% filter(variable == "rmseG"),aes(x = n, y = rmse.mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se), width = .1) +
  geom_line() +
  geom_point() +
  xlab("Number of individuals ($n$)") +
  ylab("RMSE") +
  gtheme +
  theme(legend.position = "none") +
  scale.linetype +
  scale.color

pl.n.B <- ggplot(toplot %>% filter(variable == "rmseQ"),aes(x = n, y = rmse.mean, col = Methods, linetype = Methods, shape = Methods)) +
  geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se), width = .1) +
  geom_line() +
  geom_point() +
  xlab("Number of individuals ($n$)") +
  ylab("") +
  gtheme +
  theme(legend.position = c(0.75,0.70)) +
  scale.linetype +
  scale.color

toplot <- df.L  %>%
  melt(id = c("nsites.neutral", "rep", "L", "method"), value.name = "rmse") %>%
  group_by(nsites.neutral, method, variable) %>%
  mutate(rmse.mean = mean(rmse), N = length(rmse), sd = sd(rmse), se = sd / sqrt(N), L = mean(L))

pl.L.C <- ggplot(toplot %>% filter(variable == "rmseG"), aes(x = L, y = rmse.mean, col = method, linetype = method, shape = method)) +
  geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se), width = .1) +
  geom_line() +
  geom_point() +
  xlab("Number of loci ($L$)") +
  ylab("RMSE") +
  gtheme +
  theme(legend.position = "none") +
  scale.linetype +
  scale.color

pl.L.D <- ggplot(toplot %>% filter(variable == "rmseQ"), aes(x = L, y = rmse.mean, col = method, linetype = method, shape = method)) +
  geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se), width = .1) +
  geom_line() +
  geom_point() +
  xlab("Number of loci ($L$)") +
  ylab("") +
  gtheme +
  theme(legend.position = "none") +
  scale.linetype +
  scale.color

#options(tikzDefaultEngine = "luatex")
tikzDevice::tikz(paste0(fig.dir,"figure1.tex"), width = (fig.prop$width * page$width),
  height = (fig.prop$heigth * page$heigth), standAlone = TRUE)
plot_grid(pl.n.A, pl.n.B, pl.L.C, pl.L.D, ncol = 2, labels = c("A", "B", "C", "D"))
dev.off()
# bup <- getwd()
# setwd(fig.dir)
# tools::texi2dvi("figure1.tex", pdf = TRUE)
# setwd(bup)
