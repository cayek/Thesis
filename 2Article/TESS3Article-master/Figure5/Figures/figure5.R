source("setup.R")
library(ggplot2)
library(tikzDevice)
library(tools)
library(tess3r)

################################################################################
# load res and data
res.file <- paste0(res.dir, "variogram.RData")
load(res.file)

################################################################################
# variogram.pl
variogram.pl <- ggplot(vario.gen, aes(x = h, y = semi.variance, size = size)) + 
  geom_point() + geom_vline(xintercept = 1.5, colour = "red") +
  labs(y = "Semivariance", 
       x = "Geographic distance (km $\\times 100$)") +
  theme_gray(base_size = 12) + 
  scale_size_continuous(range = c(1,3)) +
  guides(size = guide_legend(title = "Bin size")) +
  gtheme +
  theme(legend.position = c(0.8,0.3))


################################################################################
# load res and data
res.file <- paste0(res.dir, "err.K110.rep5.RData")
load(res.file)

################################################################################
# K selection
toplot <- err.df %>% group_by(K) %>% 
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
  gtheme 


tikzDevice::tikz(paste0(fig.dir, "figure5.tex"), width = fig.prop$width * page$width,
                 height = fig.prop$heigth * page$heigth * 0.65,standAlone = TRUE)
plot_grid(variogram.pl, selection.pl, nrow = 1, labels = c("A", "B"))
dev.off()
