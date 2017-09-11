source("setup.R")
library(ggplot2)
library(tikzDevice)
library(tools)
library(tess3r)

################################################################################
# load res and data
res.file <- paste0(res.dir, "tess3K6.sigmaHeat1.5.RData")
load(res.file)
res.file <- paste0(res.dir, "snmfK6.RData")
load(res.file)
res.file <- paste0(res.dir, "snpsTAIR9vepTAIR10.RData")
load(res.file)
data.file <-
  paste0(data.dir, "AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.RData")
load(data.file)

################################################################################
# flowering genes
# search on http://plants.ensembl.org/
# SHORT VEGETATIVE PHASE (SVP), a MADS box gene that negatively regulates the transition to flowering (Differentiating Fennoscandia and Eastern Europe/Russia)
flowering.gene <- vep.res %>% filter(Gene == "AT2G22540") %>% mutate(label = "SVP")
# COP1-interacting protein 4.1 (CIP4.1)
flowering.gene <- rbind(flowering.gene, vep.res %>% filter(Gene == "AT4G00930") %>% mutate(label = "CIP4.1"))
# FRIGIDA (FRI)
flowering.gene <- rbind(flowering.gene, vep.res %>% filter(Gene == "AT4G00650") %>% mutate(label = "FRI"))
# FLOWERING LOCUS C (FLC),
flowering.gene <- rbind(flowering.gene, vep.res %>% filter(Gene == "AT5G10140") %>% mutate(label = "FLC"))
# DELAY OF GERMINATION 1 (DOG1)
flowering.gene <- rbind(flowering.gene, vep.res %>% filter(Gene == "AT5G45830")%>% mutate(label = "DOG1"))


################################################################################
# Plot TESS3 manhattanplot
toplot <- data.frame(fst = tess3Main.obj$Fst,
                     pvalue = tess3Main.obj$pvalue,
                     call_method_75_TAIR9.europe$locus.coord,
                     index = seq_along(tess3Main.obj$Fst)) %>%
  mutate(Location = paste0(Chromosome,":",Positions))

alert <- merge(toplot, flowering.gene, by = c("Location"))
label <- alert %>% group_by(Gene) %>% filter(row_number(index)==1)
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
toplot <- toplot %>% filter(pvalue != 0.0)
pl <- ggplot(toplot, aes(x = index, y = -log(pvalue),
                         color = as.factor(Chromosome), fill = Chromosome)) +
  geom_point() +
  labs(y = "-log(pvalue)", x = "locus index") +
  scale_y_continuous(limits = c(0,510)) +
  scale_x_continuous(breaks = sapply(1:5, function(i) mean(toplot[toplot$Chromosome == i, ]$index)),
                     labels = 1:5) +
  xlab("Chromosome") +
  ylab("$-\\log(p{\\rm -value})$") +
  gtheme +
  theme(legend.position = "none")



tikzDevice::tikz(paste0(fig.dir,"manhattanplot.tex"), width = fig.prop$width * page$width,
                 height = fig.prop$heigth * page$heigth, standAlone = TRUE)
pl
dev.off()
#
# pdf(paste0(fig.dir,"manhattanplot.pdf"), width = page$width * 0.8,
#     height = page$heigth * 0.4)
# pl
# dev.off()

# png(paste0(fig.dir,"manhattanplot.png"), width = page$width * 0.8,
#     height = page$heigth * 0.4,res = 600, units = "in")
# pl
# dev.off()
