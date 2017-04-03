#' @export
Tess3Sampler <- function(n = 100,
                         nsites.neutral = 1e4,
                         nsites.selected = 0,
                         crossover.proba = 0.25 * 10 ^ -8,
                         m.neutral = 0.25 * 10 ^ -6,
                         m.selected = NULL,
                         mutation.rate.per.site = 0.25 * 10 ^ -7,
                         N0 = 10 ^ 6,
                         k = 0.5,
                         min.maf = 0.05) {
  structure(list(n = n,
                 nsites.neutral = nsites.neutral,
                 nsites.selected = nsites.selected,
                 crossover.proba = crossover.proba,
                 m.neutral = m.neutral,
                 m.selected = m.selected,
                 mutation.rate.per.site = mutation.rate.per.site,
                 N0 = N0,
                 k = k,
                 min.maf = min.maf),
            class = c("Tess3Sampler","Sampler"))
}

#' Sample data
#'
#'
#' @export
sampl.Tess3Sampler <- function(s) {

  out <- capture.output(dat <- Article2.env$sample.data(s))
  DebugMessage("Tess3Sampler", out)
  dat$G <- dat$admixed.genotype
  dat$Fst.theorical <- 1 / (1 + 4 * dat$N0 * dat$m.neutral)
  dat
}
