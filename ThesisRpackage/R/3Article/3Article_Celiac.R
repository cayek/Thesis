#' @export
Article3_Celiac_sampler <- function(clumped = TRUE) {

  if (clumped) {
    s <- TrueSampler(G.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/G_clumped.rds",
                     X.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/X.rds",
                     outlier.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/gwas_catalog_candidates_clumped.rds",
                     n = NULL,
                     L = NULL)
  } else {
    s <- TrueSampler(G.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/G.rds",
                     X.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/X.rds",
                     outlier.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/gwas_catalog_candidates.rds",
                     ind.clumping = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/ind.clumpling.rds",
                     n = NULL,
                     L = NULL)
  }
  s
}

#' @export
Article3_Celiac_lm <- function(m, dat) {

  ## lm function
  lm.functor <- lm_zscore(calibrate = FALSE,
                      sigma.computation = "lm",
                      correctionByC = FALSE,
                      center = FALSE)

  ## we split the dataset
  L <- ncol(dat$G)
  d <- ncol(dat$X)
  nb.chunks <- 20
  chunks <- split(1:L, ceiling(1:L / nb.chunk))
  it <- 1
  dat.aux <- list(X = dat$X)
  m$pvalue <- matrix(NA, d, L)
  m$score <- matrix(NA, d, L)
  for (ch in chunks) {
    flog.trace("Chunk", it, "/", nb.chunks)
    dat.aux$G <- dat$G[,ch]
    m.aux <- lm.functor$fun(m, dat.aux)
    m$pvalue[,ch] <- m.aux$pvalue
    m$score[,ch] <- m.aux$score
    it <- it + 1
  }
  m
}
