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

#' Just to work with MethodBatchExperiment
#'
#' @export
Article3_Celiac_fake_sampler <- function() {

  s <- TrueSampler(G.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/G_fake.rds",
                   X.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/X.rds",
                   outlier.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/gwas_catalog_candidates.rds",
                   n = NULL,
                   L = NULL)
  s
}

#' @export
Article3_Celiac_list_G_split_files<- function() {
  file.pattern <- "G_[0-9]*.rds$"
  files <- list.files("~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/G_splits/")
  files <- grep(file.pattern, files, value = TRUE)
  paste0( "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/G_splits/", files)
}

#' @export
Article3_Celiac_lm <- function(m, dat) {

  ## lm function
  lm.functor <- lm_zscore(calibrate = FALSE,
                      sigma.computation = "lm",
                      correctionByC = FALSE,
                      center = FALSE)

  d <- ncol(dat$X)
  L <- ncol(dat$G)

  ## files
  files <- Article3_Celiac_list_G_split_files()

  ## loop
  m$pvalue <- matrix(NA, d, L)
  m$score <- matrix(NA, d, L)
  for (f in files) {
    flog.trace(f)
    ## read data
    aux <- readRDS(f)
    dat$G <- aux$G
    ch <- aux$ch
    rm(aux)
    gc()

    ## run method
    m.aux <- lm.functor$fun(m, dat)
    m$pvalue[,ch] <- m.aux$pvalue
    m$score[,ch] <- m.aux$score
  }
  m
}
