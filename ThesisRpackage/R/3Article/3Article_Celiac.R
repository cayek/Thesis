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

