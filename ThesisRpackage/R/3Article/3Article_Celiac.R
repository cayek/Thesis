#' @export
Article3_Celiac_sampler <- function() {
  
  s <- TrueSampler(G.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/G.rds",
                   X.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/X.rds",
                   outlier.file = NULL,
                   ind.clumping = "~/Projects/Thesis/Data/ThesisDataset/3Article/Celiac/ind.clumpling.rds",
                   n = NULL,
                   L = NULL)
  s
}

