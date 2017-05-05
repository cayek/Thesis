#' @export 
Preprocessing_filter_maf <- function(G, maf.threshold = 0.05, plot = FALSE) {
  ploidy <- max(G)
  maf <- apply(G, 2, function(l){p <- mean(l) / ploidy;min(p, 1 - p)})
  if(plot) {
    hist(maf)
  }
  out.index <- which(maf <= maf.threshold)
  G[,-out.index]
}

#' @export 
Preprocessing_filter_sd <- function(G, sd.threshold = 0, plot = FALSE) {
  sds <- apply(G, 2, sd)
  if(plot) {
    hist(sds)
  }
  out.index <- which(sds <= sd.threshold)
  G[,-out.index]
}


#' @export 
Preprocessing_standardisation <- function(G) {
  scale(G)
}
