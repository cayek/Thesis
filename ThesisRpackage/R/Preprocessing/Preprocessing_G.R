#' @export 
Preprocessing_filter_maf <- function(G, maf.threshold = 0.05, plot = FALSE) {
  ploidy <- max(G)
  maf <- apply(G, 2, function(l){p <- mean(l) / ploidy;min(p, 1 - p)})
  if(plot) {
    hist(maf)
  }
  out <- maf <= maf.threshold
  flog.trace("proportion of removed loci = ", mean(out))
  G[,!out]
}

#' @export 
Preprocessing_filter_na <- function(G, na.threshold = 0, plot = FALSE) {
  nas <- apply(G, 2, function(l){mean(is.na(l))})
  if(plot) {
    hist(nas)
  }
  out <- nas > na.threshold
  flog.trace("proportion of removed loci = ", mean(out))
  G[,!out]
}

#' @export 
Preprocessing_filter_sd <- function(G, sd.threshold = 0, plot = FALSE) {
  sds <- apply(G, 2, sd)
  if(plot) {
    hist(sds)
  }
  out <- sds <= sd.threshold
  flog.trace("proportion of removed loci = ", mean(out))
  G[,!out]
}


#' @export 
Preprocessing_standardisation <- function(G) {
  scale(G)
}
