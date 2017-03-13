################################################################################
# maf filter

#' run the sampler and filter minor allele frequency
#'
#'
#' @export
MafFilterSampler <- function(sampler, maf.threshold) {
  s <- Sampler()
  s$sampler <- sampler
  s$maf.threshold <- maf.threshold
  class(s) <- c("MafFilterSampler", class(s))
  s
}



#' @export
sampl.MafFilterSampler <- function(s){
  # sample
  dat <- sampl(s$sampler)
  #filter
  ## estimate propotion a binomial distrubution p_j = mean(G_j) / ploidy
  ploidy <- max(dat$G)
  stop("TODO")
  # return
  dat
}

################################################################################
# var filter

#' run the sampler and filter column without variation
#'
#'
#' @export
VarFilterSampler <- function(sampler) {
  stop("TOTEST")
  s <- Sampler()
  s$sampler <- sampler
  class(s) <- c("VarFilterSampler", class(s))
  s
}



#' @export
sampl.VarFilterSampler <- function(s){
  # sample
  dat <- sampl(s$sampler)
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  kept.loci <- which(variance != 0.0)
  dat$G <- dat$G[,kept.loci]
  dat$V <- dat$V[kept.loci,]
  dat$B <- dat$B[,kept.loci]
  dat$outlier <- dat$outlier[dat$outlier %in% kept.loci]
  # return
  dat
}
