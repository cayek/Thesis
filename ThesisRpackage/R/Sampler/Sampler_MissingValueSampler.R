################################################################################
# Uniform missing value

#' Sample missing values
#'
#'
#' @export
MissingValueSampler <- function(sampler, missing.prop) {
  s <- Sampler()
  s$sampler <- sampler
  s$missing.prop <- missing.prop
  class(s) <- c("MissingValueSampler", class(s))
  s
}



#' @export
sampl.MissingValueSampler <- function(s){
  dat <- sampl(s$sampler)
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  dat$missing.index <- sample.int(n * L, size = n * L * s$missing.prop, replace = FALSE)
  dat$missing.value <- dat$G[dat$missing.index]
  dat$G[dat$missing.index] <- NA
  dat
}

################################################################################
# Sparse missing value


#' Sample missing values only on some loci
#'
#'
#' @export
SparseMissingValueSampler <- function(sampler, missing.prop, missing.prop.by.loci) {
  s <- Sampler()
  s$sampler <- sampler
  s$missing.prop <- missing.prop
  s$missing.prop.by.loci <- missing.prop.by.loci
  class(s) <- c("SparseMissingValueSampler", class(s))
  s
}



#' @export
sampl.SparseMissingValueSampler <- function(s){
  dat <- sampl(s$sampler)
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  nb.missing.loci <- s$missing.prop * L / s$missing.prop.by.loci
  dat$missing.loci <- sample.int(L, size = nb.missing.loci)
  dat$missing.ind <- sample.int(n, size = n * s$missing.prop.by.loci, replace = FALSE)
  dat$missing.value <- dat$G[dat$missing.ind, dat$missing.loci]
  dat$G[dat$missing.ind, dat$missing.loci] <- NA
  dat
}
