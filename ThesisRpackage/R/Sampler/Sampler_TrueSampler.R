################################################################################
# TrueSampler


#' sample true data from file
#'
#' @export
##' @param G.file a file or a matrix
##' @param X.file a file or a matrix
##' @param outlier.file a file or a matrix
##' @param n number of indiv
##' @param L number of locus
##' @param reference if TRUE the sampler retrun R5 object
TrueSampler <- function(G.file, X.file, outlier.file,
                        ind.clumping = NULL,
                        n = NULL, L = NULL,
                        reference = FALSE) {

  structure(list(n = n,
                 L = L,
                 G.file = G.file,
                 X.file = X.file,
                 ind.clumping = ind.clumping,
                 reference = reference,
                 outlier.file = outlier.file), class = c("TrueSampler","Sampler"))
}


#' sample true data from file
#'
#'
#' @export
sampl.TrueSampler <- function(s) {

  # read G
  if (is.character(s$G.file)) {
   G <- read_G(s$G.file)
  } else if (is.matrix(s$G.file)) {
    G <- s$G.file
  } else {
    G <- matrix(NA,1,1)
  }

  # read X
  if (is.character(s$X.file)) {
    X <- read_X(s$X.file)
  } else if (is.matrix(s$X.file)) {
    X <- s$X.file
  } else {
    X <- matrix(NA, nrow(G), 1)
  }

  ## read outlier
if (is.character(s$outlier.file)) {
    outlier <- read_all(s$outlier.file)
  } else if (is.numeric(s$outlier.file)) {
    outlier <- s$outlier.file
  } else {
    outlier <- c()
  }
  
  ## read ind.clumping
  if (is.character(s$ind.clumping)) {
    ind.clumping <- read_all(s$ind.clumping)
  } else if (is.numeric(s$ind.clumping)) {
    ind.clumping <- s$ind.clumping
  } else {
    ind.clumping <- NULL
  }


  n <- nrow(G)
  L <- ncol(G)

  # sample if required
  if (!is.null(s$n)) {
    indiv.sample <- sample.int(n, size = s$n)
    X <- X[indiv.sample,,drop = FALSE]
    G <- G[indiv.sample,,drop = FALSE]
    outlier <- indiv.sample[indiv.sample %in% outlier]
    n <- s$n
  }

  if (!is.null(s$L)) {
    indiv.loci <- sample.int(L, size = s$L)
    G <- G[,indiv.loci,drop = FALSE]
    L <- s$L
  }


  TrueDataSet(G = G,
              X = X,
              outlier = outlier,
              ind.clumping = ind.clumping,
              reference = s$reference)
}
