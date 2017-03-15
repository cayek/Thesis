tess3r.env <- new.env()

#' Retrun tess3r env functions
#'
#' @export
get_tess3r <- function() {
  tess3r.env
}


tess3r.env$ComputeMeanDist <- function(coord) {
  W <- matrix(0,nrow(coord),nrow(coord))
  for (i in 1:nrow(coord)) {
    for (j in 1:nrow(coord)){
      W[i,j] <- sqrt(sum((coord[i,]-coord[j,])^2))
    }
  }
  return(mean(W))
}
