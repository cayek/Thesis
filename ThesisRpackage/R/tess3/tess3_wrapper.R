################################################################################
# helpers

computePloidy <- function(G) {
  ploidy = max(G)
  assertthat::assert_that(ploidy > 0)
  assertthat::assert_that(ploidy / round(ploidy) == 1)
  ploidy
}


################################################################################
# tess3r

#' @export
tess3Method <- function(K,
                        sigma = NULL,
                        lambda = 1.0,
                        max.iteration = 200,
                        tolerance = 1e-05,
                        openMP.core.num = 1,
                        Q.init = NULL,
                        name = "tess3Method",
                        nickname = NULL) {
  m <- Method(name = name,
              nickname = nickname)
  class(m) <- c("tess3Method", class(m))
  m$K = K
  m$lambda = lambda
  m$sigma = sigma
  m$max.iteration = max.iteration
  m$tolerance = tolerance
  m$openMP.core.num = openMP.core.num
  m$Q.init = Q.init

  m
}


#' @export
fit.tess3Method <- function(m, dat) {

  assertthat::assert_that(!is.null(dat$coord))

  ## W
  if (is.null(m$sigma)) {
    W <- tess3r::ComputeHeatKernelWeight(dat$coord, tess3r.env$ComputeMeanDist(dat$coord) * 0.05)
  }

  ## ploidy
  ploidy <- computePloidy(dat$G)

  out <- capture.output(res <- tess3r::tess3(X = dat$G,
                                             coord = dat$coord,
                                             K = m$K,
                                             ploidy = ploidy,
                                             lambda = m$lambda,
                                             W = W,
                                             max.iteration = m$max.iteration,
                                             tolerance = m$tolerance,
                                             openMP.core.num = m$openMP.core.num,
                                             Q.init = m$Q.init,
                                             mask = 0))
  DebugMessage("tess3r", out)
  m[names(res)] <- res
  m
}


#' @export
plot.tess3Method <- function(m, dat) {

  ## barplot
 barplot(m$Q, border = NA, space = 0,
                         sort.by.Q = FALSE,
                         main = "Ancestry matrix",
                         xlab = "Individuals", ylab = "Ancestry proportions")

  ## map
  plot(m$Q, dat$coord, method = "map.max", interpol = tess3r::FieldsKrigModel(10),
                      background = FALSE,
                      main = "Ancestry coefficients",
                      xlab = "Longitude", ylab = "Latitude",
                      resolution = c(200,200), cex = .4)
}

################################################################################
# sNMF

#' @export
sNMFMethod <- function(K,
                        alpha = 10,
                        max.iteration = 200,
                        tolerance = 1e-05,
                        openMP.core.num = 1,
                        name = "sNMFMethod",
                        nickname = NULL) {
  m <- Method(name = name,
              nickname = nickname)
  class(m) <- c("sNMFMethod", class(m))
  m$K = K
  m$alpha = alpha
  m$max.iteration = max.iteration
  m$tolerance = tolerance
  m$openMP.core.num = openMP.core.num

  m
}


#' @export
fit.sNMFMethod <- function(m, dat) {

  if (is.null(dat$G.geno)) {
    file.geno <- paste0(tempfile(),".geno")
    LEA::write.geno(dat$G, file.geno)
  } else {
    file.geno <- dat$G.geno
  }

  ## ploidy
  ploidy <- computePloidy(dat$G)

  aux.f <- function() {
    LEA::snmf(input.file = file.geno,
              K = m$K,
              project = "new",
              repetitions = 1,
              alpha = m$alpha,
              tolerance = m$tolerance,
              entropy = FALSE,
              percentage = - 1.0,
              I = 0,
              iterations = m$max.iteration,
              ploidy = ploidy,
              seed = -1,
              CPU = m$openMP.core.num,
              Q.input.file = "")
  }

  out <- capture.output(aux <- aux.f())
  DebugMessage("sNMF", out)

  m$Q <- LEA::Q(aux, K = m$K, run = 1)
  class(m$Q) <- "tess3Q"
  m$G <- LEA::G(aux, K = m$K, run = 1)

  m

}


#' @export
plot.sNMFMethod <- function(m, dat) {
  plot.tess3Method(m, dat)
}

