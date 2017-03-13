################################################################################
# With Likelyhood ratio

#' ChisqScore functor
#'
#' We assume that lr.score follow a chi2 with 1 degreee of freedom
#'
#'
#' @export
ChisqScore <- function() {
  Functor(fun = function(score) {
    score %>%
      apply(1:2,function(z) pchisq(z, df = 1, lower.tail = FALSE))
  },
  name = "ChisqScore")
}

#' Likehood ratio
#'
#'
#'
#'
#' @export
LikelyhoodRatio <- function(lr2pvalue.functor = ChisqScore()) {
  Functor(fun = function(m, dat) {
    res <- list()
    # compute the ratio
    res$score <- -2 * loglr(m, dat)
    # compute p.value
    res$pvalue <- lr2pvalue.functor$fun(res$score)
    res$minus.log.pvalue <- -log(res$pvalue)
    res
  },
  name = paste0("LikelyhoodRatio","|",name(lr2pvalue.functor)))
}
