#' @export
FDRControl_qvalue <- function(pvalue) {
  qvalue::qvalue(pvalue)$qvalues
}

#' @export
FDRControl_BH <- function(pvalue) {
  p.adjust(pvalue, method = "BH")
}

#' compute expeted true FDR and power
#'
#' assumig we know m0 the number not outlier. We use
#' P(H0|positif) = P(positif | H0) * P(H0) / P(positif)
#'
#' @param pvalue a vector of pvalue for each snips
#' @param outlier a vector of outlier snps index
#'
#' @export
ExpectedFDR_trueFDR_Power <- function(pvalue, outlier) {
  L <- length(pvalue)
  m1 <- length(outlier)
  m0 <- L - m1
  res <- tibble(pvalue = as.numeric(pvalue))
  res <- res %>% mutate(index = 1:nrow(res),
                        outlier = index %in% outlier)
  res <- res %>% arrange(pvalue)
  res <- res %>%
    mutate(expected.fdr = pvalue * m0 / L,
           expected.fd = pvalue * m0,
           true.fdr = cumsum(!outlier) / 1:L,
           true.fd = cumsum(!outlier),
           true.power = cumsum(outlier) / m1)
  res <- res %>%
    arrange(index)
  res
}

#' Retrun a tidy data frame with fdr power ect
#'
#' @export
tidy_fdr <- function(pvalue, outlier) {
  res <- tibble()
  if (!is.null(pvalue)) {
    for (d in 1:nrow(pvalue)) {
      res <- rbind(res,
                   ExpectedFDR_trueFDR_Power(as.numeric(pvalue[d,]), outlier) %>%
                     mutate(pvalue.index = paste0("pvalue",d)))
    }
    res
  } else {
    tibble()
  }
}
