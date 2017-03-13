################################################################################
# ggplot funcion

gplot_VB <- function(V, B, outlier) {
  K <- ncol(V)
  L <- nrow(V)
  d <- nrow(B)
  toplot <- as_tibble(V)
  names(toplot) <- sapply(1:K,function(i) paste0("V",i))
  aux <- t(B)
  colnames(aux) <- sapply(1:d,function(i) paste0("B",i))
  toplot <- cbind(toplot, as_tibble(aux)) %>%
    mutate(index = 1:L,
           outlier = 1:L %in% outlier) %>%
    gather("factor", "size.effect", 1:(K + d))
  ggplot(toplot, aes(x = index, y = size.effect, color = outlier)) +
    geom_point() +
    facet_grid(. ~ factor, scales = "free")
}


gplot_UX <- function(U, X) {
  K <- ncol(U)
  n <- nrow(U)
  d <- ncol(X)
  toplot <- as_tibble(U)
  names(toplot) <- sapply(1:K,function(i) paste0("U",i))
  aux <- X
  colnames(aux) <- sapply(1:d,function(i) paste0("X",i))
  toplot <- cbind(toplot, as_tibble(aux)) %>%
    mutate(index = 1:n) %>%
    gather("factor", "size.effect", 1:(K + d))
  ggplot(toplot, aes(x = index, y = size.effect, color = "red")) +
    geom_point() +
    facet_grid(. ~ factor, scales = "free")
}

gplot_manhattan <- function(pvalue, outlier) {
  toplot <- tibble(index = 1:length(pvalue),
                   pvalue = pvalue,
                   outlier = 1:length(pvalue) %in% outlier)
  ggplot(toplot, aes(x = index, y = -log(pvalue), color = outlier)) +
    geom_point()
}

gplot_hist <- function(stat, outlier, stat.name = "pvalue") {
  toplot <- tibble(index = 1:length(stat),
                   stat = stat,
                   outlier = 1:length(stat) %in% outlier)
  ggplot(toplot, aes(stat, fill = outlier)) +
    geom_histogram(aes(y = ..density..)) +
    ggplot2::ggtitle(stat.name)
}

gplot_B <- function(B, outlier) {
  toplot <- tibble(index = 1:length(B),
                   pvalue = B,
                   outlier = 1:length(B) %in% outlier)
  ggplot(toplot, aes(x = index, y = B, color = outlier)) +
    geom_point()
}

gplot_zcsore <- function(zscore, outlier) {
  toplot <- tibble(index = 1:length(zscore),
                   pvalue = zscore,
                   outlier = 1:length(zscore) %in% outlier)
  ggplot(toplot, aes(x = index, y = B, color = outlier)) +
    geom_point()
}

#' Plot stat
#'
#'
#' @examples
#' library(Article3Package)
#'
#' K <- 5
#' s <- NormalSampler(100, 1000, K,
#'                    prop.outlier = 0.02,
#'                    c = 0.8) %>%
#'   MissingValueSampler(missing.prop = 0.0)
#'
#' m <- ClassicLinearMethod()
#' dat <- sampl(s)
#' m <- run(m, dat)
#' gplot_stat(m$score[1,] outlier = dat$outlier) +
#'  geom_point(aes(x = index, y = stat, color = outlier)) +
#'
#' @export
gplot_stat <- function(...,outlier) {

  toplot <- tibble(...)
  toplot <- toplot %>%
    mutate(index = 1:nrow(toplot),
           outlier = index %in% outlier) %>%
    gather(key = stat.name, value = stat, -index, -outlier)
  ggplot(toplot) +
    facet_grid(stat.name ~ ., scales = "free")
}

################################################################################
# plot lattent factor and co variable

#' Print the correlation cor(A[,i], X)
#'
#' @export
print_cor <- function(A, X) {
  for (i in 1:ncol(A)) {
    cat(paste0("cor(A[,",i,"], X) = ",
               cor(A[,i], X),"\n"))
  }
}



#' Represention confunding between estimated lattent factor and X variable
#'
#'
#' @param dat data object with outlier and X
#' @param m method object with U V and B
#' @param i x
#' @param j y
#'
#' @examples
#' library(Article3Package)
#'
#' K <- 5
#' s <- NormalSampler(100, 1000, K,
#'                    prop.outlier = 0.02,
#'                    c = 0.8) %>%
#'   MissingValueSampler(missing.prop = 0.0)
#'
#' m <- ClassicLinearMethod()
#' dat <- sampl(s)
#' m <- run(m, dat)
#'
#' gplot_confunding(dat, m, i = 1, j = 2, d = 1)
#'
#'
#' @export
gplot_confunding <- function(dat, m, i, j, d) {

  L <- ncol(dat$G)

  # U X plot
  if (!is.null(m$U)) {
    toplot <- tibble(Ui = m$U[,i],
                     Uj = m$U[,j],
                     Xd = dat$X[,d])
  } else {
    toplot <- tibble(Ui = dat$U[,i],
                     Uj = dat$U[,j],
                     Xd = dat$X[,d])
  }

  corUX <- c(cor(toplot$Ui, toplot$Xd), cor(toplot$Uj, toplot$Xd))
  cat(paste0("corUX = (", corUX[1],",",corUX[2],")", "\n"))
  ggux <- ggplot(toplot, aes(x = Ui, y = Uj, color = Xd, size = Xd)) +
    geom_point() +
    geom_point(aes(x = corUX[1], y = corUX[2]),
              color = "green",
              size = 4,
              shape = 8)
  print(ggux)

  # V B plot
  if (!is.null(m$V)) {
    toplot <- tibble(Vi = m$V[,i],
                     Vj = m$V[,j],
                     Bd = m$B[d,],
                     outlier = 1:L %in% dat$outlier)
  } else {
    toplot <- tibble(Vi = dat$V[,i],
                     Vj = dat$V[,j],
                     Bd = m$B[d,],
                     outlier = 1:L %in% dat$outlier)
  }

  corVB <- c(cor(toplot$Vi, toplot$Bd), cor(toplot$Vj, toplot$Bd))
  cat(paste0("corVB = (", corVB[1],",",corVB[2],")", "\n"))
  ggvb <- ggplot(toplot, aes(x = Vi, y = Vj, color = Bd, size = Bd)) +
    geom_point() +
    geom_point(aes(x = corUX[1], y = corUX[2]),
               color = "green",
               size = 4,
               shape = 8) +
    geom_point(data = toplot %>%
                 dplyr::filter(outlier == TRUE),
               aes(x = Vi, y = Vj),
               color = "red",
               size = 4,
               shape = 3)
  print(ggvb)

}





