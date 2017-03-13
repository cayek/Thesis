#' Make a grid of methods with param
#'
#'  @export
paramGrid <- function(method.constructor, nickname.base, ...) {
  params <- list(...)

  # expand
  params <- base::expand.grid(params)

  # make nickname
  names <- colnames(params)
  aux.f <- function(r) {
    nickname <- nickname.base
    for (i in seq_along(r)) {
      nickname <- paste0(nickname,"|", names[i], "=", r[i])
    }
    nickname
  }
  nickname <- apply(params, 1, aux.f)
  params <- params %>% dplyr::mutate(nickname = nickname)

  res <- params %>% purrr::invoke_rows(.f = method.constructor)
  res$.out
}
