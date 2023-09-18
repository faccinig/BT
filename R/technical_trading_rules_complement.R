#' Compute lagged values
#'
#' @param x a vector
#' @param n positive integer giving the number os positions to lag by
#'
#' @return a vetor with the same length as x.
#' @export
#'
#' @examples
#'
#' lag(1:3)
#' lag(1:3, n = 2)
#'
lag <- function(x, n = 1L) {
  # Naive implementation
  # talvez o melhor seria importar a versão do dplyr ou reescrever em cpp
  if (n != round(n)) {
    n <- round(n)
    warning("'n' is not an integer")
  }
  NAs <- rep(NA, n)
  x[c(NAs, seq_len(length(x)-n))]
}


#' Test if first line are crossover the second
#'
#' @param vec1 a numeric vector
#' @param vec2 a numeric vector with the same length as the first
#'
#' @return a boolean vector with the same length as vec1 where `TRUE` indicate when `vec1` crossover `vec2`
#' @export
#'
#' @examples
#'
#' crossover(c(1, 2), c(1.5, 1.5))
#' crossover(c(1, 2), c(NA, 1.5)) # NAs became FALSE
#'
#'
crossover <- function(vec1, vec2) {
  gt <- vec1 > vec2
  lt <- lag(vec1 < vec2)
  ifelse(is.na(lt), FALSE, gt & lt)
}
