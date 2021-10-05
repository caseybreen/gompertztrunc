#' Helper functions
#'
#'
#'

## parameterize gompertz with M (mode) and beta (slope)

getMode <- function(alpha, beta) {
  M <- -log(alpha/beta)/beta
  return(M)
}

getAlpha <- function(M, beta) {
  alpha = beta* exp(-beta * M)
  return(alpha)
}

dgomp_mode <- function(x, M, beta, ...) {
  alpha <- getAlpha(M, beta)
  dgompertz(x, shape = beta, rate = alpha, ...)
}

pgomp_mode <- function(q, M, beta, ...) {
  alpha <- getAlpha(M, beta)
  pgompertz(q, shape = beta, rate = alpha, ...)
}

rgomp_mode <- function(n, M, beta) {
  alpha <- getAlpha(M, beta)
  rgompertz(n, shape = beta, rate = alpha)
}

get.trunc.mean.gomp <- function(alpha, beta, l, u) {
  x <- 0:110
  hx <- alpha * exp(beta * x)
  Hx <- cumsum(hx)
  lx <- c(1, exp(-Hx))
  sum(lx)
  dx <- -diff(lx)
  s <- x %in% l:u ## approximate truncation for these cohorts
  sum((dx * x)[s]) / sum(dx[s])
}
