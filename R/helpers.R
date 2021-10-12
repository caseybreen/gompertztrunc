#' Helper functions
#'
#'
#'

## parameterize gompertz with M (mode) and beta (slope)

## we use flexsurv functions and modify just slightly

bM2a <- function(b, M) {
  a <- b * exp(-b * M)
  a
}
ab2M <- function(a, b) {
  M <- -log(a / b) / b
  M
}

library(flexsurv)

pgompertz.M <- function(q, b, M, ...) {
  a <- bM2a(b, M)
  flexsurv::pgompertz(q, shape = b, rate = a, ...)
}

dgompertz.M <- function(x, b, M, ...) {
  a <- bM2a(b, M)
  flexsurv::dgompertz(x, shape = b, rate = a, ...)
}

rgompertz.M <- function(n, b, M) {
  a <- bM2a(b, M)
  flexsurv::rgompertz(n, shape = b, rate = a)
}

hgompertz.M <- function(x, b, M) {
  a <- bM2a(b, M)
  q <- x
  p <- flexsurv::pgompertz(q, shape = b, rate = a)
  l <- 1 - p
  d <- flexsurv::dgompertz(x, shape = b, rate = a)
  h <- d / l
  return(h)
}



#########

## Old Gompertz Files


getMode <- function(alpha, beta) {
  M <- (1 / beta) * log(beta / alpha)
  return(M)
}

getAlpha <- function(M, beta) {
  alpha <- beta / exp(M * beta)
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


get.se <- function(est.vec, hess, log.vec = rep(TRUE, length(est.vec))) {
  fisher_info <- solve(hess)
  est.sd <- sqrt(diag(fisher_info))
  ## if estimates are not log scale then we have the SE
  ## if they are log scale than we can estimate SE
  ## as exp(beta)*SD
  est.se <- exp(est.vec)^(log.vec) * est.sd
  return(est.se)
}

tgm_summary <- function(fit, true.coef.values = NULL,
                        a0 = 10^-4, beta = 1 / 10) {
  ## return parameters with 95% confidence intervals and, if using
  ## simulated data, also the original parameter values for comparison
  nobs <- fit$nobs
  par <- fit$par
  hess <- fit$hess
  se.vec <- get.se(par, hess)
  if (!is.null(true.coef.values)) {
    out <- cbind(
      "est" = fit$par,
      "lower" = fit$par - 2 * se.vec,
      "upper" = fit$par + 2 * se.vec,
      "true" = c(
        "log(beta)" = log(beta),
        "log(a0)" = log(a0), true.coef.values
      )
    )
  }
  if (is.null(true.coef.values)) {
    out <- cbind(
      "est" = fit$par,
      "lower" = fit$par - 2 * se.vec,
      "upper" = fit$par + 2 * se.vec,
      "true" = true.coef.values
    )
  }
  print(paste("nobs:", nobs))
  print(out)
  return(out)
}
