#' Gompertz Negative Log Likelihood Function
#'
#' Computes negative log likelihood for optimizer
#'
#' @param par a vector of parameter estimates
#' @param y a vector of death ages
#' @param X a model matrix
#' @param y.left left truncation age
#' @param y.right right trunction age
#' @param wt weight
#'
#' @return The negative log likelihood of parameter estimates given observed data
#'

negLL_function <- function(par, y, X, y.left, y.right, wt) {

  ## note exp(par) just gets them back to original scale
  b <- exp(par[1])
  names(b) <- "b"
  Beta <- par[2:length(par)] ## vector of parameters, original scale is

  ## in terms of log effect, so no
  ## transformation needed.
  log.A <- X %*% cbind(Beta) ## add up log effects
  A <- exp(log.A) ## transform to multiplicative effects
  M <- ab2M(a = A, b = b) ## vector of M values

  num <- dgompertz.M(y, b = b, M = M)
  #num <- wt * pgompertz.M(y+1, b = b, M = M) - pgompertz.M(y, b = b, M = M)

  num[num == 0] <- 10^-10 ## very low likelihood for 0 to avoid log(0)

  denom <- pgompertz.M(y.right, b = b, M) - pgompertz.M(y.left, b = b, M)
  denom[denom == 0] <- 10^-10 ## denom gets bigger value for zeros so
  ## num/denom likelihood is very small.

  LL <- sum(wt * (log(num) - log(denom))) ## use of weight correct??
  negLL <- -LL ## optim() minimizes so to maximize likelihood we
  ## return negative log-likelihood.
  return(negLL)
}
