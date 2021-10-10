#' Gompertz mle function
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param fml the estimation formula
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#'
#' @return None
#'
#' @export


get.negLL <- function(par, y, X, y.left, y.right, wt = 1) {
  ## note exp(par) just gets them back to original scale
  beta <- exp(par[1])
  names(beta) <- "b"
  B <- par[2:length(par)] ## vector of parameters, original scale is

  ## in terms of log effect, so no
  ## transformation needed.
  log.A <- X %*% cbind(B) ## add up log effects
  A <- exp(log.A) ## transform to multiplicative effects
  M <- ab2M(a = A, b = beta) ## vector of M values

  num <- wt * dgompertz.M(y, b = beta, M = M) ## is use of wt correct???

  #  num <- wt * pgompertz.M(y+1, b = beta, M = M) - pgompertz.M(y, b = beta, M = M) ## is use of wt correct???

  num[num == 0] <- 10^-10 ## very low likelihood for 0 to avoid log(0)


  denom <- pgompertz.M(y.right, b = beta, M) - pgompertz.M(y.left, b = beta, M)
  denom[denom == 0] <- 10^-0 ## denom gets bigger value for zeros so
  ## num/denom likelihood is very small.

  LL <- sum(log(num) - log(denom))
  negLL <- -LL ## optim() minimizes so to maximize likelihood we
  ## return negative log-likelihood.
  return(negLL)
}
