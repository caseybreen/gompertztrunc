#' Gompertz mle function
#'
#' Estimates seperate alpha values for each cohort
#'
#'
#' @param p the initialization function
#' @param A data matrix with covariates y, u, l, and covariates, including cohort
#'
#' @keywords internal
#'
#' @return None
#'


mll.gomp.multi.cohort.cov <- function(p, ## as many alphas as cohorts, and 1 beta
                                      A,
                                      predictors = predictors) ## data matrix with y, u, l, and covariates, including cohort
{

  ## (1) get parameters alpha, beta, and B
  ## we'll convert later to one element per individual
  cohorts <- names(table(A$cohort))
  k <- length(cohorts)
  M <- exp(p[1:k])
  names(M) <- cohorts
  beta <- exp(p[k + 1])
  names(beta) <- "beta" ## slope of gompertz
  ## convert M to alpha
  alpha <- getAlpha(M, beta)
  names(alpha) <- cohorts

  ## multiple covariates
  b <- p[names(p) %in% predictors]

  ## (2) build rate.vec, which has the combined effect of cohort and covariates, 1 element per individual
  alpha.vec <- alpha[paste(A$cohort)] ## this now has one alpha for each individual
  ## note: effects are multiplicative of form haz_i = base * exp(covar_i * b)

  matrix <- A %>%
    dplyr::select(dplyr::all_of(predictors)) %>%
    as.matrix()

  covar_effect.vec <- exp(matrix %*% as.matrix(b))

  if (length(alpha.vec) != length(covar_effect.vec)) {
    print("warning: length(alpha.vec) != length(covar_effect.vec)")
  }
  rate.vec <- alpha.vec * (covar_effect.vec)

  y <- A$y
  #  u.vec <- A$u - 1
  u.vec <- A$u
  l.vec <- A$l

  ## (3) obtain likelihood Numerator has difference in cumulative
  ## probabilities, 1 year apart.  This works with data that is
  ## exact to the year of death like we have in CenSoc.
  num <- flexsurv::pgompertz(y + 1, shape = beta, rate = rate.vec) -
    flexsurv::pgompertz(y, shape = beta, rate = rate.vec)
  denom <- flexsurv::pgompertz(u.vec, shape = beta, rate = rate.vec) -
    flexsurv::pgompertz(l.vec, shape = beta, rate = rate.vec)
  R <- num / denom
  minusloglik <- -sum(log(R))
  return(minusloglik)
}


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
  num[num == 0] <- 10^-10 ## very low likelihood for 0 to avoid log(0)
  denom <- pgompertz.M(y.right, b = beta, M) -
    pgompertz.M(y.left, b = beta, M)
  denom[denom == 0] <- 10^-0 ## denom gets bigger value for zeros so
  ## num/denom likelihood is very small.
  LL <- sum(log(num) - log(denom))
  negLL <- -LL ## optim() minimizes so to maximize likelihood we
  ## return negative log-likelihood.
  return(negLL)
}
