#' Gompertz mle function
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param p the initialization function
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#'
#' @return None
#'
#'
#' @export


mll.gomp.multi.cohort.cov <- function(p, ## as many alphas as cohorts, and 1 beta
                                      A) ## data matrix with y, u, l, and covariates, including cohort
{
  ## (1) get parameters alpha, beta, and B
  ## we'll convert later to one element per individual
  cohorts <- names(table(A$cohort)) ## small vector of cohorts, e.g, 1915-1919.
  k <- length(cohorts)
  M <- exp(p[1:k])
  names(M) <- cohorts
  beta <- exp(p[k + 1])
  names(beta) <- "beta" ## slope of gompertz
  ## convert M to alpha
  alpha <- getAlpha(M, beta)
  names(alpha) <- cohorts

  ## new version just has one covariate
  ## e.g., educ in years

  b <- p[names(p) %in% predictors] ## covariate coef is last one

  ## (2) build rate.vec, which has the combined effect of cohort and covariates, 1 element per individual
  alpha.vec <- alpha[paste(A$cohort)] ## this now has one alpha for each individual
  ## note: effects are multiplicative of form haz_i = base * exp(covar_i * b)

  matrix <- A %>% select(predictors) %>% as.matrix()

  covar_effect.vec <-  exp(matrix %*% as.matrix(b))

  # covar_effect.vec <-  exp(A$educ_yrs * b[1] + A$urban * b[2])

  # covar_effect.vec <- exp(rowSums(mapply('*', A %>% select(all.vars(fml)[-1]), p.start[all.vars(fml)[-1]])))  ## this gives one covar_effect for each individual

  if (length(alpha.vec) != length(covar_effect.vec)) {
    print("warning: length(alpha.vec) != length(covar_effect.vec)")
  }
  rate.vec <- alpha.vec * (covar_effect.vec)

  y <- A$y
  u.vec <- A$u
  l.vec <- A$l

  ## (3) obtain likelihood Numerator has difference in cumulative
  ## probabilities, 1 year apart.  This works with data that is
  ## exact to the year of death like we have in CenSoc.
  num <- pgompertz(y + 1, shape = beta, rate = rate.vec) -
    pgompertz(y, shape = beta, rate = rate.vec)
  denom <- pgompertz(u.vec, shape = beta, rate = rate.vec) -
    pgompertz(l.vec, shape = beta, rate = rate.vec)
  R <- num / denom
  minusloglik <- -sum(log(R))
  return(minusloglik)
}
