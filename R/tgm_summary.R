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
