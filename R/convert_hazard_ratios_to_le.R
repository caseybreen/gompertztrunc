#' Translate to e65
#'
#'
#' @param lower the bound function
#' @param upper the initialization function
#' @param b the hazard ratio
#'
#' @return None
#'
#'
#' @export


convert_hazard_ratio_to_le <- Vectorize(function(lower, upper, hr, M = 80, beta = 0.1) {

  ## convert alpha
  my.alpha <- getAlpha(M = M, beta = beta)

  ## numident gradient
  le_estimate <- get.trunc.mean.gomp(my.alpha * hr, beta, lower, upper) -
    get.trunc.mean.gomp(my.alpha, beta, lower, upper)

  return(le_estimate)
})
