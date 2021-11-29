#' Translate a single hazard ratio to life expectancy
#'
#'
#' @param lower lower age bound
#' @param upper upper age bound
#' @param hr hazard ratio
#' @param M Gompertz parameter modal age at death
#' @param beta Gompertz mortality parameter
#'
#' @return hazard ratio converted to effect on e65
#'
#'
#' @export
#'



hazard_ratio_to_le <- Vectorize(function(lower, upper, hr, M = 80, beta = 0.1) {

  ## convert alpha
  my.alpha <- getAlpha(M = M, beta = beta)

  ## numident gradient
  le_estimate <- get.trunc.mean.gomp(my.alpha * hr, beta, lower, upper) -
    get.trunc.mean.gomp(my.alpha, beta, lower, upper)

  return(le_estimate)
})
