#' Translate a single hazard ratio to life expectancy
#'
#'
#' @param lower lower age bound
#' @param upper upper age bound
#' @param hr hazard ratio
#' @param M Gompertz modal age at death parameter
#' @param b Gompertz mortality slope parameter
#'
#' @return hazard ratio converted to effect on e65
#'
#'
#' @export
#'



hazard_ratio_to_le <- Vectorize(function(lower, upper, hr, M = 80, b = 0.1) {

  ## convert alpha
  my.alpha <- bM2a(M = M, b=b)

  ## numident gradient
  le_estimate <- get.trunc.mean.gomp(my.alpha * hr, b, lower, upper) -
    get.trunc.mean.gomp(my.alpha, b, lower, upper)

  return(le_estimate)
})
