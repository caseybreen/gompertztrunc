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


convert_hazard_ratio_to_le <- Vectorize(function(lower, upper, hr) {

  my.alpha <- getAlpha(M = 80, beta = .1)

  ## numident gradient
  le_estimate <- get.trunc.mean.gomp(my.alpha * hr, .1, 65, 100) -
    get.trunc.mean.gomp(my.alpha, .1, 65, 100)

  return(le_estimate)
})
