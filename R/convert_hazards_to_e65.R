#' Convert hazard ratios to life expectancy at age 65
#'
#'
#' @param df dataframe of results given by gompertz_mle() function
#' @param lower lower age bound
#' @param upper upper age bound
#' @param M Gompertz parameter modal age at death
#' @param beta Gompertz mortality parameter
#' @param use_model_estimates use estimates of the Gompertz Parameters from the model, rather than defaults
#'
#' @return A dataframe of hazards ratios and corresponding e65 estimates and confidence intervals
#'
#'
#' @export
#'


convert_hazards_to_e65 <- function(df, lower = 65, upper = 120, M = 80, beta = 0.075, use_model_estimates=F) {

  # Use estimated mode and beta from model, overriding any other input
  if(use_model_estimates) {
    M <-  (df %>% dplyr::filter(stringr::str_detect(parameter, "mode")))$coef
    # this will only work for a single mode
    beta <-  (df %>% dplyr::filter(stringr::str_detect(parameter, "beta")))$coef
  }

  df <- df %>%
    dplyr::filter(!stringr::str_detect(parameter, "mode")) %>%
    dplyr::filter(!stringr::str_detect(parameter, "beta")) %>%
    dplyr::mutate(
      e65 = hazard_ratio_to_le(hr = hr, lower, upper, M, beta),
      e65_lower = hazard_ratio_to_le(hr = hr_upper, lower, upper, M, beta),
      e65_upper = hazard_ratio_to_le(hr = hr_lower, lower, upper, M, beta)
    )


  return(df)
}
