#' Convert hazard ratios to differences in remaining life expectancy
#' at a given age (defaults to age 65)
#'
#' @param df dataframe of results given by gompertz_mle() function
#' @param age age at which to calculate remaining life expectancy
#' @param upper_age maximal age to use in life table calculation
#' @param M Gompertz parameter modal age at death
#' @param b Gompertz mortality slope parameter
#' @param use_model_estimates use estimates of the Gompertz Parameters from the model, rather than defaults
#'
#' @return A dataframe of hazards ratios and corresponding e(x) estimates and confidence intervals
#'
#'
#' @export
#'


convert_hazards_to_ex <- function(df, age = 65, upper_age = 120, M = 80, b = 0.075, use_model_estimates=F) {

  # Use estimated mode and b from model, overriding any other input
  if(use_model_estimates) {
    M <-  (df %>% dplyr::filter(stringr::str_detect(parameter, "gompertz_mode")))$coef
    # this will only work for a single mode
    b <-  (df %>% dplyr::filter(stringr::str_detect(parameter, "gompertz_b")))$coef
  }

  df <- df %>%
    dplyr::filter(!stringr::str_detect(parameter, "gompertz_mode")) %>%
    dplyr::filter(!stringr::str_detect(parameter, "gompertz_b")) %>%
    dplyr::mutate(
      e65 = hazard_ratio_to_le(hr = hr, lower, upper, M, b),
      e65_lower = hazard_ratio_to_le(hr = hr_upper, lower, upper, M, b),
      e65_upper = hazard_ratio_to_le(hr = hr_lower, lower, upper, M, b)
    )


  return(df)
}
