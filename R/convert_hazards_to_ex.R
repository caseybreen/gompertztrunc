#' Convert hazard ratios to life expectancy
#'
#' Convert hazard ratios to differences in remaining life expectancy
#' at a given age (defaults to age 65)
#'
#' @param df Dataframe of results given by gompertz_mle() function
#' @param age Age at which to calculate remaining life expectancy
#' @param upper_age Maximal age to use in life table calculation
#' @param M Gompertz parameter modal age at death
#' @param b Gompertz mortality slope parameter
#' @param use_model_estimates Use estimates of the Gompertz Parameters from the model, rather than defaults
#'
#' @return A dataframe of hazards ratios and corresponding e(x) estimates and confidence intervals
#'
#' @importFrom rlang := .data
#' @export
#'


convert_hazards_to_ex <- function(df, age = 65, upper_age = 120, M = 80, b = 0.075, use_model_estimates=FALSE) {

  # Use estimated mode and b from model, overriding any other input
  if(use_model_estimates) {
    M <-  (df %>% dplyr::filter(stringr::str_detect(.data$parameter, "gompertz_mode")))$coef
    b <-  (df %>% dplyr::filter(stringr::str_detect(.data$parameter, "gompertz_b")))$coef
  }

  # calculate life expectancy
  df <- df %>%
    dplyr::filter(!stringr::str_detect(.data$parameter, "gompertz_mode")) %>%
    dplyr::filter(!stringr::str_detect(.data$parameter, "gompertz_b")) %>%
    dplyr::mutate(
      "e{age}" := hazard_ratio_to_le(hr = .data$hr, lower = age, upper = upper_age, M, b),
      "e{age}_lower" := hazard_ratio_to_le(hr = .data$hr_upper, lower = age, upper = upper_age, M, b),
      "e{age}_upper" := hazard_ratio_to_le(hr = .data$hr_lower, lower = age, upper = upper_age, M, b)
    )

  return(df)
}
