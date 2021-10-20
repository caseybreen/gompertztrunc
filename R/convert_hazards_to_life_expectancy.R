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
#'


convert_hazards_to_e65 <- function(df, lower = 65, upper = 120, M = 80, beta = 0.075, use_model_estimates=F) {

  # Use estimated mode and beta from model, overriding any other input
  if(use_model_estimates) {
    M <-  (df %>% dplyr::filter(stringr::str_detect(parameter, "mode")))$coef
    beta <-  (df %>% dplyr::filter(stringr::str_detect(parameter, "beta")))$coef
  }

  df <- df %>%
    dplyr::filter(!stringr::str_detect(parameter, "mode")) %>%
    dplyr::filter(!stringr::str_detect(parameter, "beta")) %>%
    dplyr::mutate(
      e65 = convert_hazard_ratio_to_le(hr = hr, lower, upper, M, beta),
      e65_lower = convert_hazard_ratio_to_le(hr = hr_upper, lower, upper, M, beta),
      e65_upper = convert_hazard_ratio_to_le(hr = hr_lower, lower, upper, M, beta)
    )


  return(df)
}
