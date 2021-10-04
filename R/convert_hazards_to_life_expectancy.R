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


convert_hazards_to_e65  <- function(df){


  df <- df %>%
    dplyr::filter(!stringr::str_detect(parameter, "mode.coh")) %>%
    dplyr::filter(!stringr::str_detect(parameter, "beta")) %>%
    dplyr::mutate(e65 = convert_hazard_ratio_to_le(hr = value, lower = 65, upper = 120, M = 80, beta = 0.08),
           e65_lower = convert_hazard_ratio_to_le(hr = lower, lower = 65, upper = 120, M = 80, beta = 0.08),
           e65_upper = convert_hazard_ratio_to_le(hr = upper, lower = 65, upper = 120, M = 80, beta = 0.08))

  return(df)

}

