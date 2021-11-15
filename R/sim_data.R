#' Simulated mortality data set
#'
#' A data set containing simulated age at death and covariates according to
#' a truncated Gompertz distribution with proportional hazards
#'
#' @format A data frame with 6732 rows and 6 variables:
#' \describe{
#'   \item{aod}{Age at death, in integer years}
#'   \item{byear}{Calender year of birth}
#'   \item{dyear}{Calender year of death}
#'   \item{temp}{Temperature}
#'   \item{sex}{Sex (0 = male, 1 = female)}
#'   \item{isSouth}{Live in south (0 = FALSE, 1 = TRUE)}
#'
#' }
"sim_data"
