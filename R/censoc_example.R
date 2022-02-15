#' Example CenSoc Data Set
#'
#' A data set containing age at death and select covariates
#' for a sample for 100,000 men from the cohort of 1910. Data are drawn from a
#' linked sample of the CenSoc-DMF file and the 1940 Census.
#'
#' @format A data frame with 100,000 rows and 6 variables:
#' \describe{
#'   \item{id}{Individual-level identifier}
#'   \item{byear}{Calender year of birth}
#'   \item{death_age}{Age at death (integer years)}
#'   \item{educ_yrs}{Years of education completed}
#'   \item{urban}{Resided in urban area in 1940 (0 = FALSE, 1 = TRUE)}
#'   \item{ownhome}{Lived in owned housing unit in 1940 (0 = FALSE, 1 = TRUE)}
#' }
#'
#'
"censoc_example"
