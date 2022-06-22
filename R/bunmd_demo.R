#' Demo BUNMD Data Set
#'
#' A data set containing a sample of the CenSoc Berkeley Unified Numident Mortality Database (BUNMD) file,
#' including age at death and select covariates.
#'
#
#' @format A data frame with 81,002 rows and 6 variables:
#' \describe{
#'   \item{ssn}{Social Security number}
#'   \item{bpl_string}{Country of birth}
#'   \item{death_age}{Age at death (integer years)}
#'   \item{byear}{Calendar year of birth}
#'   \item{dyear}{Calendar year of death}
#'   \item{age_first_application}{Age at first Social Security application}
#'
#' }
#'
#' @details
#'
#' The Berkeley Unified Numident Mortality Database (BUNMD) is a
#' cleaned and harmonized version of the NARA Numident file, consisting of the most
#' informative parts of the 60+ application, claim, and death files released by the
#' National Archives.The full data set of nearly 50 million records is available at
#' \link[https://censoc.berkeley.edu/data/]{https://censoc.berkeley.edu/data/}.
#'
#'
#' @source
#'
#' Joshua R. Goldstein, Monica Alexander, Casey Breen, Andrea Miranda González,
#' Felipe Menares, Maria Osborne, Mallika Snyder, and Ugur Yildirim.
#' CenSoc Mortality File: Version 2.0. Berkeley: University of California, 2021.
#' \link[https://censoc.berkeley.edu/]{https://censoc.berkeley.edu/}
#'
#'
"bunmd_demo"
