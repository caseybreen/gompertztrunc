#' Demo Numident dataset
#'
#' A data set containing a sample of the CenSoc-Numident file,
#' including age at death and select covariates.
#'
#
#' @format A data frame with 62,899 rows and 30 variables:
#' \describe{
#' \item{histid}{Historical unique identifier}
#' \item{byear}{Year of birth}
#' \item{bmonth}{Month of birth}
#' \item{dyear}{Year of death}
#' \item{dmonth}{Month of death}
#' \item{death_age}{Age at death (years)}
#' \item{weight}{CenSoc weight}
#' \item{zip_residence}{ZIP Code of residence at time of death}
#' \item{pernum}{Person number in sample unit}
#' \item{perwt}{IPUMS person weight}
#' \item{age}{Age in 1940}
#' \item{sex}{Sex in 1940}
#' \item{bpl}{Place of birth}
#' \item{mbpl}{Mother’s place of birth}
#' \item{fbpl}{Father’s place of birth}
#' \item{educd}{Educational attainment (detailed)}
#' \item{empstatd}{Employment status (detailed)}
#' \item{hispan}{Hispanic/Spanish/Latino origin}
#' \item{incnonwg}{Had non-wage/salary income over $50}
#' \item{incwage}{Wage and salary income}
#' \item{marst}{Marital status}
#' \item{nativity}{Foreign birthplace or parentage}
#' \item{occ}{Occupation}
#' \item{occscore}{Occupational income score}
#' \item{ownershp}{Ownership of dwelling (tenure)}
#' \item{race}{Race}
#' \item{rent}{Monthly contract rent}
#' \item{serial}{Household serial number}
#' \item{statefip}{State of residence 1940}
#' \item{urban}{Urban/rural status}
#' \item{educ_yrs}{Years of education attained}
#'
#' }
#'
#' @details
#'
#' The CenSoc-Numident dataset links the 1940 census to the National Archives’
#' public release of the Social Security Numident file. The prelinked “demo”
#' version of the file has 63 thousand mortality records and 20
#' mortality covariates from the 1940 census (~1\% of the complete CenSoc-Numident dataset).It
#' is availible at \link[https://censoc.berkeley.edu/data/]{https://censoc.berkeley.edu/data/}.
#'
#'
#' @source
#'
#' Joshua R. Goldstein, Monica Alexander, Casey Breen, Andrea Miranda González,
#' Felipe Menares, Maria Osborne, Mallika Snyder, and Ugur Yildirim.
#' CenSoc Mortality File: Version 2.0. Berkeley: University of California, 2021.
#' \link[https://censoc.berkeley.edu/]{https://censoc.berkeley.edu/}
#'
#' Steven Ruggles, Sarah Flood, Ronald Goeken, Megan Schouweiler and Matthew Sobek.
#' IPUMS USA: Version 12.0 [dataset]. Minneapolis, MN: IPUMS, 2022.
#' \link[https://doi.org/10.18128/D010.V12.0]{https://doi.org/10.18128/D010.V12.0}
#'
"numident_demo"
