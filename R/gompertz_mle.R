#' Gompertz mle function
#'
#'
#' @param fml the estimation formula
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#'
#' @return fit
#'
#' @export gompertz_mle

gompertz_mle <- function(fml, right_trunc = 2005, left_trunc = 1975, data, byear = byear, lower_bound = NULL, upper_bound = NULL) {

  ## format data
  data_formatted <- data %>%
    dplyr::mutate(byear = as.numeric(as.character(byear))) %>%
    dplyr::select(all.vars(fml), byear) %>%
    dplyr::mutate(
      y = get(all.vars(fml)[1]),
      right_trunc_age = right_trunc - byear,
      left_trunc_age = left_trunc - byear,
      cohort = byear
    )

  ## lower bound (e.g., only observe deaths over 65)
  if (!missing(lower_bound)) {
    data_formatted <- data_formatted %>%
      dplyr::mutate(left_trunc_age = case_when(
        left_trunc_age < lower_bound ~ lower_bound,
        TRUE ~ left_trunc_age
      ))
  }

  ## upper bound (e.g., only observe deaths over 65)
  if (!missing(upper_bound)) {
    data_formatted <- data_formatted %>%
      dplyr::mutate(right_trunc_age = case_when(
        right_trunc_age > upper_bound ~ upper_bound,
        TRUE ~ right_trunc_age
      ))
  }

  ## convert data to integers
  if (sum(data_formatted$y - floor(data_formatted$y)) == 0){

    data_formatted <- data_formatted %>%
      dplyr::mutate(y = y + 0.5)

  }

  ## remove endpoints
  data_formatted <- data_formatted %>%
    dplyr::filter(y > left_trunc_age & y < right_trunc_age)

  ## add weights

  if (!is.null(data_formatted$wt)) {
    wt <- wt
  }
  if (is.null(data_formatted$wt)) {
    wt <- 1.0
  }

  ## get unique cohorts
  cohorts <- sort(unique(data_formatted$cohort))

  ## starting values, just let all the modes be the same ("80")
  M.start <- rep(80, length(cohorts))
  names(M.start) <- paste0("coh", cohorts)

  ## get starting parameters
  p.start <- get.par.start(fml, data_formatted)

  model_matrix <- modelr::model_matrix(formula = fml, data = data_formatted) %>%
    as.matrix()

  ## create controls
  my.control <- list(
    trace = 0,
    parscale = c(par.scale = p.start),
    maxit = 5000
  )

  fit <- optim(
    par = p.start,
    fn = get.negLL,
    hessian = TRUE,
    y = data_formatted$y,
    X = model_matrix,
    wt = wt,
    y.left = data_formatted$left_trunc_age,
    y.right = data_formatted$right_trunc_age,
    control = my.control
  )


  ## tidy up optim output
  fit <- broom::tidy(fit) %>%
    dplyr::mutate(
      lower = value - 1.96 * std.error,
      upper = value + 1.96 * std.error
    ) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "log.", "")) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "of.", "")) %>%
    dplyr::select(-std.error) %>%
    mutate(hr = exp(value),
           hr_lower = exp(lower),
           hr_upper = exp(upper))

  # out <- out %>%
  #   dplyr::mutate(across(where(is.numeric), exp))

  return(fit)
}
