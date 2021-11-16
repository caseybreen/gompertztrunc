#' Gompertz MLE function
#'
#' Fits a truncated Gompertz distribution with proportional hazards
#' to mortality data using maximum likelihood estimation.
#'
#'
#' @param fml the estimation formula
#' @param left_truc left truncation year
#' @param right_trunc right truncation year
#' @param byear year of birth
#' @param lower_bound lowest age at death (optional)
#' @param upper_bound highest age at death (optional)
#' @param maxiter maximum number of iterations for optimizer
#'
#' @return Returns a named list consisting of the following components:
#' \describe{
#'   \item{\code{starting_values}}{list of starting values of paramters to be estimated}
#'   \item{\code{optim_fit}}{list consisting of:
#'   \describe{
#'     \item{\code{par}}{parameter initialization}
#'     \item{\code{value}}{don't know what this is}
#'     \item{\code{counts}}{function and gradient}
#'     \item{\code{convergence}}{returns 0 if the model converged}
#'     \item{\code{message}}{idk}
#'     \item{\code{hessian}}{the Hessian matrix}
#'     }
#'   }
#'   \item{results}{a table of estimates and upper/lower bounds}

#' }
#'
#' @export gompertz_mle

gompertz_mle <- function(fml, right_trunc = 2005, left_trunc = 1975, data, byear = byear, lower_bound = NULL, upper_bound = NULL, maxiter = 10000) {

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

  ## get starting parameters
  p.start <- get.par.start(fml, data_formatted)

  model_matrix <- modelr::model_matrix(formula = fml, data = data_formatted) %>%
    as.matrix()

  ## create controls
  my.control <- list(
    trace = 0,
    parscale = c(par.scale = p.start),
    maxit = maxiter
  )

  ## get optimization function
  optim_fit <- optim(
    par = p.start,
    fn = negLL_function,
    hessian = TRUE,
    y = data_formatted$y,
    X = model_matrix,
    wt = wt,
    y.left = data_formatted$left_trunc_age,
    y.right = data_formatted$right_trunc_age,
    control = my.control
  )

  fit <- optim_fit

  ## tidy up optim output
  fit <- broom::tidy(fit) %>%
    dplyr::mutate(
      lower = value - 1.96 * std.error,
      upper = value + 1.96 * std.error
    )

  ## calculate mode
  mode <- fit %>%
    dplyr::filter(parameter == "b0.start") %>%
    dplyr::mutate(parameter = "gompertz_mode",
           value = ab2M(exp(value), b = exp(fit$value[1])),
           lower = ab2M(exp(lower), b = exp(fit$value[1])),
           upper = ab2M(exp(upper), b = exp(fit$value[1]))) %>%
    dplyr::rename(upper = lower, lower = upper)

  ## calculate beta
  beta <- fit %>%
    dplyr::filter(parameter == "log.b.start") %>%
    dplyr::mutate(parameter = "gompertz_beta",
           value = exp(value),
           lower = exp(lower),
           upper = exp(upper))

  ## calculate mode
  fit <- dplyr::bind_rows(beta, mode) %>%
    dplyr::bind_rows(fit %>%
                dplyr::filter(!parameter %in% c("b0.start", "log.b.start")))

  ## tidy up fit object
  fit <- fit %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "log.", "")) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "of.", "")) %>%
    dplyr::select(-std.error) %>%
    dplyr::mutate(hr = dplyr::if_else(parameter %in% c("gompertz_mode", "gompertz_beta"), NA_real_, exp(value)),
           hr_lower = dplyr::if_else(parameter %in% c("gompertz_mode", "gompertz_beta"), NA_real_, exp(lower)),
           hr_upper = dplyr::if_else(parameter %in% c("gompertz_mode", "gompertz_beta"), NA_real_, exp(upper)))

  results <- fit %>%
    dplyr::select(parameter, coef = value, coef_lower = lower, coef_upper = upper, hr, hr_lower, hr_upper)

  ## return all results as a list
  gompertz_trunc <- list("starting_values" = p.start, "optim_fit" = optim_fit, "results" = results)

  return(gompertz_trunc)
}
