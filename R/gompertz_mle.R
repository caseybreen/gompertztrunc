#' Gompertz MLE function
#'
#' Fits a truncated Gompertz distribution with proportional hazards
#' to mortality data using maximum likelihood estimation.
#'
#'
#' @param formula the estimation formula
#' @param left_trunc left truncation year
#' @param right_trunc right truncation year
#' @param data a data frame containing variables in the model
#' @param byear name of birth year variable in \code{data}
#' @param dyear name of death year variable in \code{data}
#' @param lower_age_bound  lowest age at death to include (optional)
#' @param upper_age_bound highest age at death to include (optional)
#' @param weights an optional vector of individual weights
#' @param death_age_data_type option for handling of continuous and discrete death age variable (not yet implemented)
#' @param maxiter maximum number of iterations for optimizer
#'
#'
#' @return Returns a named list consisting of the following components
#' (See \code{stats::\link[stats:optim]{optim}} for additional details):
#' \describe{
#'   \item{\code{starting_values}}{list of starting values of parameters}
#'   \item{\code{optim_fit}}{A list consisting of:
#'   \describe{
#'     \item{\code{par}}{best estimation of parameter values}
#'     \item{\code{value}}{log likelihood}
#'     \item{\code{counts}}{number of calls to function and gradient}
#'     \item{\code{convergence}}{returns 0 if the model converged, for other values see \code{stats::\link[stats:optim]{optim}} }
#'     \item{\code{message}}{any other information returned by optimizer}
#'     \item{\code{hessian}}{Hessian matrix}
#'     }
#'   }
#'   \item{\code{results}}{a table of estimates and upper/lower bounds}

#' }
#'
#' @examples
#' #model hazards as function of birthplace using bunmd_demo file
#' gompertz_mle(formula = death_age ~ bpl_string, left_trunc = 1988, right_trunc = 2005, data = bunmd_demo)
#'
#' @export gompertz_mle

gompertz_mle <- function(formula, left_trunc = 1975, right_trunc = 2005, data, byear = byear, dyear=dyear, lower_age_bound = NULL,
                         upper_age_bound = NULL, weights = NULL, death_age_data_type = 'auto', maxiter = 10000) {

  ## format data
  data_formatted <- data %>%
    dplyr::mutate(byear = as.numeric(as.character(byear))) %>%
    dplyr::select(all.vars(formula), byear) %>%
    dplyr::mutate(
      y = get(all.vars(formula)[1]),
      right_trunc_age = right_trunc - byear,
      left_trunc_age = left_trunc - byear,
      cohort = byear
    ) %>%
    droplevels()

  ## extract weights
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)] # reduces the call to just things specified in m
  mf$drop.unused.levels <- TRUE # adds argument to call
  mf[[1L]] <- quote(stats::model.frame) # stats:modelframe
  mf <- eval(mf, parent.frame())
  w <- as.vector(model.weights(mf))
  if(!is.null(w) && !is.numeric(w)) {
    stop("'weights' must be a numeric vector")
  } else if(is.null(w)) {
    data_formatted <- data_formatted %>% dplyr::mutate(sample_weights = 1)
  } else {
    data_formatted <- cbind(data_formatted, sample_weights = w/mean(w))}


  ## check truncation bounds
  detected_minimum_dyear <- min(data$dyear)
  detected_maximum_dyear <- max(data$dyear)
  if(right_trunc < left_trunc) {
    stop('Invalid truncation bounds')
  }
  if(right_trunc != detected_maximum_dyear | left_trunc != detected_minimum_dyear) {
    warning('Supplied truncation bounds do not align with detected minimal and maximal dates
            in the data. Please make sure they are specified correctly.')
  }

  ## lower bound (e.g., only observe deaths over 65)
  if (!missing(lower_age_bound)) {
    data_formatted <- data_formatted %>%
      dplyr::mutate(left_trunc_age = case_when(
        left_trunc_age < lower_age_bound ~ lower_age_bound,
        TRUE ~ left_trunc_age
      ))
  }

  ## upper bound (e.g., only observe deaths below 100)
  if (!missing(upper_age_bound)) {
    data_formatted <- data_formatted %>%
      dplyr::mutate(right_trunc_age = case_when(
        right_trunc_age > upper_age_bound ~ upper_age_bound,
        TRUE ~ right_trunc_age
      ))
  }

  ## convert integer death years to midpoint
  if (sum(data_formatted$y - floor(data_formatted$y)) == 0){
    data_formatted <- data_formatted %>%
      dplyr::mutate(y = y + 0.5)
  }

  ## remove endpoints
  data_formatted <- data_formatted %>%
    dplyr::filter(y > left_trunc_age & y < right_trunc_age)

  ## get starting parameters
  p.start <- get.par.start(formula, data_formatted)

  model_matrix <- modelr::model_matrix(formula = formula, data = data_formatted) %>%
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
    wt = data_formatted$sample_weights,
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

  ## calculate b
  b <- fit %>%
    dplyr::filter(parameter == "log.b.start") %>%
    dplyr::mutate(parameter = "gompertz_b",
           value = exp(value),
           lower = exp(lower),
           upper = exp(upper))

  ## calculate mode
  fit <- dplyr::bind_rows(b, mode) %>%
    dplyr::bind_rows(fit %>%
                dplyr::filter(!parameter %in% c("b0.start", "log.b.start")))

  ## tidy up fit object
  fit <- fit %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "log.", "")) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "of.", "")) %>%
    dplyr::select(-std.error) %>%
    dplyr::mutate(hr = dplyr::if_else(parameter %in% c("gompertz_mode", "gompertz_b"), NA_real_, exp(value)),
           hr_lower = dplyr::if_else(parameter %in% c("gompertz_mode", "gompertz_b"), NA_real_, exp(lower)),
           hr_upper = dplyr::if_else(parameter %in% c("gompertz_mode", "gompertz_b"), NA_real_, exp(upper)))

  results <- fit %>%
    dplyr::select(parameter, coef = value, coef_lower = lower, coef_upper = upper, hr, hr_lower, hr_upper)

  ## return all results as a list
  gompertz_trunc <- list("starting_values" = p.start, "optim_fit" = optim_fit, "results" = results)

  return(gompertz_trunc)
}
