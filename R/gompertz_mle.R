#' Gompertz MLE function
#'
#' Fits a Gompertz distribution with proportional hazards
#' to doubly-truncated mortality data using maximum likelihood estimation.
#'
#'
#' @param formula the estimation formula
#' @param left_trunc left truncation year
#' @param right_trunc right truncation year
#' @param data a data frame containing variables in the model
#' @param byear vector of birth years
#' @param dyear vector of death years
#' @param lower_age_bound  lowest age at death to include (optional)
#' @param upper_age_bound highest age at death to include (optional)
#' @param weights an optional vector of individual weights
#' @param start an optional vector of starting values for the optimizer. must be
#' a numeric vector that exactly matches the output of
#' \code{get.par.start(formula, data)} in length and element names.
#' @param death_age_data_type option for handling of continuous and discrete
#' death age variable (not yet implemented)
#' @param maxiter maximum number of iterations for optimizer
#'
#'
#' @return Returns a named list consisting of the following components
#' (See [stats::optim()] for additional details):
#' \describe{
#'   \item{\code{starting_values}}{list of starting values of parameters}
#'   \item{\code{optim_fit}}{A list consisting of:
#'   \describe{
#'     \item{\code{par}}{best estimation of parameter values}
#'     \item{\code{value}}{log likelihood}
#'     \item{\code{counts}}{number of calls to function and gradient}
#'     \item{\code{convergence}}{returns 0 if the model converged, for other values see [stats::optim()] }
#'     \item{\code{message}}{any other information returned by optimizer}
#'     \item{\code{hessian}}{Hessian matrix}
#'     }
#'   }
#'   \item{\code{results}}{A table of estimates and upper/lower bounds of the 95 percent confidence interval
#'   for the estimates. Confidence interval computed as 1.96*standard_error.}
#' }
#'
#' @examples
#' \dontrun{
#' #model hazards as function of birthplace using bunmd_demo file
#' gompertz_mle(formula = death_age ~ bpl_string, left_trunc = 1988, right_trunc = 2005,
#' data = bunmd_demo)
#' }
#' @export gompertz_mle

gompertz_mle <- function(formula, left_trunc = 1975, right_trunc = 2005, data, byear = byear, dyear=dyear, lower_age_bound = NULL,
                         upper_age_bound = NULL, weights = NULL, start= NULL, death_age_data_type = 'auto', maxiter = 10000) {

  ## extract arguments
  mf <- rlang::call_match(defaults=TRUE)
  m <- match(c("formula", "data", "byear", "dyear", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <-  "na.pass"
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())

  ## birth year and death year vectors
  by <- as.vector(stats::model.extract(frame=mf, component = "byear"))
  dy <- as.vector(stats::model.extract(frame=mf, component = "dyear"))

  ## weights
  w <- as.vector(stats::model.weights(mf))
  if(!is.null(w) && !is.numeric(w)) {
    stop("'weights' must be a numeric vector")
  } else if(!is.null(w)) {
    w <- as.numeric(as.character(w))
  } else{
    w <- rep(1, nrow(data)) # set all weights to 1 if they are not specified
  }

  ## check truncation bounds
  if(right_trunc < left_trunc) {
    stop('Invalid truncation bounds')
  }
  detected_minimum_dyear <- min(as.numeric(as.character(dy)), na.rm=TRUE)
  detected_maximum_dyear <- max(as.numeric(as.character(dy)), na.rm=TRUE)
  if(right_trunc != detected_maximum_dyear | left_trunc != detected_minimum_dyear) {
    warning('Supplied truncation bounds do not align with detected minimal and maximal dates
            in the data. Please make sure they are specified correctly.')
  }

  ## format data
  sample_weights <- left_trunc_age <- right_trunc_age <- y <-  NULL
  data_formatted <- data %>%
    dplyr::mutate(byear = as.numeric(as.character(by)),
                  dyear = as.numeric(as.character(dy)),
                  sample_weights = w) %>%
    dplyr::select(all.vars(formula), byear, dyear, sample_weights) %>%
    tidyr::drop_na() %>% # drop all rows with NA values
    dplyr::mutate(
      sample_weights = sample_weights/mean(sample_weights), # normalize weight
      y = get(all.vars(formula)[1]),
      right_trunc_age = right_trunc - byear,
      left_trunc_age = left_trunc - byear,
      cohort = byear
    ) %>%
    droplevels()

  if(nrow(data_formatted) < nrow(data)) {
    message('Rows containing NA values have been dropped')
  }

  ## lower bound (e.g., only observe deaths over 65)
  if (!missing(lower_age_bound)) {
    data_formatted <- data_formatted %>%
      dplyr::mutate(left_trunc_age = dplyr::case_when(
        left_trunc_age < lower_age_bound ~ lower_age_bound,
        TRUE ~ left_trunc_age
      ))
  }

  ## upper bound (e.g., only observe deaths below 100)
  if (!missing(upper_age_bound)) {
    data_formatted <- data_formatted %>%
      dplyr::mutate(right_trunc_age = dplyr::case_when(
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
  if (!is.null(start)) {
    if(!is.vector(start) | length(start) != length(p.start) | is.null(names(start)) | !is.numeric(start) |
       ( (length(start) == length(p.start)) & !is.null(names(start)) &(mean(names(start) == names(p.start)))!=1 ) ) {
        stop("Invalid start parameter. starting values must be a numeric vector with exactly the
            same length and named elements as output of get.par.start(formula, data).")
    }
    else {
      p.start <- start
    }
  }

  model_matrix <- modelr::model_matrix(formula = formula, data = data_formatted) %>%
    as.matrix()

  ## create controls
  my.control <- list(
    trace = 0,
    parscale = c(par.scale = p.start),
    maxit = maxiter
  )

  ## get optimization function
  optim_fit <- stats::optim(
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
  lower <-  upper <- NULL
  fit <- broom::tidy(fit) %>%
    dplyr::mutate(
      lower = .data$value - 1.96 * .data$std.error,
      upper = .data$value + 1.96 * .data$std.error
    )

  ## calculate mode

  # Use delta method for estimating Var(M), a transformation of a and b
  # First we have to get Var(a) and Var(b) in the non-log scale
  log_a_hat <- optim_fit$par[2]
  a_hat <- exp(log_a_hat)
  log_b_hat <- optim_fit$par[1]
  b_hat <- exp(log_b_hat)
  var_covar_mat_log <- solve(optim_fit$hessian)
  sigma_vec_log <- sqrt(diag(solve(optim_fit$hessian)))[1:2]
  names(sigma_vec_log) <- c("log.b", "log.a")
  var_a_hat <- sigma_vec_log["log.a"]^2 * exp(2*log_a_hat) # Var(exp(Y)) = Var(Y)*exp(2*E[Y])
  var_b_hat <- sigma_vec_log["log.b"]^2 * exp(2*log_b_hat)

  covar_a_b_log <- var_covar_mat_log[1,2] # Cov(log(a), log(b))
  cov_a_b_hat <- exp(log_a_hat + log_b_hat)*covar_a_b_log

  # Var(M) = [dM/da, dM/db] %*% Cov(a,b) %*% [dM/da, dM/db]^T
  var_M_hat <- (1/(b_hat*a_hat)^2)*var_a_hat + ((log(a_hat/b_hat) + 1)/(b_hat^2))^2*var_b_hat -
    2*( (log(a_hat/b_hat) + 1)/(b_hat^3 * a_hat) )*cov_a_b_hat
  se_M_hat <- sqrt(var_M_hat)

  parameter <- NULL
  mode <- fit %>%
    dplyr::filter(parameter == "b0.start") %>%
    dplyr::mutate(parameter = "gompertz_mode",
                  std.error = se_M_hat,
                  value = ab2M(exp(log_a_hat), b = exp(log_b_hat)),
                  lower = .data$value - 1.96*se_M_hat,
                  upper = .data$value + 1.96*se_M_hat)

  ## calculate b
  b <- fit %>%
    dplyr::filter(parameter == "log.b.start") %>%
    dplyr::mutate(parameter = "gompertz_b",
           value = exp(.data$value),
           lower = exp(.data$lower),
           upper = exp(.data$upper))

  ## calculate mode
  fit <- dplyr::bind_rows(b, mode) %>%
    dplyr::bind_rows(fit %>%
                dplyr::filter(!parameter %in% c("b0.start", "log.b.start")))

  ## tidy up fit object
  hr_lower <- hr_upper <- hr <-  NULL
  fit <- fit %>%
    dplyr::select(-.data$std.error) %>%
    dplyr::mutate(hr = dplyr::if_else(.data$parameter %in% c("gompertz_mode", "gompertz_b"), NA_real_, exp(.data$value)),
           hr_lower = dplyr::if_else(.data$parameter %in% c("gompertz_mode", "gompertz_b"), NA_real_, exp(.data$lower)),
           hr_upper = dplyr::if_else(.data$parameter %in% c("gompertz_mode", "gompertz_b"), NA_real_, exp(.data$upper)))

  results <- fit %>%
    dplyr::select(parameter, coef = .data$value, coef_lower = lower, coef_upper = upper, hr, hr_lower, hr_upper)

  ## return all results as a list
  gompertz_trunc <- list("starting_values" = p.start, "optim_fit" = optim_fit, "results" = results)

  return(gompertz_trunc)
}
