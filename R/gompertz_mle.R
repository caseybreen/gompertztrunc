#' Gompertz mle function
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param fml the estimation formula
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#'
#' @return None
#'
#' @export

gompertz_mle <- function(fml, upper = 2005, lower = 1975, data, byear = byear) {

  ## data
  data_formatted <- data %>%
    dplyr::mutate(byear = as.numeric(as.character(byear))) %>%
    dplyr::select(all.vars(fml), byear) %>%
    dplyr::mutate(
      y = get(all.vars(fml)[1]),
      u = upper - byear,
      l = lower - byear,
      cohort = byear
    ) %>%
    dplyr::mutate(intercept = 1) %>% ## create intercept
    dplyr::filter(l < y & y < u) %>%
    dplyr::mutate(y = round(y)) ## round

  ## add weights

  if(!is.null(data_formatted$wt))
    wt = wt
  if(is.null(data_formatted$wt))
    wt = 1.0

  ## get unique cohorts
  cohorts <- sort(unique(data_formatted$cohort))

  ## starting values, just let all the modes be the same ("80"
  M.start <- rep(80, length(cohorts))
  names(M.start) <- paste0("coh", cohorts)

  ## get starting parameters
  p.start <- get.par.start(fml, data_formatted)

  model_matrix <- modelr::model_matrix(formula = fml, data = data_formatted) %>%
    as.matrix()

  ## create comtrols
  my.control = list(trace = 0,
                    parscale = c(par.scale = p.start),
                    maxit = 5000)


  fit <- optim(par = p.start,
               fn = get.negLL,
               hessian = TRUE,
               y = data_formatted$y,
               X = model_matrix,
               wt = wt,
               y.left = data_formatted$l[1],
               y.right = data_formatted$u[1],
               control = my.control)

  # ## run optimizer
  # fit <- optim(
  #   par = p.start, fn = mll.gomp.multi.cohort.cov,
  #   A = model_matrix,
  #   predictors = predictors,
  #   hessian = TRUE,
  #   control = list(maxit = 5000)
  # )

  ## tidy up optim output
  out <- broom::tidy(fit) %>%
    dplyr::mutate(
      lower = value - 1.96 * std.error,
      upper = value + 1.96 * std.error
    ) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "log.", "")) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "of.", "")) %>%
    dplyr::select(-std.error)


  out <- out %>%
    dplyr::mutate(across(where(is.numeric), exp))

  return(out)
}

