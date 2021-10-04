#' Gompertz mle function
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param fml the estimation formula
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#'
#' @return None
#'
#' @export

gompertz_mle <- function(fml, upper = 2005, lower = 1975, data, byear = byear) {

  ## data
  data <- data %>%
    dplyr::mutate(byear = as.numeric(as.character(byear))) %>%
    dplyr::select(all.vars(fml), byear) %>%
    dplyr::mutate(
      y = get(all.vars(fml)[1]),
      u = upper - byear,
      l = lower - byear,
      cohort = byear
    ) %>%
    dplyr::filter(l < y & y < u)

  ## convert characters to dummies
  if (sum(sapply(data, is.character)) > 0) {
    columns <- data %>%
      dplyr::select(where(is.character)) %>%
      colnames()

    data <- data %>%
      fastDummies::dummy_cols(
        remove_selected_columns = T,
        remove_first_dummy = T,
        select_columns = columns
      )
  }

  ## convert factors to dummies

  if (sum(sapply(data, is.factor)) > 0) {

    columns <- data %>%
      dplyr::select(where(is.factor)) %>%
      colnames()

    data <- data %>%
      fastDummies::dummy_cols(
        remove_selected_columns = T,
        remove_first_dummy = T,
        select_columns = columns
      )
  }

  ## assign predictors
  predictors <- data %>%
    dplyr::select(-y, -u, -l, -cohort, -all.vars(fml)[1], -byear) %>%
    colnames()


  ## get unique cohorts
  cohorts <- sort(unique(data$cohort))

  ## starting values, just let all the modes be the same ("80"
  M.start <- rep(80, length(cohorts))
  names(M.start) <- paste0("coh", cohorts)

  ## and let slope of gompertz start at .1
  ## and the effect of 1 year of educ to lower mortality by 10%
  p.start <- c(
    "mode" = log(M.start),
    "beta" = log(.1)
  )

  ## note we use "b" in original scale, not logged
  vec <- rep(0, length(predictors))
  names(vec) <- predictors

  p.start <- c(p.start, vec)

  ## run optimizer
  ## big fix is making sure maxit is a big number!
  fit <- optim(
    par = p.start, fn = mll.gomp.multi.cohort.cov,
    A = data,
    predictors = predictors,
    hessian = TRUE,
    method = "BFGS",
    control = list(maxit = 5000)
  )

  ## tidy up optim output
  out <- broom::tidy(fit) %>%
    dplyr::mutate(
      lower = value - 1.96 * std.error,
      upper = value + 1.96 * std.error
    ) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "log.", "")) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "of.", "")) %>%
    dplyr::select(-std.error)

  ## remove globally assigned (<<-) predictors
  ## TODO find better way of handling this
  rm(predictors)

  out <- out %>%
    dplyr::mutate(across(where(is.numeric), exp))

  return(out)
}
