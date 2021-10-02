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
#'
#' @export

gompertz_mle <- function(fml, upper = 2005, lower = 1975, data) {

  fml <<- fml

  data <- data %>%
    select(all.vars(fml), byear) %>%
    fastDummies::dummy_cols(remove_selected_columns = T, remove_first_dummy = T) %>%
    mutate(y = get(all.vars(fml)[1]),
           u = upper - byear,
           l = lower - byear,
           cohort = byear) %>%
    filter(l < y & y < u) %>%
    mutate(across(where(is.character), is.numeric))

  predictors <<- data %>% select(-y, -u, -l, -cohort, -all.vars(fml)[1], -byear) %>% colnames()

  ## check densities
  ## ok, it looks like we don't have end-of-interval issues
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
  vec <- rep(-0.1, length(predictors))
  names(vec) <- predictors

  p.start <- c(p.start, vec)

  ## run optimizer
  ## big fix is making sure maxit is a big number!
  fit <- optim(
    par = p.start, fn = mll.gomp.multi.cohort.cov,
    A = data,
    hessian = TRUE,
    method="BFGS",
    control = list(maxit = 5000)
  )

  ## tidy up optim output
  out <- tidy(fit) %>%
    mutate(lower = value - 1.96 * std.error,
           upper = value + 1.96 * std.error) %>%
    mutate(parameter = str_replace_all(parameter, "log.", "")) %>%
    mutate(parameter = str_replace_all(parameter, "of.", "")) %>%
    select(-std.error)

  ## exponentiate beta and m — not covariates
  out <- out %>%
    filter(!parameter %in% predictors) %>%
    mutate(across(where(is.numeric), exp)) %>%
    bind_rows(out %>%
                filter(parameter %in% predictors))

  return(out)
}
