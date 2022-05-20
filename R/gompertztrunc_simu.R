#' Simulate Gompertzian death distribution
#'
#'
#'
#' @param n sample size
#' @param formula estimation formula
#' @param coefs named vectors of coefficients and corresponding true values
#' @param dummy vector flags for each coefficient
#' @param sigma standard deviation for each variable
#' @param seed random seed to duplicate data
#' @param a0 Gompertz alpha parameter
#' @param b Gompertz b parameter
#' @param verbose print internal check if true
#'
#' @return data frame of simulated death ages and covariate values
#'
#' @examples
#' gompertztrunc_simu(n=1000, formula = death_age ~ sex + ambient_temp,
#' coefs = c('sex'=-0.8, 'ambient_temp'=0.3), dummy=c(TRUE,FALSE))
#'
#' @importFrom rlang .data
#' @export


gompertztrunc_simu <- function(n, ## sample size
                     formula, ## formula "death_age ~ sex + educ"
                     coefs, ## "true" values for coefficients
                     dummy = NULL, ## dummy flag for each var (optional)
                     sigma = NULL, ## sd for each var (optional)
                     seed = NULL, ## random seed to duplicate data
                     a0 = 10^-4, ## gompertz "alpha"
                     b = 1 / 10, ## gompertz "b"
                     verbose = FALSE) ## print internal check
{
  ## proportional hazards model of form

  ## h(x) = exp(b*x) * a0 * exp(beta1*x1 + beta2*x2 + ...)
  ##      = exp(b*x) * exp(beta0 + beta1*x1 + beta2*x2 + ...)
  ##      = exp( b * x) * exp(X %*% Beta)
  ## with a0 = exp(b0)
  ##      X  = design matrix of vars
  ##      Beta  = vector of coefs

  ## 0. Preliminaries


  coefs <- c("b0" = log(a0), coefs) ## include log(a0) as intercept

  ## check to see if right number of coefs with correct labels
  myterms <- stats::terms(formula) ## complex object with lots of info from formula
  coef.names.from.formula <- attr(myterms, "term.labels")
  ##
  if (!identical(names(coefs)[-1], coef.names.from.formula)) {
    stop(c(
      "term labels not correct. should be: ",
      paste(coef.names.from.formula, collapse = " ")
    ))
  }

  ## check to see if "dummy" has right number of terms
  ## get names of vars to simulate
  vars.to.sim <- all.vars(formula[[3]])
  k <- length(vars.to.sim)
  if (!is.null(dummy) & length(dummy) != k) {
    print("incorrect length of dummy argument:\n
           should equal number of vars in formula.\n
           E.g., in y ~ x1 + x2 + x1:x2, there are 2 vars")
  }

  ## 1. simulate covariates (e.g., x1, x2, and x3)

  ## initialize data frame
  data <- data.frame(matrix(NA, nrow = n, ncol = k))
  names(data) <- vars.to.sim

  if (!is.null(seed)) {
    set.seed(seed)
  }

  for (i in 1:k)
  {
    if (is.null(sigma[i])) {
      data[, i] <- stats::rnorm(n, mean = 0, sd = 1)
    } ## standardized
    if (!is.null(sigma[i])) {
      data[, i] <- stats::rnorm(n, mean = 0, sd = sigma[i])
    } ## own variance
  }
  ## create dummy vars (for now just random 0,1 coding)
  if (!is.null(dummy)) {
    for (i in 1:k)
    {
      if (dummy[i] == TRUE) {
        data[, i] <- ifelse(data[, i] > 0, 1, 0)
      }
    }
  }

  ## 2. get design matrix
  form.without.y <- paste(formula[[1]], format(formula[[3]]))
  form.without.y <- stats::as.formula(form.without.y)
  mat <- stats::model.matrix(form.without.y, data)
  ## head(mat)
  ##   (Intercept)          x1 x2         x3      x2:x3
  ## 1           1 -0.96193342  1 -1.2913493 -1.2913493
  ## 2           1 -0.29252572  1  2.6350454  2.6350454
  ## 3           1  0.25878822  1  0.4870723  0.4870723
  ## 4           1 -1.15213189  0  0.8538923  0.0000000
  ## 5           1  0.19578283  0  1.0884427  0.0000000
  ## 6           1  0.03012394  1  0.2260140  0.2260140
  ## label design matrix "X"
  X <- mat

  ## 3. multiply out to get individual effects

  Beta <- cbind(coefs)
  log.effects <- X %*% Beta
  effects <- exp(log.effects)

  ## 4. generate  gompertz draws
  y <- flexsurv::rgompertz(n, shape = b, rate = effects) ## ages of death
  data_with_y <- cbind(y, data)
  ## re-label "y" with name used on left hand side of formula
  left.string <- deparse(formula[[2]])
  colnames(data_with_y)[1] <- left.string
  right.string <- format(formula[[3]])

  ## 5. check with lm using -10*B as entropy approximation
  ## Since data is untruncated, we can use result that
  ## (1) d e0 / d haz \approx -1/b
  ## A coefficient value of b3 = 0.20, increases hazards by a factor
  ## exp(b3) = exp(.2) \approx 1.2,
  ## which is like increasing hazards (d haz) by +20%.
  ## From (1), this means life expectancy should go down
  ## by about -0.2/b \approx -.2 * 10, or about 2 years.
  ## (Note: this approximation will not work for truncated data!)
  lm.form <- stats::update(formula, paste(left.string, " ~ ."))
  m <- stats::lm(lm.form, data_with_y)

  Beta.vec <- as.vector(Beta) ## true coefs as vector
  names(Beta.vec) <- rownames(Beta) ## add rownames
  coef.m.hat <- Beta.vec / (-b)
  check.dt <- data.table::data.table(
    "coef.name" = names(stats::coef(m)),
    "coef(m)" = round(stats::coef(m), 3),
    "coef.m.hat" = round(coef.m.hat, 3),
    "beta.name" = names(Beta.vec),
    Beta = round(Beta.vec, 3)
  )

  if (verbose) {
    print(check.dt)
  }


  simu_data <- data.table::as.data.table(data_with_y)
  return(simu_data)
}
