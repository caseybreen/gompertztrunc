#' Gompertz mle function
#'
#'
#' @param fml the estimation formula
#' @param A data matrix with covariates y, u, l, and covariates, including cohor
#'
#' @return None
#'
#' @export


tgm_simu <- function(n, ## sample size
                     form, ## formula "death_age ~ sex + educ"
                     coefs, ## "true" values for coefficients
                     dummy = NULL, ## dummy flag for each var (optional)
                     sigma = NULL, ## sd for each var (optional)
                     seed = NULL, ## random seed to duplicate data
                     a0 = 10^-4, ## gompertz "alpha"
                     beta = 1 / 10, ## gompertz "beta"
                     verbose = FALSE) ## print internal check
{
  ## proportional hazards model of form

  ## h(x) = exp(beta*x) * a0 * exp(b1*x1 + b2*x2 + ...)
  ##      = exp(beta*x) * exp(b0 + b1*x1 + b2*x2 + ...)
  ##      = exp( beta * x) * exp(X %*% B)
  ## with a0 = exp(b0)
  ##      X  = design matrix of vars
  ##      B  = vector of coefs

  ## 0. Preliminaries


  coefs <- c("b0" = log(a0), coefs) ## include log(a0) as intercept

  ## check to see if right number of coefs with correct labels
  myterms <- terms(form) ## complex object with lots of info from formula
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
  vars.to.sim <- all.vars(form[[3]])
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
      data[, i] <- rnorm(n, mean = 0, sd = 1)
    } ## standardized
    if (!is.null(sigma[i])) {
      data[, i] <- rnorm(n, mean = 0, sd = sigma[i])
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
  form.without.y <- paste(form[[1]], format(form[[3]]))
  form.without.y <- as.formula(form.without.y)
  mat <- model.matrix(form.without.y, data)
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

  B <- cbind(coefs)
  log.effects <- X %*% B
  effects <- exp(log.effects)

  ## 4. generate  gompertz draws
  y <- flexsurv::rgompertz(n, shape = beta, rate = effects) ## ages of death
  data_with_y <- cbind(y, data)
  ## re-label "y" with name used on left hand side of formula
  left.string <- format(form[[2]])
  colnames(data_with_y)[1] <- left.string
  right.string <- format(form[[3]])

  ## 5. check with lm using -10*B as entropy approximation
  ## Since data is untruncated, we can use result that
  ## (1) d e0 / d haz \approx -1/beta
  ## A coefficient value of b3 = 0.20, increases hazards by a factor
  ## exp(b3) = exp(.2) \approx 1.2,
  ## which is like increasing hazards (d haz) by +20%.
  ## From (1), this means life expectancy should go down
  ## by about -0.2/beta \approx -.2 * 10, or about 2 years.
  ## (Note: this approximation will not work for truncated data!)
  lm.form <- update(form, paste(left.string, " ~ ."))
  m <- lm(lm.form, data_with_y)

  B.vec <- as.vector(B) ## true coefs as vector
  names(B.vec) <- rownames(B) ## add rownames
  coef.m.hat <- B.vec / (-beta)
  check.dt <- data.table(
    "coef.name" = names(coef(m)),
    "coef(m)" = round(coef(m), 3),
    "coef.m.hat" = round(coef.m.hat, 3),
    "b.name" = names(B.vec),
    B = round(B.vec, 3)
  )

  if (verbose) {
    print(check.dt)
  }


  simu_data <- as.data.table(data_with_y)
  return(simu_data)
}
