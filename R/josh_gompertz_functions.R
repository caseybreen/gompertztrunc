#' Gompertz mle functions from Josh
#'
#'
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

############################## tgm()

get.negLL <- function(par, y, X, y.left, y.right, wt = 1) {
  ## note exp(par) just gets them back to original scale
  beta <- exp(par[1])
  names(beta) <- "b"
  B <- par[2:length(par)] ## vector of parameters, original scale is
  ## in terms of log effect, so no
  ## transformation needed.
  log.A <- X %*% cbind(B) ## add up log effects
  A <- exp(log.A) ## transform to multiplicative effects
  M <- ab2M(a = A, b = beta) ## vector of M values
  num <- wt * dgompertz.M(y, b = beta, M = M) ## is use of wt correct???
  num[num == 0] <- 10^-10 ## very low likelihood for 0 to avoid log(0)
  denom <- pgompertz.M(y.right, b = beta, M) -
    pgompertz.M(y.left, b = beta, M)
  denom[denom == 0] <- 10^-0 ## denom gets bigger value for zeros so
  ## num/denom likelihood is very small.
  LL <- sum(log(num) - log(denom))
  negLL <- -LL ## optim() minimizes so to maximize likelihood we
  ## return negative log-likelihood.
  return(negLL)
}
get.par.start <- function(form, da) {
  ##     da = mydata
  ##     form = formula(y ~ -1 + treat)
  ## we start with lm with intercept
  newform <- update.formula(form, . ~ . + 1)
  m <- lm(newform, da)
  ## (Intercept)        treat
  ##      56.004       -1.494
  ## then let m.start be the intercept
  M.start <- coef(m)["(Intercept)"]
  names(M.start) <- ""
  ## let b.start = 1/10
  b.start <- 1 / 10
  a.start <- bM2a(b = b.start, M = M.start)
  b0.start <- log(a.start)
  ## then use entropy-based ball park. Say entropy is 0.1
  ## say 1 year of change is 1%, so 1 year is like 10% decrease
  ## -perc*H*e0 = change = -(-.1) * .1 * 100 = +1 year
  coef.vec <- coef(m)[names(coef(m)) != "(Intercept)"]
  b.vec.start <- -1 * coef.vec * .1
  ## rename
  par.start <- c(
    log.b.start = log(b.start), b0.start = b0.start,
    b.vec.start
  )

  return(par.start)
}

tgm <- function(formula, data, wt = NULL) {
  ## autostart with lm
  m <- lm(formula, data)
  print(m)
  par.start <- get.par.start(formula, data)
  my.control <- list(
    trace = 0,
    parscale = c(par.scale = par.start),
    maxit = 5000
  )
  print("par.start")
  print(round(par.start, 4))
  ## assign weights (wt). Note: I'm not sure this is working right.
  if (!is.null(data$wt)) {
    wt <- wt
  }
  if (is.null(data$wt)) {
    wt <- 1.0
  }
  ## assign upper and lower age bounds
  ## first, make sure bounds are in data
  if (!("y.left" %in% names(data))) {
    stop("Need to define y.left in data")
  }
  if (!("y.right" %in% names(data))) {
    stop("Need to define y.right in data")
  }
  y.left <- data$y.left
  y.right <- data$y.right
  ##
  Xt <- model.matrix(formula, data = data)
  y.name <- as.character(formula[2]) ## character used for dependent var
  y.val <- data[[y.name]]
  fit <- optim(
    par = par.start,
    fn = get.negLL,
    hessian = TRUE,
    y = y.val,
    X = Xt,
    wt = wt,
    y.left = y.left,
    y.right = y.right,
    control = my.control
  )
  fit$nobs <- nrow(Xt) ## add number of obs to fitted object
  print("fit$par")
  print(fit$par)
  return(fit)
}

###################### helper functions #####################3


get.se <- function(est.vec, hess, log.vec = rep(TRUE, length(est.vec))) {
  fisher_info <- solve(hess)
  est.sd <- sqrt(diag(fisher_info))
  ## if estimates are not log scale then we have the SE
  ## if they are log scale than we can estimate SE
  ## as exp(beta)*SD
  est.se <- exp(est.vec)^(log.vec) * est.sd
  return(est.se)
}

tgm_summary <- function(fit, true.coef.values = NULL,
                        a0 = 10^-4, beta = 1 / 10) {
  ## return parameters with 95% confidence intervals and, if using
  ## simulated data, also the original parameter values for comparison
  nobs <- fit$nobs
  par <- fit$par
  hess <- fit$hess
  se.vec <- get.se(par, hess)
  if (!is.null(true.coef.values)) {
    out <- cbind(
      "est" = fit$par,
      "lower" = fit$par - 2 * se.vec,
      "upper" = fit$par + 2 * se.vec,
      "true" = c(
        "log(beta)" = log(beta),
        "log(a0)" = log(a0), true.coef.values
      )
    )
  }
  if (is.null(true.coef.values)) {
    out <- cbind(
      "est" = fit$par,
      "lower" = fit$par - 2 * se.vec,
      "upper" = fit$par + 2 * se.vec,
      "true" = true.coef.values
    )
  }
  print(paste("nobs:", nobs))
  print(out)
  return(out)
}
