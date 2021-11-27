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


get.par.start <- function(formula, da) {
  ##     da = mydata
  ##     formula = formula(y ~ -1 + treat)
  ## we start with lm with intercept
  newform <- update.formula(formula, . ~ . + 1)
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
