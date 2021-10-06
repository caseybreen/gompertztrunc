#' Gompertz mle function
#'
#'
#' @param p the initialization function
#'
#' @return None
#'
#' @export


get.par.start <- function(fml, data)
{
  ##     da = mydata
  ##     form = formula(y ~ -1 + treat)
  ## we start with lm with intercept
  newform <- update.formula(fml, . ~ . + 1)
  m <- lm(newform, data)
  ## (Intercept)        treat
  ##      56.004       -1.494
  ## then let m.start be the intercept
  M.start = coef(m)["(Intercept)"]
  names(M.start) <- ""
  ## let b.start = 1/10
  b.start = 1/10
  a.start = bM2a(b = b.start, M = M.start)
  b0.start = log(a.start)
  ## then use entropy-based ball park. Say entropy is 0.1
  ## say 1 year of change is 1%, so 1 year is like 10% decrease
  ## -perc*H*e0 = change = -(-.1) * .1 * 100 = +1 year
  coef.vec <- coef(m)[names(coef(m)) != "(Intercept)"]
  b.vec.start = -1 * coef.vec * .1
  ## rename
  par.start = c(log.b.start = log(b.start), b0.start = b0.start,
                b.vec.start)

  return(par.start)
}




