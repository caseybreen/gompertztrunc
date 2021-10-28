#' Gompertz mle functions from Josh
#'
#'
#'
#' @return None
#'
#' @export
#'
#'
#'
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
