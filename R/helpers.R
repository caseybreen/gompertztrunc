#' Helper functions
#'
#'
#'

## parameterize gompertz with M (mode) and beta (slope)

## we use flexsurv functions and modify just slightly

bM2a <- function(b, M)
{
  a = b* exp(-b * M)
  a
}
ab2M <- function(a, b)
{
  M = -log(a/b)/b
  M
}

library(flexsurv)

pgompertz.M <- function(q, b, M, ...)
{
  a = bM2a(b,M)
  flexsurv::pgompertz(q, shape = b, rate = a, ...)
}

dgompertz.M <- function(x, b, M, ...)
{
  a = bM2a(b,M)
  flexsurv::dgompertz(x, shape = b, rate = a, ...)
}

rgompertz.M <- function(n, b, M)
{
  a = bM2a(b,M)
  flexsurv::rgompertz(n, shape = b, rate = a)
}

hgompertz.M <- function(x, b, M)
{
  a = bM2a(b,M)
  q = x
  p = flexsurv::pgompertz(q, shape = b, rate = a)
  l = 1-p
  d = flexsurv::dgompertz(x, shape = b, rate = a)
  h = d/l
  return(h)
}

## let's check hgompertz
x = 0:100
a = 10^-4
b = 1/10
h.true = a* exp(b*x)
## [1] 2.202647
h.hat = hgompertz.M(x, b, M = ab2M(a,b))
sum(sqrt((h.true - h.hat)^2))
## [1] 2.438451e-07


