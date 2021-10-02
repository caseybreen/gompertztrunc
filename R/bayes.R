# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

multicohort_truncated_fe_cov_model_text = "functions {
  real get_alpha(real mode, real beta) {
    real alpha = beta/exp(mode*beta);
    return(alpha);
  }
  real gompertz_lcdf(real x, real mode, real beta, real b, real C){
    real alpha = get_alpha(mode, beta) * exp(C*b);
    real cdf = 1 - exp((-alpha/beta)*(exp(beta*x)-1));
    return log(cdf);
  }
}
data {
  int N;                   // number of observations
  real L[N];               // lower bound = lowest observed age
  real U[N];               // upper bound = highest observed age + 1
  int cohort_index[N];     // assigns cohorts to ordinal numbers 1,2,..,num_cohorts
  int num_cohorts;         // number of cohorts included in the data set
  real C[N];               // a single covariate vector
  real<lower=0> x[N];      // observed ages at death
}
parameters {
  real<lower=0> mode[num_cohorts];    // modal age at death param (a vector)
  real<lower=0.001> beta;             // 'force of mortality' param
  real b;                             // effect of the single covariate
}
model {
  for (i in 1:N) {
     if (x[i] < L[i] || x[i] > U[i]) {
          target += negative_infinity();
     }
     else {
          target += log_diff_exp(gompertz_lcdf(x[i] + 1|mode[cohort_index[i]], beta, b, C[i]),
                                gompertz_lcdf(x[i] | mode[cohort_index[i]],beta, b, C[i])) -
                    log_diff_exp(gompertz_lcdf(U[i] | mode[cohort_index[i]], beta, b, C[i]),
                                gompertz_lcdf(L[i] | mode[cohort_index[i]], beta, b, C[i]));
     }
  }
  mode ~ uniform(70, 90);             // prior for m (we will have priors be all the same regardless of cohort)
  beta ~ uniform(0.001, 0.3);         // prior for beta
  b ~ normal(0,2);                    // prior for b
}"


bayes <- function(df, lower= 1975, upper = 2005){

  cohorts <- sort(unique(df$byear))

  multicohort_fe_cov_model = stan_model(model_code = multicohort_truncated_fe_cov_model_text,
                                        model_name='fixed effects gompertz with multiple cohorts, common beta, single covariate')
  init_multicohort_fe_cov_model <- function () {list(beta=0.08, mode=as.array(rep(75, cohorts)), b=0)}

  df <- as.data.table(df)

  A = df[, .(y = death_age,
             u = upper - byear,
             l = lower - byear,
             cohort = byear,
             covar = educ_yrs)]

  At = A[l < y & y < u]


  # # drop the endpoints
  # At <- At %>% group_by(byear) %>%
  #   mutate(lb = min(death_age), ub = max(death_age)) %>%
  #   ungroup() %>%
  #   filter(death_age > lb & death_age < ub)

  # put this in a list for stan. Notice the lower bound adjustment; this ensures we are not including an
  # 'empty' age in the distribution after dropping data at the lower endpoint. The upper bound is fine as
  # it because we actually want that to be one higher than the highest observed age after dropping endpoints.
  data_list <- list(
    N = nrow(At),
    L = At$l ,
    U = At$u,
    cohort_index = At$cohort - min(At$cohort) + 1,
    num_cohorts = cohorts,
    C = At$covar,
    x = At$y)

  covariate <- sampling(object = multicohort_fe_cov_model,
                        data = data_list,
                        init = init_multicohort_fe_cov_model,
                        chains = 2,
                        iter   = 3000,
                        warmup = 200,
                        cores = 2)

  return(covariate)
}
