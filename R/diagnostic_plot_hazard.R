#' Run diagnostic plots - plot hazards
#'
#' Compare empirical and model-based estimated hazard ratios within a cohort. Only
#' works with a single discrete covariate.
#'
#' @param data data.frame use for gompertz_mle
#' @param object gompertz_mle object
#' @param covar covariate of interest
#' @param death_var death age variable
#' @param xlim x-limits for figure
#'
#'
#' @return a ggplot object
#'
#' @importFrom rlang := .data
#' @export
#'
#' @examples
#' # Diagnostic hazard plot to assess model fit. This diagnostic plot only works for one birth cohort
#' # and discrete covariates.
#' gompertztrunc::diagnostic_plot_hazard(object = gradient, data = sim_data_trunc, covar = "sex",
#'  death_range = c(65, 85))
#'

diagnostic_plot_hazard <- function(data, object, death_var = "death_age", covar = hs, xlim = c(65, 110)) {


  ## make death age var
  data <- data %>%
    rename(death_age = !!death_var) %>%
    mutate(death_age = floor(death_age)) # %>% filter(death_age %in% c((min(death_age)+1):(max(death_age)-1)))

  ## create lists and counter
  counter = 1
  death_counts <- list()
  death_counts_modeled <- list()

  ## get factor levels
  cov_levels <- levels(as.factor(data[[covar]]))

  ## extract gompertz parameters
  b <- object$results$coef[[1]]
  M <- object$results$coef[[2]]

  ## calculate hazard ratio
  hr <- object$results %>%
    filter(parameter == !!covar) %>%
    select(hr) %>% as.numeric()

  ## calculate lifetable quantities (modeled)
  hx <- hx_calc(b = b, M = M, x = 0:121 + 0.5) # use midpoints because we're trying to calculate total deaths between x and x + 1
  lx <- c(1, lx_calc(hx))
  dx <- dx_calc(1, lx)

  ## calculate number of deaths simulate
  deaths <- tibble(dx, death_age = 0:122) %>%
    filter(!is.na(dx))

  ## calculate bounds dropping endpoints
  bounds <- data %>%
    summarize(min(death_age), max(death_age) ) %>%
    as.numeric()

  ## calculate multiplier
  deaths_sim <- deaths %>%
    filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    summarize(sum(dx)) %>%
    as.numeric()

  ## calculate deaths real
  deaths_real <- data %>%
    filter(get(covar) == cov_levels[1]) %>%
    filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    summarize(n()) %>%
    as.numeric()

  ## multipled
  multiplier <- deaths_real/deaths_sim

  ## new dx and simulate deaths
  dx <- dx_calc(multiplier, lx)

  ## number of deaths
  deaths <- tibble(dx, hx = c(hx, NA), death_age = 0:122) %>%
    filter(!is.na(dx))

  death_counts_modeled[[counter]] <- deaths %>%
    mutate(!!covar := cov_levels[1])

  ## get covariates
  covariates <- str_remove(object$results$parameter[3:length(object$results$parameter)], pattern = covar)

  for (cov in covariates) {

    ## move counter
    counter = counter + 1

    ## calculate hazard ratio
    hr <- object$results %>%
      mutate(parameter = str_remove(parameter, pattern = covar)) %>%
      filter(parameter == !!cov) %>%
      select(hr) %>% as.numeric()

    ## calculate lifetable quantities (modeled)
    hx <- hr * hx_calc(b = b, M = M, x = 0:121 + 0.5) # 0.6158778 *
    lx <- c(1, lx_calc(hx))
    dx <- dx_calc(1, lx)

    ## calculate number of deaths simulate
    deaths <- tibble(dx, death_age = 0:122) %>%
      filter(!is.na(dx))

    ## calculate bounds dropping endpoints
    bounds <- data %>% summarize(min(death_age), max(death_age)) %>%
      as.numeric()

    ## calculate multiplier
    deaths_sim <- deaths %>%
      filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
      summarize(sum(dx)) %>%
      as.numeric()

    deaths_real <- data %>%
      filter(get(covar) == cov) %>%
      filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
      summarize(n()) %>%
      as.numeric()

    ## multiplied
    multiplier <- deaths_real/deaths_sim

    ## new dx and simulate deaths
    dx <- dx_calc(multiplier, lx)

    deaths <- tibble(dx, hx = c(hx, NA), death_age = 0:122) %>%
      filter(!is.na(dx))

    death_counts_modeled[[counter]] <- deaths %>%
      mutate(!!covar := covariates[[counter-1]])
  }

  ## death counts modeled
  death_counts_modeled <- bind_rows(death_counts_modeled) %>%
    rename(covar=!!covar)

  ## number of deaths
  deaths <- data %>%
    rename(covar=!!covar) %>%
    group_by(covar, death_age) %>%
    summarize(dx = n())

  ## calculate radix
  radix <- death_counts_modeled %>%
    group_by(covar) %>%
    filter(death_age %in% (max(deaths$death_age)+1):120) %>%
    summarize(radix = sum(dx))

  death_count <- deaths %>%
    group_by(covar) %>%
    summarize(obs_deaths = sum(dx))

  ## calculate lx
  hazards <- deaths %>%
    left_join(radix, by = "covar") %>%
    left_join(death_count, by = "covar") %>%
    mutate(death = cumsum(dx)) %>%
    mutate(lx = obs_deaths + radix - lag(death)) %>% #
    mutate(hx = -log(lead(lx) / lx)) %>%
    mutate(type = "observed")

  hazards_modeled <- death_counts_modeled %>%
    mutate(type = "modeled")

  hazards_modeled_alt <- death_counts_modeled %>%
    left_join(radix, by = "covar") %>%
    mutate(death = cumsum(dx)) %>%
    mutate(lx = radix - lag(death)) %>% #
    mutate(hx = dx/lx) %>%
    mutate(hx = -log(lead(lx) / lx)) %>%
    mutate(type = "modeled")

  hr_ratio_plot <- hazards %>%
    bind_rows(hazards_modeled) %>%
    filter(dplyr::between(x = death_age, left = xlim[1], right = xlim[2])) %>%
    select(death_age, covar, hx, type) %>%
    ggplot() +
    geom_line(aes(x = death_age, y = log(hx), color = covar, linetype = type, alpha = type),  size = 1.3 ) +
    ggsci::scale_color_lancet() +
    cowplot::theme_cowplot() +
    labs(x = "Death Age",
         y = "Log Hazard rate") +
    scale_alpha_discrete(range = c(0.5, 1)) +
    theme(legend.position = "bottom", legend.title = element_blank())


  ## return plot
  return(hr_ratio_plot)

}

