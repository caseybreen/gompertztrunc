#' Run diagnostic plots - plot hazards
#'
#' Empirical and modeled distribution of ages of death.
#'
#' @param data data from model plot
#' @param object gompertz_mle object
#' @param var variable of interest
#'
#' @return a ggplot object
#'
#' @importFrom rlang := .data
#' @export
#'

mle_plot_hazard <- function(data, object, var = hs, death_range = c(65, 110)) {

  ## create lists and counter
  counter = 1
  death_counts <- list()
  death_counts_modeled <- list()

  ## get factor levels
  cov_levels <- levels(as.factor(data[[var]]))

  ## extract gompertz parameters
  b <- object$results$coef[[1]]
  M <- object$results$coef[[2]]

  ## calculate hazard ratio
  hr <- object$results %>%
    filter(parameter == !!var) %>%
    select(hr) %>% as.numeric()

  ## calculate lifetable quantities (modeled)
  hx <- hx_calc(b = b, M = M, x = 0:121) # 0.6158778 *
  lx <- c(1, lx_calc(hx))
  dx <- dx_calc(1, lx)

  ## calculate number of deaths simulate
  deaths <- tibble(dx, death_age = 0:122) %>%
    filter(!is.na(dx))

  ## calculate bounds dropping endpoints
  bounds <- data %>% summarize(min(death_age) + 1, max(death_age) - 1) %>%
    as.numeric()

  ## calculate multiplier
  deaths_sim <- deaths %>%
    filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    summarize(sum(dx)) %>%
    as.numeric()

  ## calculate deaths real
  deaths_real <- data %>%
    filter(get(var) == cov_levels[1]) %>%
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
    mutate(!!var := cov_levels[1])

  ## get covariates
  covariates <- str_remove(object$results$parameter[3:length(object$results$parameter)], pattern = var)

  for (cov in covariates) {

    ## move counter
    counter = counter + 1

    ## calculate hazard ratio
    hr <- object$results %>%
      mutate(parameter = str_remove(parameter, pattern = var)) %>%
      filter(parameter == !!cov) %>%
      select(hr) %>% as.numeric()

    ## calculate lifetable quantities (modeled)
    hx <- hr * hx_calc(b = b, M = M, x = 0:121) # 0.6158778 *
    lx <- c(1, lx_calc(hx))
    dx <- dx_calc(1, lx)

    ## calculate number of deaths simulate
    deaths <- tibble(dx, death_age = 0:122) %>%
      filter(!is.na(dx))

    ## calculate bounds dropping endpoints
    bounds <- data %>% summarize(min(death_age) + 1, max(death_age) - 1) %>%
      as.numeric()

    ## calculate multiplier
    deaths_sim <- deaths %>%
      filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
      summarize(sum(dx)) %>%
      as.numeric()

    deaths_real <- data %>%
      filter(get(var) == cov) %>%
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
      mutate(!!var := covariates[[counter-1]])

  }

  ## death counts modeled
  death_counts_modeled <- bind_rows(death_counts_modeled) %>%
    rename(var=!!var)

  ## number of deaths
  deaths <- data %>%
    mutate(death_age = floor(death_age)) %>%
    rename(var=!!var) %>%
    group_by(var, death_age) %>%
    summarize(dx = n())

  ## calculate radix
  radix <- death_counts_modeled %>%
    group_by(var) %>%
    filter(death_age %in% 0:120) %>%
    summarize(radix = sum(dx))

  ## calculate lx
  hazards <- deaths %>%
    left_join(radix, by = "var") %>%
    mutate(death = cumsum(dx)) %>%
    mutate(lx = radix - lag(death)) %>% # + 0.5*(dx)
    mutate(hx = dx/lx) %>%
    mutate(type = "observed")

  hazards_modeled <- death_counts_modeled %>%
    mutate(type = "modeled")

  hr_ratio_plot <- hazards %>%
    bind_rows(hazards_modeled) %>%
    filter(between(death_age, left = death_range[1], right = death_range[2])) %>%
    select(death_age, var, hx, type) %>%
    ggplot() +
    geom_line(aes(x = death_age, y = log(hx), color = var, linetype = type),  size = 1) +
    xlim(death_range) +
    ggsci::scale_color_lancet() +
    cowplot::theme_cowplot() +
    labs(x = "Death Age",
         y = "Log Hazard rate") +
    theme(legend.position = "bottom", legend.title = element_blank())


  ## return plot
  return(hr_ratio_plot)
}
