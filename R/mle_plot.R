#' Run diagnostic plots
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

mle_plot <- function(data, object, var = hs, death_range =c(65, 110)) {

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
  hx <- hx_calc(b = b, M = M, x = 1:121) # 0.6158778 *
  lx <- c(1, lx_calc(hx))
  dx <- dx_calc(1, lx)

  ## calculate number of deaths simulate
  deaths <- tibble(dx, death_age = 1:122) %>%
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
  deaths <- tibble(dx, death_age = 1:122) %>%
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
    hx <- hr * hx_calc(b = b, M = M, x = 1:121) # 0.6158778 *
    lx <- c(1, lx_calc(hx))
    dx <- dx_calc(1, lx)

    ## calculate number of deaths simulate
    deaths <- tibble(dx, death_age = 1:122) %>%
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

    deaths <- tibble(dx, death_age = 1:122) %>%
      filter(!is.na(dx))

    death_counts_modeled[[counter]] <- deaths %>%
      mutate(!!var := covariates[[counter-1]])

  }

  death_counts_modeled <- bind_rows(death_counts_modeled) %>%
    rename(var=!!var)

  ## create plot
  plot <- data %>%
    rename(var=!!var) %>%
    ggplot() +
    geom_histogram(aes(x = death_age), binwidth  = 1, color = "black", fill = "grey") +
    cowplot::theme_cowplot() +
    geom_line(data = death_counts_modeled, aes(x = death_age -.5, y = dx, color = "Modeled"), size = 1, linetype = "solid") +
    labs(x = "Death Age",
         y = "n",
         title = "") +
    scale_color_manual(name = "", values = c("Modeled" = "blue")) +
    theme(legend.position = "bottom", legend.key.width= unit(1.5, 'cm')) +
    labs(x = "Death Age",
         y = "n") +
    xlim(death_range) +
    facet_wrap(~var, scales = "free")

  ## return plot
  return(plot)
}
