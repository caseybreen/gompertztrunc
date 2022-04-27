#' Run diagnostic plots
#'
#' Compare empirical and model-based distribution of ages of death within a cohort. Only
#' works with a single discrete covariate.
#'
#' @param data data.frame use for gompertz_mle
#' @param object gompertz_mle object
#' @param covar covariate of interest
#' @param death_var death age variable
#' @param xlim x-limits for figure
#'
#' @return a ggplot object
#'
#' @importFrom rlang := .data
#' @examples
#'
#' # Diagnostic plot to assess model fit. This diagnostic plot only works for one birth cohort
#' # and discrete covariates.
#' gompertztrunc::diagnostic_plot(object = gradient, data = sim_data_trunc, covar = "sex",
#'death_range = c(65, 85))
#'

diagnostic_plot <- function(data, object, covar, xlim =c(65, 110), death_var = "death_age") {

  ## give warning if data isn't a factor
  if (!(is.factor(data[[covar]]) | is.character(data[[covar]]))){
    stop('Covariate must be a factor or character variable')
  }

  ## make death age var
  data <- data %>%
    rename(death_age = !!death_var) %>%
    mutate(death_age = floor(death_age))

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
  hx <- hx_calc(b = b, M = M, x = 0:121+ 0.5) # 0.6158778 *
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
    filter(get(covar) == cov_levels[1]) %>%
    filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    summarize(n()) %>%
    as.numeric()

  ## multipled
  multiplier <- deaths_real/deaths_sim

  ## new dx and simulate deaths
  dx <- dx_calc(multiplier, lx)

  ## number of deaths
  deaths <- tibble(dx, death_age = 0:122) %>%
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
    hx <- hr * hx_calc(b = b, M = M, x = 0:121+0.5) # 0.6158778 *
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
      filter(get(covar) == cov) %>%
      filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
      summarize(n()) %>%
      as.numeric()

    ## multiplied
    multiplier <- deaths_real/deaths_sim

    ## new dx and simulate deaths
    dx <- dx_calc(multiplier, lx)

    deaths <- tibble(dx, death_age = 0:122) %>%
      filter(!is.na(dx))

    death_counts_modeled[[counter]] <- deaths %>%
      mutate(!!covar := covariates[[counter-1]])

  }

  ## death counts modeled
  death_counts_modeled <- bind_rows(death_counts_modeled) %>%
    rename(var=!!covar)

  ## create plot
  plot <- data %>%
    rename(var=!!covar) %>%
    filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%

    ggplot() +
    geom_histogram(aes(x = death_age), binwidth  = 1, color = "black", fill = "grey") + #  center = .499,
    cowplot::theme_cowplot() +
    geom_line(data = death_counts_modeled, aes(x = death_age, y = dx, color = "Modeled"), size = 1, linetype = "solid") +
    labs(x = "Death Age",
         y = "n",
         title = "") +
    scale_color_manual(name = "", values = c("Modeled" = "blue")) +
    theme(legend.position = "bottom", legend.key.width= unit(1.5, 'cm')) +
    labs(x = "Death Age",
         y = "n") +
    xlim(xlim) +
    facet_wrap(~var, scales = "free")

  ## return plot
  return(plot)
}
