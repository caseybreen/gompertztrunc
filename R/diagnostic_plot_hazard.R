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
#'
#' # Create a single-cohort data set
#' numident_c1920 <- numident_demo %>% dplyr::filter(byear == 1920) %>%
#' dplyr::mutate(finished_hs = as.factor(educ_yrs >= 12))
#'
#' # Run gompertz_mle()
#' gradient <- gompertztrunc::gompertz_mle(formula = death_age ~ finished_hs,
#' left_trunc = 1988, right_trunc = 2005, data = numident_c1920)
#'
#' # Create diagnostic hazards plot using model outcome
#' gompertztrunc::diagnostic_plot_hazard(object = gradient, data = numident_c1920,
#' covar = "finished_hs", xlim = c(60, 95))
#' @export

diagnostic_plot_hazard <- function(data, object, death_var = "death_age", covar, xlim = c(65, 110)) {


  ## make death age var
  death_age <- NULL
  data <- data %>%
    dplyr::rename(death_age = !!death_var) %>%
    dplyr::mutate(death_age = floor(death_age)) # %>% filter(death_age %in% c((min(death_age)+1):(max(death_age)-1)))

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
    dplyr::filter(parameter == !!covar) %>%
    dplyr::select(hr) %>% as.numeric()

  ## calculate lifetable quantities (modeled)
  hx <- hx_calc(b = b, M = M, x = 0:121 + 0.5) # use midpoints because we're trying to calculate total deaths between x and x + 1
  lx <- c(1, lx_calc(hx))
  dx <- dx_calc(1, lx)

  ## calculate number of deaths simulate
  deaths <- tibble::tibble(dx, death_age = 0:122) %>%
    dplyr::filter(!is.na(dx))

  ## calculate bounds dropping endpoints
  bounds <- data %>%
    dplyr::summarize(min(death_age), max(death_age) ) %>%
    as.numeric()

  ## calculate multiplier
  deaths_sim <- deaths %>%
    dplyr::filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    dplyr::summarize(sum(dx)) %>%
    as.numeric()

  ## calculate deaths real
  deaths_real <- data %>%
    dplyr::filter(get(covar) == cov_levels[1]) %>%
    dplyr::filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    dplyr::summarize(dplyr::n()) %>%
    as.numeric()

  ## multipled
  multiplier <- deaths_real/deaths_sim

  ## new dx and simulate deaths
  dx <- dx_calc(multiplier, lx)

  ## number of deaths
  deaths <- tibble::tibble(dx, hx = c(hx, NA), death_age = 0:122) %>%
    dplyr::filter(!is.na(dx))

  death_counts_modeled[[counter]] <- deaths %>%
    dplyr::mutate(!!covar := cov_levels[1])

  ## get covariates
  parameter <- NULL
  covariates <- stringr::str_remove(object$results$parameter[3:length(object$results$parameter)], pattern = covar)

  for (cov in covariates) {

    ## move counter
    counter = counter + 1

    ## calculate hazard ratio
    hr <- object$results %>%
      dplyr::mutate(parameter = stringr::str_remove(parameter, pattern = covar)) %>%
      dplyr::filter(parameter == !!cov) %>%
      dplyr::select(hr) %>% as.numeric()

    ## calculate lifetable quantities (modeled)
    hx <- hr * hx_calc(b = b, M = M, x = 0:121 + 0.5) # 0.6158778 *
    lx <- c(1, lx_calc(hx))
    dx <- dx_calc(1, lx)

    ## calculate number of deaths simulate
    deaths <- tibble::tibble(dx, death_age = 0:122) %>%
      dplyr::filter(!is.na(dx))

    ## calculate bounds dropping endpoints
    bounds <- data %>% dplyr::summarize(min(death_age), max(death_age)) %>%
      as.numeric()

    ## calculate multiplier
    deaths_sim <- deaths %>%
      dplyr::filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
      dplyr::summarize(sum(dx)) %>%
      as.numeric()

    deaths_real <- data %>%
      dplyr::filter(get(covar) == cov) %>%
      dplyr::filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
      dplyr::summarize(dplyr::n()) %>%
      as.numeric()

    ## multiplied
    multiplier <- deaths_real/deaths_sim

    ## new dx and simulate deaths
    dx <- dx_calc(multiplier, lx)

    deaths <- tibble::tibble(dx, hx = c(hx, NA), death_age = 0:122) %>%
      dplyr::filter(!is.na(dx))

    death_counts_modeled[[counter]] <- deaths %>%
      dplyr::mutate(!!covar := covariates[[counter-1]])
  }

  ## death counts modeled
  death_counts_modeled <- dplyr::bind_rows(death_counts_modeled) %>%
    dplyr::rename(covar=!!covar)

  ## number of deaths
  deaths <- data %>%
    dplyr::rename(covar=!!covar) %>%
    dplyr::group_by(covar, death_age) %>%
    dplyr::summarize(dx = dplyr::n())

  ## calculate radix
  radix <- death_counts_modeled %>%
    dplyr::group_by(covar) %>%
    dplyr::filter(death_age %in% (max(deaths$death_age)+1):120) %>%
    dplyr::summarize(radix = sum(dx))

  obs_deaths <- NULL
  death_count <- deaths %>%
    dplyr::group_by(covar) %>%
    dplyr::summarize(obs_deaths = sum(dx))

  ## calculate lx
  type <- NULL
  death <- NULL
  hazards <- deaths %>%
    dplyr::left_join(radix, by = "covar") %>%
    dplyr::left_join(death_count, by = "covar") %>%
    dplyr::mutate(death = cumsum(dx)) %>%
    dplyr::mutate(lx = obs_deaths + radix - dplyr::lag(death)) %>% #
    dplyr::mutate(hx = -log(dplyr::lead(lx) / lx)) %>%
    dplyr::mutate(type = "observed")

  hazards_modeled <- death_counts_modeled %>%
    dplyr::mutate(type = "modeled")

  hazards_modeled_alt <- death_counts_modeled %>%
    dplyr::left_join(radix, by = "covar") %>%
    dplyr::mutate(death = cumsum(dx)) %>%
    dplyr::mutate(lx = radix - dplyr::lag(death)) %>% #
    dplyr::mutate(hx = dx/lx) %>%
    dplyr::mutate(hx = -log(dplyr::lead(lx) / lx)) %>%
    dplyr::mutate(type = "modeled")

  hr_ratio_plot <- hazards %>%
    dplyr::bind_rows(hazards_modeled) %>%
    dplyr::filter(dplyr::between(x = death_age, left = xlim[1], right = xlim[2])) %>%
    dplyr::select(death_age, covar, hx, type) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = death_age, y = log(hx), color = covar, linetype = type, alpha = type),  size = 1.3 ) +
    ggsci::scale_color_lancet() +
    cowplot::theme_cowplot() +
    ggplot2::labs(x = "Death Age",
         y = "Log Hazard rate") +
    ggplot2::scale_alpha_discrete(range = c(0.5, 1)) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())


  ## return plot
  return(hr_ratio_plot)

}

