#' Create diagnostic plots
#'
#' Compare empirical and modeled distribution of ages of death within a cohort. Only
#' works with a single discrete covariate and a single cohort.
#'
#' @param data data used to create gompertz_mle object
#' @param object gompertz_mle object
#' @param covar covariate of interest
#' @param death_var death age variable
#' @param byear_var birth year/cohort variable
#' @param xlim x-limits for figure
#'
#' @return a ggplot object
#'
#' @importFrom rlang := .data
#'
#' @examples
#' # Create a single-cohort data set
#' numident_c1920 <- numident_demo %>% dplyr::filter(byear == 1920) %>%
#' dplyr::mutate(finished_hs = as.factor(educ_yrs >= 12))
#'
#' # Run gompertz_mle()
#' gradient <- gompertztrunc::gompertz_mle(formula = death_age ~ finished_hs,
#' left_trunc = 1988, right_trunc = 2005, data = numident_c1920)
#'
#' # Create diagnostic histogram plot using model outcome
#' gompertztrunc::diagnostic_plot(object = gradient, data = numident_c1920,
#' covar = "finished_hs", xlim = c(60, 95))
#'
#' @export

diagnostic_plot <- function(data, object, covar, death_var = "death_age", byear_var = "byear",
                            xlim =c(65, 110)) {

  ## abort if covariate isn't a factor or string
  if (!(is.factor(data[[covar]]) | is.character(data[[covar]]))){
    stop('Covariate must be a factor or character variable')
  }

  ## abort if data contains multiple cohorts
  if(length(unique(data[[byear_var]])) > 1) {
    stop('Data and model can only include a single birth cohort')
  }

  ## make death age var
  death_age <-  NULL
  data <- data %>%
    dplyr::rename(death_age = !!death_var) %>%
    dplyr::mutate(death_age = floor(death_age))

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
  parameter <- NULL
  hr <- object$results %>%
    dplyr::filter(parameter == !!covar) %>%
    dplyr::select(hr) %>% as.numeric()

  ## calculate lifetable quantities (modeled)
  hx <- hx_calc(b = b, M = M, x = 0:121+ 0.5) # 0.6158778 *
  lx <- c(1, lx_calc(hx))
  dx <- dx_calc(1, lx)

  ## calculate number of deaths simulate
  deaths <- tibble::tibble(dx, death_age = 0:122) %>%
    dplyr::filter(!is.na(dx))

  ## calculate bounds dropping endpoints
  bounds <- data %>% dplyr::summarize(min(death_age) + 1, max(death_age) - 1) %>%
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
  deaths <- tibble::tibble(dx, death_age = 0:122) %>%
    dplyr::filter(!is.na(dx))

  death_counts_modeled[[counter]] <- deaths %>%
    dplyr::mutate(!!covar := cov_levels[1])

  ## get covariates
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
    hx <- hr * hx_calc(b = b, M = M, x = 0:121+0.5) # 0.6158778 *
    lx <- c(1, lx_calc(hx))
    dx <- dx_calc(1, lx)

    ## calculate number of deaths simulate
    deaths <- tibble::tibble(dx, death_age = 0:122) %>%
      dplyr::filter(!is.na(dx))

    ## calculate bounds dropping endpoints
    bounds <- data %>% dplyr::summarize(min(death_age) + 1, max(death_age) - 1) %>%
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

    deaths <- tibble::tibble(dx, death_age = 0:122) %>%
      dplyr::filter(!is.na(dx))

    death_counts_modeled[[counter]] <- deaths %>%
      dplyr::mutate(!!covar := covariates[[counter-1]])

  }

  ## death counts modeled
  death_counts_modeled <- dplyr::bind_rows(death_counts_modeled) %>%
    dplyr::rename(var=!!covar)

  ## create plot
  plot <- data %>%
    dplyr::rename(var=!!covar) %>%
    dplyr::filter(death_age >= bounds[[1]] & death_age <= bounds[[2]]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = death_age), binwidth  = 1, color = "black",
                            fill = "grey", na.rm=TRUE) + #  center = .499,
    cowplot::theme_cowplot() +
    ggplot2::geom_line(data = death_counts_modeled, ggplot2::aes(x = death_age, y = dx,color = "Modeled"),
                       size = 1, linetype = "solid", na.rm=TRUE) +
    ggplot2::labs(x = "Death Age",
         y = "n",
         title = "") +
    ggplot2::scale_color_manual(name = "", values = c("Modeled" = "blue")) +
    ggplot2::theme(legend.position = "bottom", legend.key.width= grid::unit(1.5, 'cm')) +
    ggplot2::labs(x = "Death Age",
         y = "n") +
    ggplot2::xlim(xlim) +
    ggplot2::facet_wrap(~var, scales = "free")

  ## return plot
  return(plot)
}
