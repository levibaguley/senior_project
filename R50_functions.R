
require(dplyr)
require(parsnip)
require(stringr)

#' Finds the estimated range of the target for a given probability that the target will be detected.
#' @param log_fit The fit of the regression as a parsnip model object
#' @param prob The probability that the taget will be detected.
#' @param turbines Are there turbinds? 1/0 or T/F
range_from_prob <- function(prob, turbines, fit) {

  prob_root <- function(range, turbines, log_fit, prob) {
    coefs <- log_fit$fit %>% coef() %>% unname()
    return(1 / (1 + exp(-(coefs[1] + coefs[2] * range +
                            coefs[3] * turbines)))
           - prob)
  }

  map2_dbl(turbines,
           prob,
           ~ uniroot(prob_root,
                     interval = range(fit$fit$data$range),
                     prob = .y,
                     log_fit = fit,
                     turbines = .x) %>%
             `$`(root))
}


solve_R50_conf <- function(R50_mod, res=1000, interval = c(0,40)) {

  get_conf <- function(dat, conf_type) {
    conf_col <- str_glue(".pred_{conf_type}_1") %>% sym()

    dat %>%
      group_by(turbines) %>%
      select(turbines, range, !!conf_col) %>%
      slice_min(order_by=abs(!!conf_col - .5), n=1) %>%
      select(turbines, "{conf_type}" := range) %>%
      ungroup()
  }

  solve_dat <- tibble(turbines = c(rep(0, res), rep(1, res))) %>%
    mutate(range = rep(seq(interval[1], interval[2], length.out = res), 2)) %>%
    bind_cols(
      suppressMessages(predict(R50_mod, new_data = ., type = 'prob')),
      suppressMessages(predict(R50_mod, new_data = ., type = 'conf_int'))) %>%
    select(!ends_with("_0"))


  return(get_conf(solve_dat, "lower") %>%
           left_join(get_conf(solve_dat, "upper"), by = "turbines"))
}
