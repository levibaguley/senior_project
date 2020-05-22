library(tidyverse)
#library(tidymodels)
library(parsnip)
library(magrittr)


sample <-
  read_csv("radarwindmillsampledata.csv" %>% here::here()) %>%
  rename(detection = response,
         turbines = windmills)

#' Helper fuction for finding a range for a given probability from the logistic model
#' @param range The range of the target. Main input for `uniroot()`
#' @param turbines Are there turbinds? 1/0 or T/F
#' @param log_fit The fit of the regression as a parsnip model object
#' @param prob The probability that determines where the root is and
#' the corresponding `range` that will be found in `range_from_prob()`
prob_root <- function(range, turbines, log_fit, prob) {
  coefs <- log_fit$fit %>% coef() %>% unname()
  return(1 /
           (1 + exp(-(coefs[1] + coefs[2] * range +
                        coefs[3] * turbines)))
         - prob)
}

#' Finds the estimated range of the target for a given probability that the target will be detected.
#' @param log_fit The fit of the regression as a parsnip model object
#' @param prob The probability that the taget will be detected.
#' @param turbines Are there turbinds? 1/0 or T/F
range_from_prob <- function(prob, turbines, fit) {
  map2_dbl(turbines,
           prob,
           ~ uniroot(prob_root,
                     interval = range(fit$fit$data$range),
                     prob = .y,
                     log_fit = fit,
                     turbines = .x) %>%
             `$`(root))
}

R50_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(as.factor(detection) ~ range + turbines, data = sample)

R50_tibble <-
  tibble(turbines = 0:1) %>%
  mutate(range = range_from_prob(0.5, turbines, R50_mod)) %>%
  bind_cols(predict(object = R50_mod,
                    new_data = .,
                    type = 'conf_int')) %>%
  mutate(lower_R50 = range_from_prob(.pred_upper_1, turbines, R50_mod),
         upper_R50 = range_from_prob(.pred_lower_1, turbines, R50_mod)) %>%
  select(!starts_with(".pred")) %>%
  rename(R50 = range)

plot_dat <-
  tibble(turbines = c(rep(0, 1000), rep(1, 1000))) %>%
  mutate(range = rep(seq(0,40, length.out = 1000), 2)) %>%
  bind_cols(predict(R50_mod, new_data = ., type = 'prob')) %>%
  bind_cols(predict(R50_mod, new_data = ., type = 'conf_int')) %>%
  select(!ends_with("_0"))


sample %>%
  ggplot(aes(range, detection, color = as.factor(turbines))) +
  geom_jitter(width = 0, height = .02) +
  geom_line(aes(y=.pred_1),       plot_dat, size=1) +
  geom_line(aes(y=.pred_lower_1), plot_dat, linetype = 2) +
  geom_line(aes(y=.pred_upper_1), plot_dat, linetype = 2) +
  coord_cartesian(xlim = c(0,40)) +
  # geom_point(aes(lower_R50, .5), R50_tibble) +
  # geom_point(aes(upper_R50, .5), R50_tibble) +
  # geom_point(aes(R50, .5), R50_tibble) +
  # geom_segment(aes(x =  lower_R50, y =    .5,
  #                  xend=upper_R50, yend = .5),
  #              R50_tibble) +
  theme_bw()
# coord_flip()



name <- function(dat, conf_type) {

  tibble(turbines = c(rep(0, 1000), rep(1, 1000))) %>%
    mutate(range = rep(seq(0,40, length.out = 1000), 2)) %>%
    bind_cols(predict(R50_mod, new_data = ., type = 'prob')) %>%
    bind_cols(predict(R50_mod, new_data = ., type = 'conf_int')) %>%
    select(!ends_with("_0"))


}

get_conf <- function(dat, conf_type) {

  conf_type %<>%
    {str_glue(".pred_{.}_1")}


  dat %>%
    group_by(turbines) %>%
    select(turbines, range, as.symbol(conf_type)) %>%
    slice_min(abs(.pred_lower_1-.5), 1) %>%
    select(turbines, as.symbol(conf_type) = range) %>%
    ungroup()
}

plot_dat %>%
get_conf("upper")



