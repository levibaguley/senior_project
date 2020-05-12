library(tidyverse)
#library(tidymodels)
library(parsnip)


sample <-
  read_csv("radarwindmillsampledata.csv" %>% here::here()) %>%
  mutate(detection =
             if_else(response == 0, "not_detected", "detected") %>%
             factor(levels = c("not_detected", "detected")),
         turbines =
             if_else(windmills == 0, "not_present", "present") %>%
             factor(levels = c("not_present", "present")),
         .keep = "unused")

#' Helper fuction for finding R_50 from the logistic model
#' @param range The range of the target. Main input for `uniroot()`
#' @param turbines Are there turbinds? 1/0 or T/F
#' @param log_fit The fit of the regression as a parsnip model object
#' @param prob The probability that determines where the root is and
#' the corresponding `range` that will be found in `getR_50()`
#' therefore the default is .5.
prob_root <- function(range, turbines, log_fit, prob = .5) {
  coefs <- log_fit$fit %>% coef() %>% unname()
  return(1 /
           (1 + exp(-(coefs[1] + coefs[2] * range +
                        coefs[3] * turbines)))
        - prob)
}

#' Finds R_50 for when turbines are not_present and present
#' @param log_fit The fit of the regression as a parsnip model object
getR_50 <- function(fit, prob=.5) {

  R_50 <- map_dbl(F:T,
                  ~ uniroot(prob_root,
                            interval = range(fit$fit$data$range),
                            prob = prob,
                            log_fit = fit,
                            turbines = .x) %>%
                    `$`(root))
  return(tibble(turbines= c("not_present", "present"),
                range = R_50))
}


R_50_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(detection ~ range + turbines, data=sample)

R_50_tibble <- R_50_mod %>%
  getR_50()


R_50_mod %>%
  predict.model_fit(R_50_tibble,
                    type = 'conf_int')  %>%
  # select(.pred_lower_detected, .pred_upper_detected) %>%
  mutate(lower = getR_50(R_50_mod, .pred_lower_detected))


sample %>%
ggplot(aes(as.numeric(detection =="detected"), range, color = turbines)) +
  geom_jitter(width = .02) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              orientation = "y") +
  theme_bw()


sample %>%
  ggplot(aes(range,as.numeric(detection =="detected"), color = turbines)) +
  geom_jitter(height = .02) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial")) +
  theme_bw()












# sample %>%
#   filter(detected == 1) %>%
#   group_by(windmills) %>%
#   summarise(q_2 = quantile(range, .5))
#
# sample %>%
# filter(detected == 1) %>%
# ggplot(aes(range)) +
#   geom_histogram(bins = 10) +
#   facet_wrap(~windmills, ncol = 1)
