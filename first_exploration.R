library(tidyverse)
library(tidymodels)

sample <-
  read_csv("radarwindmillsampledata.csv" %>% here::here()) %>%
  mutate(detection =
             if_else(response == 0, "not_detected", "detected") %>%
             factor(levels = c("not_detected", "detected")),
         turbines =
             if_else(windmills == 0, "not_present", "present") %>%
             factor(levels = c("not_present", "present")),
         .keep = "unused")

prob_root <- function(range, turbines, reg_fit, prob = .5) {
  coefs <- reg_fit$fit %>% coef() %>% unname()
  return(1 /
           (1 + exp(-(coefs[1] + coefs[2] * range +
                        coefs[3] * turbines)))
        - prob)
}

getR_50 <- function(fit) {
  map_dbl(0:1,
          ~ uniroot(prob_root,
                    interval = range(fit$fit$data$range),
                    reg_fit = fit,
                    turbines = .x) %>%
            `$`(root)) %>%
    set_names(c("R_50.not_present", "R_50.present"))
}

logistic_reg() %>%
  set_engine("glm") %>%
  fit(detection ~ range + turbines,
      data=sample) %>%
  getR_50()


sample %>%
ggplot(aes(as.numeric(detection =="detected"), range, color = turbines)) +
  geom_jitter(width = .02) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              orientation = "y") +
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
