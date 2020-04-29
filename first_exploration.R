library(tidyverse)
library(tidymodels)

sample <- read_csv("radarwindmillsampledata.csv" %>% here::here()) %>%
  mutate(detection = if_else(response == 0, "not_detected", "detected"),
         turbines = if_else(windmills == 0, "not_present", "present"),
         .keep = "unused") %>%
  mutate(across(c("detection", "turbines"), as.factor))

sample %>%
ggplot(aes(as.numeric(detection =="detected"), range, color = turbines)) +
  # geom_point()
  geom_jitter(width = .02) +
  geom_smooth(method = "glm",
               method.args = list(family = "binomial"),
              orientation = "y")

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

log_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(detection ~ range + turbines,
      data=sample)


coefs <- log_fit$fit %>% coef()


prob <- function(range, turbinds, coefs) {
  return(1 /
           (1 + exp(-(coefs[1] + coefs[2] * range + coefs[3] * turbinds))))

}

prob(20, T, coefs)
