library(tidyverse)
library(parsnip)
library(magrittr)

source(here::here("R50_functions.R"))

sample <-
  read_csv("radarwindmillsampledata.csv" %>% here::here()) %>%
  rename(detection = response,
         turbines = windmills)

R50_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(as.factor(detection) ~ range + turbines, data = sample)

R50_dat <-
  tibble(turbines = 0:1) %>%
  mutate(R50 = range_from_prob(0.5, turbines, R50_mod)) %>%
  left_join(solve_R50_conf(R50_mod, res = 1000), by = "turbines")

plot_dat <-
  tibble(turbines = c(rep(0, 1000), rep(1, 1000))) %>%
  mutate(range = rep(seq(0, 40, length.out = 1000), 2)) %>%
  bind_cols(
    predict(R50_mod, new_data = ., type = 'prob'),
    predict(R50_mod, new_data = ., type = 'conf_int')
  ) %>%
  select(!ends_with("_0"))

sample %>%
  ggplot(aes(range, detection, color = as.factor(turbines))) +
  geom_jitter(width = 0, height = .02) +
  geom_line(aes(y = .pred_1),       plot_dat, size = 1) +
  geom_line(aes(y = .pred_lower_1), plot_dat, linetype = 2) +
  geom_line(aes(y = .pred_upper_1), plot_dat, linetype = 2) +
  coord_cartesian(xlim = c(0, NA)) +
  geom_point(aes(R50,   .5), R50_dat) +
  geom_point(aes(lower, .5), R50_dat) +
  geom_point(aes(upper, .5), R50_dat) +
  theme_bw()
