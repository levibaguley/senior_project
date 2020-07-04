library(pwr)

R50_dat %>%
  rename(range = R50) %>%
  select(-lower, -upper) %>%
  bind_cols(
    predict(R50_mod, new_data = ., type = "prob"),
    predict(R50_mod, new_data = ., type = "conf_int"),
    predict(
      R50_mod,
      new_data = .,
      type = "raw",
      opts = list(se.fit = TRUE, type = "response")
    )
  ) %>%
  select(!c(ends_with("_0"), fit, residual.scale)) %>%
  rename(
    prob = .pred_1,
    lower = .pred_lower_1,
    upper = .pred_upper_1,
    std_err = se.fit
  ) %>%
  mutate(across(c(lower, upper),
    ~ (.x - .5) / std_err,
    .names = "num_se_{col}"
  ))

R50_dat %>%
  mutate(across(c(lower, upper), ~ .x - R50, .names = "{col}_diff"))

power_plot <-
  seq(.5, 2.5, .5) %>%
  map_dfr(
    ~ pwr.t.test(
      n = seq(2, 50, .05),
      type = "two.sample",
      d = .x,
      alternative = "greater"
    ) %>%
      tidy() %>%
      mutate(effect_size = as.factor(.x))
  ) %>%
  mutate(effect_size = fct_rev(effect_size)) %>%
  ggplot(aes(n, power, color = effect_size)) +
  geom_line(size = .75) +
  labs(
    y = "Power",
    title = expression(paste(
      "Power of finding differences in ",
      R[50],
      " under different turbine conditions"
    )),
    x = "Sample size (for each condition)",
    color = "Effect Size",
    subtitle = expression("Effect size in this sample was" %~~% 2.5)
  ) +
  theme_bw()
