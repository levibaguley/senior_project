library(pwr)

R50_dat %>%
  mutate(across(c(lower, upper), ~ .x - R50, .names = "{col}_margin"))

power_plot <-
  seq(.5, 2.5, .5) %>%
  map_dfr(~ pwr.t.test(n = seq(2, 50, .05),
                       type = "two.sample",
                       d = .x,
                       alternative = "greater") %>%
            tidy() %>%
            mutate(effect_size = as.factor(.x))) %>%
  mutate(effect_size = fct_rev(effect_size)) %>%
  ggplot(aes(n, power, color = effect_size)) +
  geom_line(size = .75) +
  labs(y = "Power",
       title = expression(paste("Power of finding differences in ",
                                R[50],
                                " under different turbine conditions")),
       x = "Sample size (for each condition)",
       color = "Effect Size",
       subtitle = expression("Effect size in this sample was" %~%2.5)) +
  theme_bw()
