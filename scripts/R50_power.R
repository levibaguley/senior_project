library(pwr)

# show the margins above and below the R50 point estimate
conf_margins <-
  R50_dat %>%
  mutate(across(c(lower, upper), ~ .x - R50, .names = "{col}_margin"))

# create plot of power curves
power_plot <-
  # the effect sizes that will be ploted
  seq(.5, 2.5, .5) %>%
  # run power calculations for each effect size and put it in a dataframe
  map_dfr(~ pwr.t.test(n = seq(2, 50, .05),
                       type = "two.sample",
                       d = .x,
                       alternative = "greater") %>%
            tidy() %>%
            # keep track what effect size the calculations are for
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
       subtitle = "Effect size in this sample was about 2.5") +
  theme_bw()
