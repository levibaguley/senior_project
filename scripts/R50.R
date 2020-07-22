library(tidyverse)
library(parsnip)
library(here)

# get the function that will solve for R50
source(here("scripts/solve_R50.R"))

sample <-
  read_csv(here("radarwindmillsampledata.csv")) %>%
  rename(detection = response,
         turbines = windmills)

# create `parsnip` logistic model
R50_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(as.factor(detection) ~ range + turbines, data = sample)

# find the R50 estimates and confidence intervals
R50_dat <-
  tibble(turbines = 0:1) %>%
  mutate(R50   = solve_R50(R50_mod, turbines, "pred"),
         lower = solve_R50(R50_mod, turbines, "lower"),
         upper = solve_R50(R50_mod, turbines, "upper"))

# create data to display the logistic model and the confidence intervals
plot_dat <-
  tibble(turbines = rep(0:1, each=1000)) %>%
  mutate(range = rep(seq(0, 40, length.out = 1000), 2)) %>%
  bind_cols(predict(R50_mod, new_data = ., type = "prob"),
            predict(R50_mod, new_data = ., type = "conf_int")) %>%
  select(!ends_with("_0"))

R50_plot <-
  plot_dat %>%
  ggplot(aes(range, color = as.factor(turbines))) +
  # the main logistic curves
  geom_line(aes(y = .pred_1)) +
  # the lower bounds
  geom_line(aes(y = .pred_lower_1), linetype = 2) +
  # the upper bounds
  geom_line(aes(y = .pred_upper_1), linetype = 2) +
  # the data points
  geom_jitter(aes(y = detection),
              data = sample, width = 0, height = .02) +
  # R50 estimates
  geom_point(aes(R50, .5, shape = "R50"),
             data = R50_dat) +
  scale_shape_manual(NULL,
                     values = c("R50" = 8),
                     labels = expression(R[50])) +
  scale_color_brewer(name = "Turbines",
                     labels = c("None", "Present"),
                     palette = "Set1",
                     direction = -1) +
  labs(title = expression(paste("Presence of Wind Turbines Reduces ", R[50])),
       x = "Range (data miles)",
       y = "Probability Target is Detected") +
  theme_bw()
