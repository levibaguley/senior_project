library(tidyverse)
library(tidymodels)


# sample <-
  read_csv("radarwindmillsampledata.csv" %>% here::here()) %>%
  mutate(detection =
           if_else(response == 0, "not_detected", "detected") %>%
           factor(levels = c("not_detected", "detected")),
         turbines =
           if_else(windmills == 0, "not_present", "present") %>%
           factor(levels = c("not_present", "present")),
         .keep = "unused") %>%
slice_sample(prop = .5)

dat <- tibble(
    x = c(rnorm(20, 2.5,.5), rnorm(20, 7.5,.5)),
    pass = sample(0:1, 40, T),
    class = c(rep("A", 20), rep("B", 20)))



x = c(rnorm(50, 2.5), rnorm(50,2.5))

z = 1 + 2*x       # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(length(x),1,pr)

dat = tibble(y=y, x1=x1)

dat %>%
ggplot(aes(x,y)) +
  geom_jitter(height = .02) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial")) +
  theme_bw()

"A":"B"
logistic_reg() %>%
  set_engine("glm") %>%
  fit(detection ~ range + turbines,
      data=sample)


