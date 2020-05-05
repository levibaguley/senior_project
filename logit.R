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


lm(log(range/(1- range)) ~ detection=='detected', data = sample)




