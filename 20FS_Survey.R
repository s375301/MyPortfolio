library(readr)
google_form <- read_csv("form.csv",
                        col_names = TRUE, col_types = cols(X7 = col_skip(),
                                                           X11 = col_skip()), locale = locale(encoding = "EUC-KR"))

library(dplyr)
library(tidyr)
library(reshape2)
library(ggforce)
library(dviz.supp)
library(ggplot2)

##

google_form %>%
  filter(Experience == "Yes") %>%
  select(Age, Mean_DIST, Max_DIST, REWD_DIST) %>%
  table() %>%
  reshape2::melt() -> pm_survey

##

pm_survey <- gather_set_data(pm_survey, 1:4) ####################################
pm_survey$x <- factor(pm_survey$x, levels = c("Age", "Mean_DIST", "Max_DIST", "REWD_DIST"))

ggplot(pm_survey, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Age), alpha = 0.5, axis.width = 0.13) + #########
geom_parallel_sets_axes(axis.width = 0.1, fill = "grey80", color = "grey80") +
  geom_parallel_sets_labels(
    color = 'black',
    family = "Helvetica",
    size = 10/.pt,
    angle = 90
  ) +
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0.2)
  ) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0))+
  scale_fill_manual(
    values = c(Thirties = "#56B4E9D0", Twenties = "#F0E442D0" , Senior = "#CC79A7D0"), ####################
    guide = "none"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(14, 1.5, 2, 1.5))

