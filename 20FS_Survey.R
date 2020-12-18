library(readr)
google_form <- read_csv("form.csv",
                        col_names = TRUE, col_types = cols(X7 = col_skip(),
                                                           X11 = col_skip()), locale = locale(encoding = "EUC-KR"))

library(dplyr)
library(tidyr)
library(reshape2)
library(ggforce)
google_form %>%
  filter(Experience != "No") %>%
  select(Age, Occupation, Times, Purpose, Mean_DIST, Max_DIST, REWD_DIST) %>%
  mutate(
    Age = factor(
      case_when(
        Age == "Youth" ~ "Youth",
        Age == "Twenties" ~ "Twenties",
        Age == "Thirties" ~ "Thirties",
        Age == "Fourties" ~ "Fourties",
        Age == "Senior" ~ "Senior"
      ),
      levels = c("Youth", "Twenties", "Thirties", "Fourties", "Senior")
    ),
    Occupation = factor(
      case_when(
        Occupation == "Unemployed" ~ "Unemployed",
        Occupation == "Employed" ~ "Employed",
        Occupation == "Business" ~ "Business",
        Occupation == "Student" ~ "Student",
        Occupation == "Others" ~ "Others"#######################################
      ),
      levels = c("Unemployed", "Employed", "Business", "Student", "Others")
    ),
    Times = factor(
      case_when(
        Times == "under2" ~ "under2",
        Times == "2~3" ~ "2~3",
        Times == "4~5" ~ "4~5",
        Times == "over6" ~ "over6"
      ),
      levels = c("under2", "2~3", "4~5", "over6")
    ),
    Purpose = factor(
      case_when(
        Purpose == "Leisure" ~ "Leisure",
        Purpose == "School" ~ "School",
        Purpose == "Work" ~ "Work",
        Purpose == "Others" ~ "Others" #########################################
      ),
      levels = c("Leisure", "School", "Work", "Others")
    ),
    Mean_DIST = factor(
      case_when(
        Mean_DIST == "under1" ~ "under1",
        Mean_DIST == "1~3" ~ "1~3",
        Mean_DIST == "3~5" ~ "3~5",
        Mean_DIST == "5~10" ~ "5~10",
        Mean_DIST == "10~20" ~ "10~20",
        Mean_DIST == "over20" ~ "over20"
      ),
      levels = c("under1", "1~3", "3~5", "5~10", "10~20", "over20")
    ),
    Max_DIST = factor(
      case_when(
        Max_DIST == "no Interest" ~ "no Interest",
        Max_DIST == "under1" ~ "under1",
        Max_DIST == "1~3" ~ "1~3",
        Max_DIST == "3~5" ~ "3~5",
        Max_DIST == "5~10" ~ "5~10",
        Max_DIST == "10~20" ~ "10~20",
        Max_DIST == "over20" ~ "over20"
      ),
      levels = c("no Interest", "under1", "1~3", "3~5", "5~10", "10~20", "over20")
    ),
    REWD_DIST = factor(
      case_when(
        REWD_DIST == "no Interest" ~ "no Interest",
        REWD_DIST == "under1" ~ "under1",
        REWD_DIST == "1~3" ~ "1~3",
        REWD_DIST == "3~5" ~ "3~5",
        REWD_DIST == "5~10" ~ "5~10",
        REWD_DIST == "10~20" ~ "10~20"
      ),
      levels = c("no Interest", "under1", "1~3", "3~5", "5~10", "10~20")
    )
  ) -> pm_survey

pm_survey$id <- 1:nrow(pm_survey)
pm_survey$count <- 1
pm_survey <- gather_set_data(pm_survey, 1:7)####################################
pm_survey$x <- factor(pm_survey$x, levels = c("Age", "Occupation", "Times", "Purpose", "Mean_DIST", "Max_DIST", "REWD_DIST"))

ggplot(pm_survey, aes(x, id = id, split = y, value = count)) +
  geom_parallel_sets(aes(fill = Age), alpha = 0.5, axis.width = 0.13) +#########
  geom_parallel_sets_axes(axis.width = 0.1, fill = "grey80", color = "grey80") +
  geom_parallel_sets_labels(
    color = 'black',
    size = 10/.pt,
    angle = 90
  ) +
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0.2)
  ) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0))+
  scale_fill_manual(
    values = c(Thirties = "#56B4E9D0", Twenties = "#F0E442D0" , Senior = "#CC79A7D0"),####################
    guide = "none"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(14, 1.5, 2, 1.5))

