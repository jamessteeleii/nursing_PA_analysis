library(tidyverse)

library(patchwork)

data <- read_csv("data/nursing_PA_data.csv")
 
data |>
  select(q19) |>
  separate(q19, into = rep(letters), sep = ",") |>
  mutate(a = case_when(a == "Be advised that any amount of physical activity is better than none" ~
                         "Be advised that any amount of physical activity is better than none,  and more activity provides greater health benefits",
                       TRUE ~ as.character(a)),
         b = case_when(b == " and more activity provides greater health benefits" ~ NA,
                       TRUE ~ as.character(b))) |>
  select(a:e) |>
  pivot_longer(c(a:e)) |>
  filter(!is.na(value)) |>
  count(value)|>
  mutate(pct = n/179,
         value = factor(value, 
                        levels = c("Be advised that any amount of physical activity is better than none,  and more activity provides greater health benefits",
                                   "Incorporate activities to improve balance and coordination on at least two days a week if at risk of falls",
                                   "Only engage in physical activity if they are used to exercise or used to be active",
                                   "Refrain from physical activity in presence of chronic disease such as heart disease or arthritis",
                                   "None of the above"))) |>
  ggplot(aes(x=value)) +
  geom_col(aes(y = pct)) + 
  scale_y_continuous(name = "Percent", labels=scales::percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  geom_label(aes(y=pct,label=glue::glue("n = {n}")),
             position = position_dodge(width = .9),    # move to center of bars
             vjust = 0.5,    # nudge above top of bar
             size = 3) +
  labs(x = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size=11,colour = "black"))

