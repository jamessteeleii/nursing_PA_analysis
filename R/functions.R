read_data <- function() {
  data <- read_csv("data/nursing_PA_data.csv")
  
}

plot_PA_importance <- function(data) {
  data |>
    filter(!is.na(q3)) |>
    count(q3)|>
    mutate(pct = prop.table(n)) |>
    add_row(q3 = "Strongly disagree") |>
    mutate(q3 = factor(q3, 
                       levels = c("Strongly disagree",
                                  "Disagree",
                                  "Neutral",
                                  "Agree",
                                  "Strongly agree",
                                  "Other",
                                  NA)
    )
    ) |>
    ggplot(aes(x=q3)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=scales::percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
    geom_label(aes(y=pct,label=glue::glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "Advising patients about physical activity is an important part of a nurse’s job") +
    theme_classic()
}

plot_guideline_confidence <- function(data) {
  data |>
    select(q6, q7, q8) |>
    pivot_longer(c(q6, q7, q8)) |>
    filter(!is.na(value)) |>
    count(name, value)|>
    group_by(name) |>
    mutate(pct = prop.table(n)) |>
    # add_row(q3 = "Strongly disagree") |>
    mutate(value = factor(value, 
                          levels = c("Strongly Disagree",
                                     "Disagree",
                                     "Neutral",
                                     "Agree",
                                     "Strongly agree",
                                     "Unsure")
    )
    ) |>
    mutate(name = case_when(name == "q6" ~ "Children and young people (under 19 years)",
                            name == "q7" ~ "Adults (19-64 years)",
                            name == "q8" ~ "Older Adults (65+ years)")) |>
    ggplot(aes(x=value)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=scales::percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
    geom_label(aes(y=pct,label=glue::glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "I am confident I know the CMO's (Chief Medical Officer) physical activity guidelines for:") +
    facet_grid(name~.) +
    theme_classic()
}

plot_knowledge_dose_adult <- function(data) {
  mod_mins_plot <- data |>
    select(q9) |>
    pivot_longer(q9) |>
    filter(!is.na(value) & 
             value <= 500) |>
    mutate(name = case_when(name == "q9" ~ "Over a week, activity should add up to at least __ minutes of moderate intensity activity")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue::glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() +
    theme(axis.text.x = element_text(size=11,colour = "black"))
  
  
  bout_mins_plot <- data |>
    select(q10) |>
    pivot_longer(q10) |>
    filter(!is.na(value)) |>
    mutate(name = case_when(name == "q10" ~ "Bouts of activity should last at least __ minutes")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue::glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() +
    theme(axis.text.x = element_text(size=11,colour = "black"))
  
  
  vig_mins_plot <- data |>
    select(q11) |>
    pivot_longer(q11) |>
    filter(!is.na(value)) |>
    mutate(name = case_when(name == "q11" ~ "Alternatively, comparable benefits can be achieved through __ minutes of vigorous intensity activity spread across the week")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue::glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() +
    theme(axis.text.x = element_text(size=11,colour = "black"))
  
  strength_days_plot <- data |>
    select(q12) |>
    pivot_longer(q12) |>
    filter(!is.na(value) &
             value <= 7) |>
    mutate(name = case_when(name == "q12" ~ "Adults should also undertake physical activity to improve muscle strength on at least __ days a week")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue::glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Days") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() +
    theme(axis.text.x = element_text(size=11,colour = "black"))
  
  mod_mins_plot + bout_mins_plot + vig_mins_plot + strength_days_plot
}

plot_knowledge_other_adult <- function(data) {
  data |>
    select(q14:q18) |>
    pivot_longer(c(q14:q18)) |>
    filter(!is.na(value)) |>
    mutate(name = case_when(name == "q14" ~ "Should be active daily",
                            name == "q15" ~ "Activities such as walking do not contribute to MVPA (moderate to vigorous physical activity",
                            name == "q16" ~ "Conduct flexibility training at least one day per week",
                            name == "q17" ~ "Extended periods of sedentary activities should be limited ",
                            name == "q18" ~ "Overweight adults should aim for short bouts of high intensity exercise to facilitate weight loss"
    )) |>
    group_by(name) |>
    count(name, value)|>
    mutate(pct = prop.table(n)) |>
    ggplot(aes(x=name, fill=value)) +
    geom_col(aes(y=pct), position = "fill") +
    geom_label(aes(y=pct, label=glue::glue("n = {n}")),
               position = "fill",     
               vjust = 1.5,    # nudge above below bar
               size = 3, show.legend = FALSE) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    scale_y_continuous(name = "Percent", labels=scales::percent) +
    scale_fill_manual(values = c("grey60","lightgrey")) +
    labs(x = "",
         y = "Percent",
         fill = "") +
    theme_classic()
}

plot_knowledge_older_adult <- function(data) {
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
}