read_data <- function() {
  data <- read_csv("data/nursing_PA_data.csv")
  
}

make_plot_tiff <- function(plot, width, height, path) {
  ggsave(
    path,
    plot,
    width = width,
    height = height,
    device = "tiff",
    dpi = 300
  )
  
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
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "",
         title = "Advising patients about physical activity is an important part of a nurse’s job") +
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
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "",
         title = "I am confident I know the CMO's (Chief Medical Officer) physical activity guidelines for") +
    facet_grid(name~.) +
    theme_classic()
}

plot_knowledge_dose_adult <- function(data) {
  mod_mins_plot <- data |>
    select(q9) |>
    pivot_longer(q9) |>
    filter(!is.na(value) & 
             value <= 600) |>
    mutate(name = case_when(name == "q9" ~ "Over a week, activity should add up to at least __ minutes of moderate intensity activity")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.4,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic()
  
  
  bout_mins_plot <- data |>
    select(q10) |>
    pivot_longer(q10) |>
    filter(!is.na(value)) |>
    mutate(name = case_when(name == "q10" ~ "Bouts of activity should last at least __ minutes")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic()
  
  
  vig_mins_plot <- data |>
    select(q11) |>
    pivot_longer(q11) |>
    filter(!is.na(value)) |>
    mutate(name = case_when(name == "q11" ~ "Alternatively, comparable benefits can be achieved through __ minutes of vigorous intensity activity spread across the week")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic()
  
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
    geom_label(aes(y=median(value), label=glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Days") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() 
  
  (mod_mins_plot + bout_mins_plot + vig_mins_plot + strength_days_plot) +
    plot_annotation(title = "Please complete the UK CMO physical activity guidelines for adults aged 19-64")
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
    geom_label(aes(y=pct, label=glue("n = {n}")),
               position = "fill",     
               vjust = 1.5,    # nudge above below bar
               size = 3, show.legend = FALSE) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    scale_y_continuous(name = "Percent", labels=percent) +
    scale_fill_manual(values = c("grey60","lightgrey")) +
    labs(x = "",
         y = "Percent",
         fill = "",
         title = "Which of the following is/are also stated in the guidelines for adults aged 19-64?",
         subtitle = "Please tick all that apply") +
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
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "",
         title = "In addition to following adult guidelines, older adults (aged 65+ years) should also") +
    theme_classic()
}

plot_knowledge_dose_child <- function(data) {
  mod_mins_plot <- data |>
    select(q20) |>
    pivot_longer(q20) |>
    filter(!is.na(value) & 
             value <= 500) |>
    mutate(name = case_when(name == "q20" ~ "All children and young people should engage in “moderate to vigorous intensity” physical activity for at least __ minutes per day")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Minutes") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() 
  
  
  vig_days_plot <- data |>
    select(q21) |>
    pivot_longer(q21) |>
    filter(!is.na(value) &
             value <= 7) |>
    mutate(name = case_when(name == "q21" ~ "Vigorous intensity activities, including those that strengthen muscle and bone, should be incorporated at least __ days per week")) |>
    ggplot(aes(x=name, y=value)) +
    geom_violin(fill="grey60") + 
    geom_boxplot(outlier.color = NA, width = 0.2) +
    geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.5) +
    geom_label(aes(y=median(value), label=glue("Median = {median(value)} [IQR: {IQR(value)}]")),
               hjust = -0.5,
               size = 3) +
    labs(x = "",
         y = "Days") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_classic() 
  
  (mod_mins_plot + vig_days_plot) +
    plot_annotation(title = "Please complete the UK CMO physical activity guidelines for children aged 5-18")
  
}

plot_knowledge_other_child <- function(data) {
  data |>
    select(q22) |>
    separate(q22, into = rep(letters), sep = ",") |>
    select(a:e) |>
    pivot_longer(c(a:e)) |>
    filter(!is.na(value)) |>
    count(value)|>
    mutate(pct = n/179,
           value = factor(value, 
                          levels = c("Should minimise the amount of time spent being sedentary (sitting) for extended periods",
                                     "Activities such as walking do not contribute to moderate to vigorous physical activity (MVPA)",
                                     "Should avoid strength training before age 16 as may stunt growth rates",
                                     "Overweight children should aim for short bouts of high intensity exercise to facilitate weight loss",
                                     "None of the above"))) |>
    ggplot(aes(x=value)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "",
         title = "Which of the following is/are also included in the Guidelines for children aged 5-18?",
         subtitle = "Please tick all that apply") +
    theme_classic()
}

plot_training <- function(data) {
  data |>
    select(q23:q24) |>
    pivot_longer(c(q23:q24)) |>
    filter(!is.na(value)) |>
    mutate(name = case_when(name == "q23" ~ "Do you feel you have been adequately trained to give physical activity advice to the general population?",
                            name == "q24" ~ "Would you like more formal training on physical activity (epidemiology, health benefits, promotion, indications and prescriptions)?"
    )) |>
    group_by(name) |>
    count(name, value)|>
    mutate(pct = prop.table(n)) |>
    ggplot(aes(x=name, fill=value)) +
    geom_col(aes(y=pct), position = "fill") +
    geom_label(aes(y=pct, label=glue("n = {n}")),
               position = "fill",     
               vjust = 1.5,    # nudge above below bar
               size = 3, show.legend = FALSE) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    scale_y_continuous(name = "Percent", labels=percent) +
    scale_fill_manual(values = c("grey60","lightgrey")) +
    labs(x = "",
         y = "Percent",
         fill = "",
         title = "Training on Physical Activity") +
    theme_classic()
}

plot_training_settings <- function(data) {
  data |>
    select(q25) |>
    separate(q25, into = rep(letters), sep = ",") |>
    select(a:f) |>
    pivot_longer(c(a:f)) |>
    filter(!is.na(value)) |>
    count(value)|>
    mutate(pct = n/179,
           value = factor(value, 
                          levels = c("Stand-alone lectures",
                                     "Seminars and workshops",
                                     "Assessable content",
                                     "Digital learning and electronic content",
                                     "Guided studies",
                                     "Other"))) |>
    ggplot(aes(x=value)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "",
         title = "In which settings would you have valued more teaching on physical activity?") +
    theme_classic() 
  
}

plot_knowledge_sources <- function(data) {
  data |>
    select(q27) |>
    separate(q27, into = rep(letters), sep = ",") |>
    mutate(a = case_when(a == "Media" ~
                           "Media, YouTube, internet",
                         a == " YouTube" ~ NA,
                         a == " internet" ~ NA,
                         a == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         a == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(a)),
           b = case_when(b == "Media" ~
                           "Media, YouTube, internet",
                         b == " YouTube" ~ NA,
                         b == " internet" ~ NA,
                         b == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         b == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(b)),
           c = case_when(c == "Media" ~
                           "Media, YouTube, internet",
                         c == " YouTube" ~ NA,
                         c == " internet" ~ NA,
                         c == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         c == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(c)),
           d = case_when(d == "Media" ~
                           "Media, YouTube, internet",
                         d == " YouTube" ~ NA,
                         d == " internet" ~ NA,
                         d == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         d == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(d)),
           e = case_when(e == "Media" ~
                           "Media, YouTube, internet",
                         e == " YouTube" ~ NA,
                         e == " internet" ~ NA,
                         e == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         e == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(e)),
           f = case_when(f == "Media" ~
                           "Media, YouTube, internet",
                         f == " YouTube" ~ NA,
                         f == " internet" ~ NA,
                         f == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         f == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(f)),
           g = case_when(g == "Media" ~
                           "Media, YouTube, internet",
                         g == " YouTube" ~ NA,
                         g == " internet" ~ NA,
                         g == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         g == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(g)),
           h = case_when(h == "Media" ~
                           "Media, YouTube, internet",
                         h == " YouTube" ~ NA,
                         h == " internet" ~ NA,
                         h == "External conferences" ~
                           "External conferences, informal teaching or presentations",
                         h == " informal teaching or presentations" ~ NA,
                         TRUE ~ as.character(h))) |>
    select(a:h) |>
    pivot_longer(c(a:h)) |>
    filter(!is.na(value)) |>
    count(value)|>
    mutate(pct = n/179,
           value = factor(value, 
                          levels = c("Nursing programme curriculum",
                                     "Individual interest and reading",
                                     "External conferences, informal teaching or presentations",
                                     "Media, YouTube, internet",
                                     "Through participating in physical activity or sport",
                                     "Other"))) |>
    ggplot(aes(x=value)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x = "",
         title = "Where have you gained your physical activity knowledge?") +
    theme_classic() 
}

plot_PA_levels <- function(data) {
  student_PA_comparison <- data |>
    select(q30) |>
    pivot_longer(q30) |>
    filter(!is.na(value)) |>
    count(value)|>
    mutate(pct = prop.table(n)) |>
    mutate(value = factor(value, 
                          levels = c("Very",
                                     "Somewhat",
                                     "Average",
                                     "Somewhat not",
                                     "Not at all")
    )
    ) |>
    ggplot(aes(x=value)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x=str_wrap("How physically active would you rate yourself compared to an average university student?",50)) +
    theme_classic() +
    theme(axis.title.x = element_text(size=10))
  
  
  PA_mins <- data |>
    select(q39) |>
    pivot_longer(q39) |>
    filter(!is.na(value)) |>
    count(value)|>
    mutate(pct = prop.table(n)) |>
    mutate(value = factor(value, 
                          levels = c("Under 30 minutes",
                                     "31-90 minutes",
                                     "90-149 minutes",
                                     "150+ minutes")
    )
    ) |>
    ggplot(aes(x=value)) +
    geom_col(aes(y = pct)) + 
    scale_y_continuous(name = "Percent", labels=percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
    geom_label(aes(y=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 3) +
    labs(x=str_wrap("In the past week, how much moderate intensity physical activity have you completed in total?",50)) +
    theme_classic() +
    theme(axis.title.x = element_text(size=10))
  
  (student_PA_comparison + PA_mins) +
    plot_annotation(title = "Physical Activity Levels")
}

plot_example_mod_activities <- function(data) {
  
  
  # Let's create tokens
  data("stop_words")
  
  data_tokens <- data |>
    select(q32:q34) |>
    rowid_to_column("id") |>
    pivot_longer(q32:q34) |>
    unnest_tokens(word, value) |>
    anti_join(stop_words) |>
    filter(!is.na(word)) 
  
  # spelling errors - https://books.psychstat.org/textmining/data.html
  words <- unique(data_tokens$word)
  bad_words <- hunspell(words)
  bad_words <- unique(unlist(bad_words))
  suggest_words <- hunspell_suggest(bad_words)
  suggest_words <- unlist(lapply(suggest_words, function(x) x[1]))
  
  
  # # combine and compare suggestions (manually checked obvious errors)
  #
  bad_suggest_words <- bind_cols(bad_words, suggest_words)
  
  count_words <- count(data_tokens, word)
  
  bad_suggest_words <- inner_join(count_words, bad_suggest_words, by = c(word = "...1"))
  
  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words <- recode(suggest_words,
                          "kangaroo" = "kangoo",
                          "Nintendo" = "nintendo",
                          "Pilates" = "pilates",
                          "push up" = "pushup",
                          "setup" = "situp",
                          "tie" = "tai",
                          "rumba" = "zumba"
  )
  
  
  ### Add checking the suggestions
  
  bad_whole_words <- paste0("\\b", bad_words, "\\b")
  
  data_tokens$word <- stri_replace_all_regex(data_tokens$word, bad_whole_words, suggest_words,
                                             vectorize_all = FALSE)
  
  # for all double barrel terms split unnest again
  data_tokens <-  data_tokens |>
    unnest_tokens(word, word) |>
    filter(!is.na(word)) |>
    rowwise() |>
    mutate(word = singularize(word, dictionary = TRUE))
  
  
  # reunite words back into their individual examples
  data_tokens_wide <- data_tokens |>
    group_by(id) |>
    rowid_to_column() |>
    pivot_wider(id_cols = c(id, rowid),
                names_from = name,
                values_from = word) 
  
  example_one <- data_tokens_wide |>
    select(id,q32) |>
    group_by(id) |> 
    filter(!is.na(q32)) |>
    mutate(word = row_number(),
           word = paste("word",word)) |>
    pivot_wider(id_cols = id,
                names_from = word,
                values_from = q32) |>
    unite("Example One", c(2:5), sep = " ", remove = TRUE, na.rm = TRUE)
  
  example_two <- data_tokens_wide |>
    select(id,q33) |>
    group_by(id) |> 
    filter(!is.na(q33)) |>
    mutate(word = row_number(),
           word = paste("word",word)) |>
    pivot_wider(id_cols = id,
                names_from = word,
                values_from = q33) |>
    unite("Example Two", c(2:6), sep = " ", remove = TRUE, na.rm = TRUE)
  
  example_three <- data_tokens_wide |>
    select(id,q34) |>
    group_by(id) |> 
    filter(!is.na(q34)) |>
    mutate(word = row_number(),
           word = paste("word",word)) |>
    pivot_wider(id_cols = id,
                names_from = word,
                values_from = q34) |>
    unite("Example Three", c(2:7), sep = " ", remove = TRUE, na.rm = TRUE)
  
  data_tokens_reunited <- left_join(example_one, example_two, by = "id") |>
    left_join(example_three, by = "id") |>
    pivot_longer(2:4, 
                 names_to = "example_number",
                 values_to = "response") |>
    filter(!is.na(response)) |>
    mutate(example_number = factor(example_number,
                                   levels = c("Example One",
                                              "Example Two",
                                              "Example Three")))
  
  # Plot simple counts
  simple_count_plot <- data_tokens_reunited |>
    ungroup() |>
    count(response, example_number, sort = TRUE) |>
    mutate(pct = prop.table(n)) |>
    filter(n > 2) |>
    mutate(response = reorder(response, n)) |>
    ggplot(aes(pct, response)) +
    geom_col() +
    scale_x_continuous(name = "Percent", labels=percent, 
                       breaks = seq(from=0,to=0.10, by=0.025),
                       limits = c(0,0.11)) +
    geom_label(aes(x=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 2,
               label.padding = unit(0.15, "lines")) +
    labs(y = NULL) +
    facet_grid(.~example_number) +
    theme_classic()
  
  
  # Common bigrams
  data_bigrams <- data |>
    select(q32:q34) |>
    rowid_to_column("id") |>
    pivot_longer(q32:q34) |>
    unnest_tokens(bigram, value, token = "ngrams", n =2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word) |>
    filter(!word2 %in% stop_words$word) |>
    filter(!is.na(word1)) |>
    filter(!is.na(word2)) |>
    rowid_to_column()
  
  # spelling errors - https://books.psychstat.org/textmining/data.html
  words1 <- unique(data_bigrams$word1)
  bad_words1 <- hunspell(words1)
  bad_words1 <- unique(unlist(bad_words1))
  suggest_words1 <- hunspell_suggest(bad_words1)
  suggest_words1 <- unlist(lapply(suggest_words1, function(x) x[1]))
  
  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words1 <- recode(suggest_words1,
                           "kangaroo" = "kangoo",
                           "Nintendo" = "nintendo",
                           "Pilates" = "pilates",
                           "push up" = "pushup",
                           "setup" = "situp",
                           "tie" = "tai",
                           "rumba" = "zumba"
  )
  
  
  ### Add checking the suggestions
  
  bad_whole_words1 <- paste0("\\b", bad_words1, "\\b")
  
  
  data_bigrams$word1 <- stri_replace_all_regex(data_bigrams$word1, bad_whole_words1, suggest_words1,
                                               vectorize_all = FALSE)
  
  # spelling errors - https://books.psychstat.org/textmining/data.html
  words2 <- unique(data_bigrams$word2)
  bad_words2 <- hunspell(words2)
  bad_words2 <- unique(unlist(bad_words2))
  suggest_words2 <- hunspell_suggest(bad_words2)
  suggest_words2 <- unlist(lapply(suggest_words2, function(x) x[1]))
  
  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words2 <- recode(suggest_words2,
                           "kangaroo" = "kangoo",
                           "Nintendo" = "nintendo",
                           "Pilates" = "pilates",
                           "push up" = "pushup",
                           "setup" = "situp",
                           "tie" = "tai",
                           "rumba" = "zumba"
  )
  
  
  ### Add checking the suggestions
  
  bad_whole_words2 <- paste0("\\b", bad_words2, "\\b")
  
  data_bigrams$word2 <- stri_replace_all_regex(data_bigrams$word2, bad_whole_words2, suggest_words2,
                                               vectorize_all = FALSE)
  
  data_bigrams <- data_bigrams |>
    unite(response_name, c("word1","word2"), sep = " ") |>
    unnest_tokens(bigram, response_name, token = "ngrams", n =2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word) |>
    filter(!word2 %in% stop_words$word) |>
    mutate(word1 = na_if(word1, "na"),
           word2 = na_if(word2, "na")) |>
    filter(!is.na(word1)) |>
    filter(!is.na(word2)) |>
    rowwise() |>
    mutate(word1 = singularize(word1, dictionary = TRUE),
           word2 = singularize(word2, dictionary = TRUE)) |>
    mutate(name = case_when(name == "q32" ~ "Example One",
                            name == "q33" ~ "Example Two",
                            name == "q34" ~ "Example Three"),
           example_number = factor(name,
                                   levels = c("Example One",
                                              "Example Two",
                                              "Example Three"))
    )
  
  example_number <- sort(unique(data_bigrams$name))
  
  bigram_plots <- list()
  
  for(i in example_number) {
    bigram_counts <- data_bigrams |>
      filter(example_number == i) |>
      count(word1, word2, sort = TRUE) |>
      select(word1, word2, n) |>
      filter(n > 1) |>
      as_tbl_graph()
    
    set.seed(2020)
    
    a <- arrow(length = unit(.1, "inches"))
    
    bigram_plot <- ggraph(bigram_counts, layout = 'grid') +
      geom_edge_arc(aes(edge_alpha = n,
                        start_cap = label_rect(node1.name),
                        end_cap = label_rect(node2.name)),
                    show.legend = FALSE,
                    arrow = arrow(angle = 20, length = unit(2, 'mm')))  +
      geom_node_point(color = NA, size = 10) +
      geom_node_label(aes(label = name), size = 2, label.padding = unit(0.1, "lines")) +
      labs(title = paste(i)) +
      theme_graph() +
      theme(plot.title = element_text(size = 10),
            panel.border = element_rect(colour = "black", fill=NA))
    
    bigram_plots[[i]] <- bigram_plot
    
  }
  
  (simple_count_plot /
    (bigram_plots$`Example One` + bigram_plots$`Example Two` + bigram_plots$`Example Three`)) +
    plot_annotation(title = "Please give 3 examples of what types of physical activity that you believe are at a moderate intensity")
  
}

plot_example_vig_activities <- function(data) {
  
  
  # Let's create tokens
  data("stop_words")
  
  data_tokens <- data |>
    select(q36:q38) |>
    rowid_to_column("id") |>
    pivot_longer(q36:q38) |>
    unnest_tokens(word, value) |>
    anti_join(stop_words) |>
    filter(!is.na(word)) 
  
  # spelling errors - https://books.psychstat.org/textmining/data.html
  words <- unique(data_tokens$word)
  bad_words <- hunspell(words)
  bad_words <- unique(unlist(bad_words))
  suggest_words <- hunspell_suggest(bad_words)
  suggest_words <- unlist(lapply(suggest_words, function(x) x[1]))
  
  
  # # combine and compare suggestions (manually checked obvious errors)
  #
  bad_suggest_words <- bind_cols(bad_words, suggest_words)
  
  count_words <- count(data_tokens, word)
  
  bad_suggest_words <- inner_join(count_words, bad_suggest_words, by = c(word = "...1"))
  
  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words <- recode(suggest_words,
                          "cross fit" = "crossfit",
                          "kraal" = "krav",
                          "mags" = "maga",
                          "Pilates" = "pilates",
                          "pilasters" = "pilates",
                          "rumba" = "zumba"
  )
  
  
  ### Add checking the suggestions
  
  bad_whole_words <- paste0("\\b", bad_words, "\\b")
  
  data_tokens$word <- stri_replace_all_regex(data_tokens$word, bad_whole_words, suggest_words,
                                             vectorize_all = FALSE)
  
  # for all double barrel terms split unnest again
  data_tokens <-  data_tokens |>
    unnest_tokens(word, word) |>
    filter(!is.na(word)) |>
    rowwise() |>
    mutate(word = singularize(word, dictionary = TRUE))
  
  
  # reunite words back into their individual examples
  data_tokens_wide <- data_tokens |>
    group_by(id) |>
    rowid_to_column() |>
    pivot_wider(id_cols = c(id, rowid),
                names_from = name,
                values_from = word) 
  
  example_one <- data_tokens_wide |>
    select(id,q36) |>
    group_by(id) |> 
    filter(!is.na(q36)) |>
    mutate(word = row_number(),
           word = paste("word",word)) |>
    pivot_wider(id_cols = id,
                names_from = word,
                values_from = q36) |>
    unite("Example One", c(2:4), sep = " ", remove = TRUE, na.rm = TRUE)
  
  example_two <- data_tokens_wide |>
    select(id,q37) |>
    group_by(id) |> 
    filter(!is.na(q37)) |>
    mutate(word = row_number(),
           word = paste("word",word)) |>
    pivot_wider(id_cols = id,
                names_from = word,
                values_from = q37) |>
    unite("Example Two", c(2:6), sep = " ", remove = TRUE, na.rm = TRUE)
  
  example_three <- data_tokens_wide |>
    select(id,q38) |>
    group_by(id) |> 
    filter(!is.na(q38)) |>
    mutate(word = row_number(),
           word = paste("word",word)) |>
    pivot_wider(id_cols = id,
                names_from = word,
                values_from = q38) |>
    unite("Example Three", c(2:5), sep = " ", remove = TRUE, na.rm = TRUE)
  
  data_tokens_reunited <- left_join(example_one, example_two, by = "id") |>
    left_join(example_three, by = "id") |>
    pivot_longer(2:4, 
                 names_to = "example_number",
                 values_to = "response") |>
    filter(!is.na(response)) |>
    mutate(example_number = factor(example_number,
                                   levels = c("Example One",
                                              "Example Two",
                                              "Example Three")))
  
  # Plot simple counts
  simple_count_plot <- data_tokens_reunited |>
    ungroup() |>
    count(response, example_number, sort = TRUE) |>
    mutate(pct = prop.table(n)) |>
    filter(n > 2) |>
    mutate(response = reorder(response, n)) |>
    ggplot(aes(pct, response)) +
    geom_col() +
    scale_x_continuous(name = "Percent", labels=percent, 
                       breaks = seq(from=0,to=0.05, by=0.0125),
                       limits = c(0,0.055)) +
    geom_label(aes(x=pct,label=glue("n = {n}")),
               position = position_dodge(width = .9),    # move to center of bars
               vjust = 0.5,    # nudge above top of bar
               size = 2,
               label.padding = unit(0.15, "lines")) +
    labs(y = NULL) +
    facet_grid(.~example_number) +
    theme_classic()
  
  
  # Common bigrams
  data_bigrams <- data |>
    select(q36:q38) |>
    rowid_to_column("id") |>
    pivot_longer(q36:q38) |>
    unnest_tokens(bigram, value, token = "ngrams", n =2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word) |>
    filter(!word2 %in% stop_words$word) |>
    filter(!is.na(word1)) |>
    filter(!is.na(word2)) |>
    rowid_to_column()
  
  # spelling errors - https://books.psychstat.org/textmining/data.html
  words1 <- unique(data_bigrams$word1)
  bad_words1 <- hunspell(words1)
  bad_words1 <- unique(unlist(bad_words1))
  suggest_words1 <- hunspell_suggest(bad_words1)
  suggest_words1 <- unlist(lapply(suggest_words1, function(x) x[1]))
  
  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words1 <- recode(suggest_words1,
                           "kangaroo" = "kangoo",
                           "Nintendo" = "nintendo",
                           "Pilates" = "pilates",
                           "push up" = "pushup",
                           "setup" = "situp",
                           "tie" = "tai",
                           "rumba" = "zumba"
  )
  
  
  ### Add checking the suggestions
  
  bad_whole_words1 <- paste0("\\b", bad_words1, "\\b")
  
  
  data_bigrams$word1 <- stri_replace_all_regex(data_bigrams$word1, bad_whole_words1, suggest_words1,
                                               vectorize_all = FALSE)
  
  # spelling errors - https://books.psychstat.org/textmining/data.html
  words2 <- unique(data_bigrams$word2)
  bad_words2 <- hunspell(words2)
  bad_words2 <- unique(unlist(bad_words2))
  suggest_words2 <- hunspell_suggest(bad_words2)
  suggest_words2 <- unlist(lapply(suggest_words2, function(x) x[1]))
  
  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words2 <- recode(suggest_words2,
                           "kangaroo" = "kangoo",
                           "Nintendo" = "nintendo",
                           "Pilates" = "pilates",
                           "push up" = "pushup",
                           "setup" = "situp",
                           "tie" = "tai",
                           "rumba" = "zumba"
  )
  
  
  ### Add checking the suggestions
  
  bad_whole_words2 <- paste0("\\b", bad_words2, "\\b")
  
  data_bigrams$word2 <- stri_replace_all_regex(data_bigrams$word2, bad_whole_words2, suggest_words2,
                                               vectorize_all = FALSE)
  
  data_bigrams <- data_bigrams |>
    unite(response_name, c("word1","word2"), sep = " ") |>
    unnest_tokens(bigram, response_name, token = "ngrams", n =2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word) |>
    filter(!word2 %in% stop_words$word) |>
    mutate(word1 = na_if(word1, "na"),
           word2 = na_if(word2, "na")) |>
    filter(!is.na(word1)) |>
    filter(!is.na(word2)) |>
    rowwise() |>
    mutate(word1 = singularize(word1, dictionary = TRUE),
           word2 = singularize(word2, dictionary = TRUE)) |>
    mutate(name = case_when(name == "q36" ~ "Example One",
                            name == "q37" ~ "Example Two",
                            name == "q38" ~ "Example Three"),
           example_number = factor(name,
                                   levels = c("Example One",
                                              "Example Two",
                                              "Example Three"))
    )
  
  example_number <- sort(unique(data_bigrams$name))
  
  bigram_plots <- list()
  
  for(i in example_number) {
    bigram_counts <- data_bigrams |>
      filter(example_number == i) |>
      count(word1, word2, sort = TRUE) |>
      select(word1, word2, n) |>
      filter(n > 1) |>
      as_tbl_graph()
    
    set.seed(2020)
    
    a <- arrow(length = unit(.1, "inches"))
    
    bigram_plot <- ggraph(bigram_counts, layout = 'grid') +
      geom_edge_arc(aes(edge_alpha = n,
                        start_cap = label_rect(node1.name),
                        end_cap = label_rect(node2.name)),
                    show.legend = FALSE,
                    arrow = arrow(angle = 20, length = unit(2, 'mm')))  +
      geom_node_point(color = NA, size = 10) +
      geom_node_label(aes(label = name), size = 2, label.padding = unit(0.1, "lines")) +
      labs(title = paste(i)) +
      theme_graph() +
      theme(plot.title = element_text(size = 10),
            panel.border = element_rect(colour = "black", fill=NA))
    
    bigram_plots[[i]] <- bigram_plot
    
  }
  
  (simple_count_plot /
      (bigram_plots$`Example One` + bigram_plots$`Example Two` + bigram_plots$`Example Three`)) +
    plot_annotation(title = "Please give 3 examples of what types of physical activity that you believe are at a high intensity")
  
}