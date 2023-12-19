read_data <- function() {
  data <- read_csv("data/nursing_PA_data.csv")
  
}

plot_q3 <- function(data) {
  data |>
    mutate(q3 = factor(q3, 
                       levels = c("Disagree",
                                  "Neutral",
                                  "Agree",
                                  "Strongly agree",
                                  "Other",
                                  NA)
    )
    ) |>
    filter(!is.na(q3)) |>
    count(q3)|>
    mutate(pct = prop.table(n)) |>
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