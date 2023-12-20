library(tidyverse)

library(patchwork)

data <- read_csv("data/nursing_PA_data.csv")

library(tidytext)
library(stringi)
library(forcats)
library(igraph)
library(ggraph)


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
library(hunspell)
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
  # filter(!row_number() %in% 34680) |>
  # slice_head(n=34680) |>
  rowwise() |>
  mutate(word = SemNetCleaner::singularize(word, dictionary = TRUE))

# # for some reason it changes "press" to "pres" and "raises" to "rais" so we change back
# data_tokens$word <- recode(data_tokens$word,
#                            "pres" = "press",
#                            "rais" = "raise")


data_tokens_wide <- data_tokens |>
  pivot_wider(id_cols = id,
              names_from = name,
              values_from = word) |>
  mutate(q34 = unlist(strsplit(q34,""))) 

# Plot simple counts
data_tokens |>
  count(word, sort = TRUE) |>
  filter(n > 10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  theme_classic()
















# Common bigrams
data_bigrams <- data |>
  select(ResponseId, book_exercise_name,
         body_position, body_part, action, equipment, equipment_position, action_direction, misc,
         recognise, response_name) |>
  unnest_tokens(bigram, response_name, token = "ngrams", n =2) |>
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
                         "flye" = "fly",
                         "flue" = "fly",
                         "flues" = "fly",
                         "flyes" = "fly",
                         "pend lay" = "pendlay",
                         "probated" = "pronated",
                         "DEC" = "deck",
                         "insulated" = "supinated",
                         "hop" = "ohp",
                         "felt" = "delt",
                         "pull downs" = "pull down",
                         "antediluvian" = "vitruvian",
                         "devoid" = "deltoid",
                         "soles" = "soleus",
                         "flex or" = "flexor",
                         "trice" = "tricep",
                         "pendent" = "pendlay",
                         "resistivity" = "resistive",
                         "kinetics" = "isokinetic",
                         "fliers" = "fly",
                         "font" = "dont",
                         "selector" = "selectorized",
                         "cal" = "calf",
                         "dumbbells" = "dumbbell",
                         "flatcar" = "flat bar",
                         "gastronomic" = "gastrocnemius",
                         "overboard" = "hoverboard",
                         "playpen" = "pendlay",
                         "pen delay" = "pendlay",
                         "tr" = "trx",
                         "virtual" = "vitruvian",
                         "id" = "idk",
                         "Maxine" = "machine",
                         "precede" = "pec deck",
                         "raid" = "raise",
                         "pinnate" = "supinate",
                         "trapezes" = "trapezius",
                         "trapeziums" = "trapezius",
                         "barb" = "barbell",
                         "calf raises" = "calf raise",
                         "chestfuls" = "chest fly",
                         "extrasensory" = "extensor",
                         "extensions" = "extension",
                         "floors" = "flexor",
                         "gluten" = "gluteal",
                         "spelldown" = "lat pull down",
                         "ply" = "Olympic",
                         "peck" = "pec deck",
                         "peddle" = "pendlay",
                         "dependably" = "pendlay",
                         "pen lay" = "pendlay",
                         "preach" = "press",
                         "detonated" = "pronated",
                         "teases" = "seated",
                         "selector" = "selectorized",
                         "serrate" = "serratus",
                         "ottoman" = "zottman"
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
                         "flye" = "fly",
                         "flue" = "fly",
                         "flues" = "fly",
                         "flyes" = "fly",
                         "pend lay" = "pendlay",
                         "probated" = "pronated",
                         "DEC" = "deck",
                         "insulated" = "supinated",
                         "hop" = "ohp",
                         "felt" = "delt",
                         "pull downs" = "pull down",
                         "antediluvian" = "vitruvian",
                         "devoid" = "deltoid",
                         "soles" = "soleus",
                         "flex or" = "flexor",
                         "trice" = "tricep",
                         "pendent" = "pendlay",
                         "resistivity" = "resistive",
                         "kinetics" = "isokinetic",
                         "fliers" = "fly",
                         "font" = "dont",
                         "selector" = "selectorized",
                         "cal" = "calf",
                         "dumbbells" = "dumbbell",
                         "flatcar" = "flat bar",
                         "gastronomic" = "gastrocnemius",
                         "overboard" = "hoverboard",
                         "playpen" = "pendlay",
                         "pen delay" = "pendlay",
                         "tr" = "trx",
                         "virtual" = "vitruvian",
                         "id" = "idk",
                         "Maxine" = "machine",
                         "precede" = "pec deck",
                         "raid" = "raise",
                         "pinnate" = "supinate",
                         "trapezes" = "trapezius",
                         "trapeziums" = "trapezius",
                         "barb" = "barbell",
                         "calf raises" = "calf raise",
                         "chestfuls" = "chest fly",
                         "extrasensory" = "extensor",
                         "extensions" = "extension",
                         "floors" = "flexor",
                         "gluten" = "gluteal",
                         "spelldown" = "lat pull down",
                         "ply" = "Olympic",
                         "peck" = "pec deck",
                         "peddle" = "pendlay",
                         "dependably" = "pendlay",
                         "pen lay" = "pendlay",
                         "preach" = "press",
                         "detonated" = "pronated",
                         "teases" = "seated",
                         "selector" = "selectorized",
                         "serrate" = "serratus",
                         "ottoman" = "zottman"
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
  mutate(word1 = SemNetCleaner::singularize(word1, dictionary = TRUE),
         word2 = SemNetCleaner::singularize(word2, dictionary = TRUE))

# for some reason it changes "press" to "pres" and "raises" to "rais" so we change back
data_bigrams$word1 <- recode(data_bigrams$word1,
                             "pres" = "press",
                             "rais" = "raise")
data_bigrams$word2 <- recode(data_bigrams$word2,
                             "pres" = "press",
                             "rais" = "raise")
