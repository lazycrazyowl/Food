# ingredients EDA

library(tidyverse)
library(tidytext)
library(widyr)

df <- read_csv("ingredients.csv")

df %>% unnest_tokens(word,ingredients_short) %>%
  count(word) %>%
  arrange(desc(n))

df %>% unnest_tokens(word,ingredients_short) %>%
  pairwise_count(word,dish_names) %>%
  arrange(desc(n)) %>%
  mutate(row = row_number()) %>%
  filter(row %% 2 != 0) %>%
  select(-row)
