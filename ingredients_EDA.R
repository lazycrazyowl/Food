# ingredients EDA

library(tidyverse)
library(tidytext)
library(widyr)

df <- read_csv("ingredients.csv")

df %>% unnest_tokens(word,ingredients_short) %>%
  count(word) %>%
  arrange(desc(n))

pair_count <- df %>% 
  unnest_tokens(word,ingredients_short) %>%
  filter(!word %in% c("sale","olio","pepe")) %>%
  pairwise_count(word,dish_names) %>%
  mutate(pair = glue::glue("{item1}, {item2}"))
  
pair_count$pair <- pair_count$pair %>%
   str_split(", ") %>% 
   map(sort) %>%
   map(paste0,collapse=", ") %>% unlist()

pair_count <- pair_count %>%
  arrange(desc(n)) %>%
  distinct(pair,.keep_all = T) %>%
  mutate(row = rev(row_number()))

pair_count %>%
  top_n(25,n) %>%
  ggplot(aes(row,n)) +
  geom_col() +
  coord_flip() +
  scale_x_continuous(breaks=pair_count$row,labels=pair_count$pair)
