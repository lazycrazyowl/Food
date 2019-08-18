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

pair_count_25 <- pair_count %>%
  top_n(25,n) 

pair_count_25 %>%
  ggplot(aes(row,n)) +
  geom_col(fill="darkred",width=.5) +
  coord_flip() +
  scale_x_continuous(breaks=pair_count_25$row,labels=pair_count_25$pair) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

pair_count %>%
  select(item1,item2,n) %>%
  filter(n > 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  theme(legend.position = "none")
