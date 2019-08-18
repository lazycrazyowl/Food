# get ingredients fromgiallozafferano, classic primi

library(tidyverse)
library(rvest)

url_part1 <- "https://www.giallozafferano.it/ricette-cat/page"
url_part2 <-  "/Primi/pasta/grandi-classici/"

page_nums <- 1:7

urls_level1 <- glue::glue("{url_part1}{page_nums}{url_part2}")

urls_level2 <- urls_level1 %>%
  map(read_html) %>%
  map(html_nodes,"h2 a") %>%
  map(html_attr,"href") %>%
  unlist()

dish_names <- urls_level1 %>%
  map(read_html) %>%
  map(html_nodes,"h2 a") %>%
  map(html_attr,"title") %>%
  unlist()

ingredients_long_list <- urls_level2 %>%
  map(read_html) %>%
  map(html_nodes,"dd") %>%
  map(html_text) %>%
  map(str_replace_all, "[^[:alpha:]]", " ") %>% # keep words
  map(str_replace_all,"[ ]+", " ") %>% # remove long spaces
  map(str_remove_all," g ") %>%
  map(str_remove_all, " q b ") %>%
  map(str_remove_all, " kg") %>%
  map(str_trim) #%>%
  #unlist()

ingredients_short_list <- ingredients1 %>%
  map(str_extract,"([[:alpha:]]+)")

ingredients_long <- ingredients_long_list %>% map(paste0, collapse=", ") %>% unlist()
ingredients_short <- ingredients_short_list %>% map(paste0, collapse=", ") %>% unlist()

df <- tibble(dish_names,ingredients_long,ingredients_short)

write_csv(df,"ingredients.csv")

#ingredients %>% tibble() %>% group_by(factor(.)) %>% count() %>% arrange(desc(n))


