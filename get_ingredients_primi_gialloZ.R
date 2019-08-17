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

ingredients1 <- urls_level2 %>%
  map(read_html) %>%
  map(html_nodes,"dd") %>%
  map(html_text) %>%
  map(str_replace_all, "[^[:alpha:]]", " ") %>%
  map(str_replace_all,"[ ]+", " ") %>%
  map(str_remove_all," g ") %>%
  map(str_remove_all, " q b ") %>%
  map(str_remove_all, " kg") %>%
  #consider keeping only first word
  map(str_trim) %>%
  unlist()

ingredients2 <- ingredients1 %>%
  str_extract("([[:alpha:]]+)")
  
ingredients %>% tibble() %>% group_by(factor(.)) %>% count() %>% arrange(desc(n))


