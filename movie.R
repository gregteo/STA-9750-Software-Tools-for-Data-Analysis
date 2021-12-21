library(tidyverse)
library(jsonlite)


tibble_metadata <- read_csv("movies_metadata.csv") 
tibble_metadata #### nrow = 45466

tibble_rating <- read_csv("ratings.csv") %>% group_by(movieId) %>% summarise(m_rating = median(rating))
tibble_rating

tibble_link <- read_csv("links.csv") %>% inner_join(tibble_rating) %>% select(-tmdbId) 
tibble_link  ### nrow = 45115

imdb_id <- NULL
for (i in tibble_link$imdbId){
  i = paste(c("tt",i), collapse = "")
  imdb_id = c(imdb_id,i)
}
tibble_link1 <- cbind(tibble_link, imdb_id)

tibble_all <- tibble_link1 %>% inner_join(tibble_metadata) %>% filter(!is.na(m_rating)) %>% 
  select(m_rating, imdb_id, budget, genres, production_companies, adult, runtime, original_language,
         release_date, revenue, popularity) %>% 
  filter(original_language == "en") %>% view()

all_genres <- NULL
for (i in tibble_all$genres){
  g <- fromJSON(gsub("'", '"', i))$name
  all_genres <- c(g, all_genres)
}
all_genres
unique(all_genres)

for (current_genre in unique(all_genres)) {  
  tibble_all <- tibble_all %>% mutate(!!current_genre := grepl(current_genre, tibble_all$genres))
}

all_company <- NULL
for (i in tibble_all$production_companies){
  p <- fromJSON(gsub("'", '"', i))$name
  all_company <- c(p, all_company)
}
all_company
unique(all_company)

for (current_company in unique(all_company)) {  
  tibble_all <- tibble_all %>% mutate(!!current_company := grepl(current_company, tibble_all$production_companies))
}

tibble_all %>% select(-genres) %>% select(-imdb_id) %>% select(-production_companies) %>% 
  select(-original_language) %>% view()



