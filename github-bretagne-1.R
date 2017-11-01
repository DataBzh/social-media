library(tidyverse)
library(httr)
library(glue)
library(jsonlite)

app_id <- "github"

# Les tokens de l'app
client_id <- "***"
client_secret <- "***"

# Créons un oauth_endpoint, autrement dit un identifiant pour accéder à l'API
github_endpoint <- oauth_endpoints("github")
github_app <- httr::oauth_app(app_id, client_id, client_secret)

access_token <- httr::oauth2.0_token(github_endpoint, github_app)


get_page <- function(page, location){
  Sys.sleep(3)
  res <- GET(url = glue("https://api.github.com/search/users?q=location%3A{location}&type=Users&per_page=100&page={page}"))
  res$content %>% rawToChar() %>% fromJSON() %>% .$items
}


all_pages <- function(page_count, location){
   map_df(.x = page_count, 
                     .f = ~get_page(page = .x, location = location))
}


get_res <- function(location){
  location <- tolower(location)
  location <- gsub(pattern = " ", "%20", location)
  location <- gsub(pattern = "'", "%27", location)
  res <- GET(url = glue("https://api.github.com/search/users?q=location%3A{location}&type=Users")) 
  results <- res$content %>% rawToChar() %>% fromJSON()
  if (results$total_count != 0) {
    pages <- 1:ceiling(results$total_count / 100)
    all_pages(page_count = pages, location = location)
  }
}


bret <- get_res("Bretagne")
rennes <- get_res("rennes")
brest <- get_res("brest")
quimper <- get_res("quimper")
lorient <- get_res("lorient")
vannes <- get_res("vannes")
saint_brieuc <- get_res("saint brieuc")
saint_malo <- get_res("saint malo")
lanester <- get_res("lanester")
fougeres <- get_res("fougeres")
lannion <- get_res("lannion")
bretagne <- rbind(bret, rennes, brest, quimper, lorient, vannes, saint_brieuc, saint_malo, 
                      lanester, fougeres, lannion)

get_user <- function(username){
  Sys.sleep(0.5)
  res <- GET(url = glue("https://api.github.com/users/{username}"), 
                 config(token = access_token)) 
  #res <- GET(url)
  res <- res$content %>% rawToChar() %>% fromJSON()
  tibble(login = res$login %||% NA, 
         id = res$id %||% NA, 
         avatar = res$avatar_url %||% NA, 
         gravatar = res$gravatar_id %||% NA, 
         url = res$url %||% NA, 
         html_url = res$html_url %||% NA, 
         followers_url = res$followers_url %||% NA, 
         following_url = res$following_url %||% NA, 
         gists_url = res$gists_url %||% NA, 
         starred_url = res$starred_url %||% NA,
         subscriptions_url = res$subscriptions_url %||% NA,
         organizations_url = res$organizations_url %||% NA,
         repos_url = res$repos_url %||% NA,
         events_url = res$events_url %||% NA,
         received_events_url = res$received_events_url %||% NA,
         type = res$type %||% NA,
         site_admin = res$site_admin %||% NA,
         name = res$name %||% NA,
         company = res$company %||% NA,
         blog = res$blog %||% NA,
         location = res$location %||% NA,
         email = res$email %||% NA,
         hireable = res$hireable %||% NA,
         bio = res$bio %||% NA,
         public_repos = res$public_repos %||% NA,
         public_gists = res$public_gists %||% NA,
         followers = res$followers %||% NA,
         following = res$following %||% NA,
         created_at = res$created_at %||% NA,
         updated_at = res$updated_at %||% NA
         )
}

bretagne_users <- map_df(bretagne$login, ~get_user(.x)) 


get_repo <- function(url){
  #Sys.sleep(0.5)
  res <- GET(url = url, 
             config(token = access_token)) 
  res$content %>% rawToChar() %>% fromJSON()
}

bretagne_repos <- map(bretagne$repos_url, ~get_repo(.x)) %>%
  discard(bretagne_repos, ~ length(.x) == 0) %>%
  map(~discard(.x, is.data.frame)) %>%
  bind_rows()

bretagne_users %>%
  count(type)

bretagne_users %>%
  count(hireable)

bretagne_users$company <- gsub("é","e", bretagne_users$company)
bretagne_users %>%
  count(company) %>%
  na.omit() %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  ggplot() +
  aes(reorder(company,n), n) + 
  geom_col(fill = databzh$colour1) + 
  coord_flip() + 
  labs(title = "Compagnie les plus représentées sur Github Bretagne", 
       subtitle = "données via Github API", 
       x = "Société", 
       y = "Volume", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()

bretagne_users %>%
  ggplot() + 
  aes(public_repos) + 
  geom_density(fill = databzh$colour2) + 
  labs(title = "Repos par utilisateurs sur Github Bretagne", 
       subtitle = "données via Github API", 
       x = "Société", 
       y = "Volume", 
       caption = "http://data-bzh.fr") + 
  databzhTheme() 

bretagne_users %>%
  arrange(desc(public_repos)) %>%
  select(login, public_repos) %>% 
  top_n(10) %>%
  knitr::kable()

mean(bretagne_users$followers)
sd(bretagne_users$followers)
bretagne_users %>%
  arrange(desc(followers)) %>%
  select(login, followers) %>% 
  top_n(1) %>%
  knitr::kable()

bretagne_users %>%
  arrange(desc(followers)) %>%
  select(login, followers) %>% 
  top_n(10) %>%
  ggplot() +
  aes(reorder(login, followers), followers) + 
  geom_col(fill = databzh$colour3) +
  coord_flip() +
  labs(title = "Utilisateurs les plus suivis sur Github Bretagne", 
       subtitle = "données via Github API", 
       x = "User", 
       y = "Nombre de followers", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()   

library(tidytext)
unnest_tokens(bretagne_users, word, bio) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  anti_join(proustr::proust_stopwords()) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  knitr::kable()

count(bretagne_users, bio) %>% select(n) %>% tail

length(keep(bretagne_repos, ~length(.x) == 0))

count(bretagne_repos_clean, fork)

bretagne_repos_clean %>%
  count(language) %>%
  na.omit() %>%
  arrange(desc(n)) %>%
  top_n(20) %>%
  ggplot() +
  aes(reorder(language, n), n) + 
  geom_col(fill = databzh$colour4) +
  coord_flip() +
  labs(title = "langages les plus développés sur Github Bretagne", 
       subtitle = "données via Github API", 
       x = "Langage", 
       y = "Nombre de repos", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()  

bretagne_repos_clean %>%
  arrange(desc(watchers_count)) %>%
  select(full_name, language, watchers_count) %>%
  top_n(10) %>%
  knitr::kable()

bretagne_repos_clean %>%
  mutate(created_at = lubridate::ymd_hms(created_at)) %>%
  ggplot() + 
  aes(created_at) + 
  geom_histogram(bins = 50, fill = databzh$colour5) + 
  labs(title = "Date de création des repos sur Github Bretagne", 
       subtitle = "données via Github API", 
       x = "Date de création", 
       y = "Volume de repos", 
       caption = "http://data-bzh.fr") + 
  databzhTheme() 


bretagne_repos_clean %>%
  mutate(created_at = lubridate::ymd_hms(created_at)) %>%
  arrange(created_at) %>%
  select(full_name, language, created_at) %>%
  slice(1:10) %>%
  knitr::kable()

bretagne_repos_clean %>%
  mutate(updated_at = lubridate::ymd_hms(updated_at), 
         created_at = lubridate::ymd_hms(created_at)) %>%
  gather(key = Action, value = date, 45:46) %>%
  ggplot() + 
  aes(date, fill = Action) +
  facet_grid(Action ~ .) + 
  geom_histogram(bins = 50) + 
  scale_fill_manual(values = c(databzh$colour5, databzh$colour6)) +
  labs(title = "Date de création et du dernier update des repos sur Github Bretagne", 
       subtitle = "données via Github API", 
       x = "Date de l'action", 
       y = "Volume de repos", 
       caption = "http://data-bzh.fr") + 
  databzhTheme() 

unnest_tokens(bretagne_repos_clean, word, description) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  anti_join(proustr::proust_stopwords()) %>%
  na.omit() %>%
  top_n(10)%>%
  arrange(desc(n)) %>%
  knitr::kable()

bretagne_repos_clean %>%
  top_n(10, size) %>%
  select(full_name, language, size) %>%
  arrange(desc(size)) %>%
  knitr::kable()
