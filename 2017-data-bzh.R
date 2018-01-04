library(tidyverse)
library(magrittr)
source("data-bzh-tools-master/main.R")

files <- list.files("../open-data/", pattern = "^2017", full.names = TRUE)
full <- map_df(files, read_csv)

full$heure %<>% as.POSIXct()
glimpse(full)

# Stats 

full %>% 
  summarise(`Volume` = n(), 
            `Impressions Moyennes` = mean(impressions), 
            `Engagements Moyens` = mean(engagements), 
            `Retweets Moyens` = mean(Retweets), 
            `Réponses Moyens` = mean(réponses), 
            `J'aime Moyens` = mean(`J'aime`)) %>% 
  knitr::kable() %>%
  clipr::write_clip()

full <- full %>% mutate(mois = lubridate::month(heure))

full %>% 
  group_by(mois) %>%
  summarise(`Volume` = n(), 
            `Impressions Totales` = sum(impressions), 
            `Engagements Totaux` = sum(engagements), 
            `Retweets Totaux` = sum(Retweets), 
            `Réponses Totales` = sum(réponses), 
            `J'aime Totaux` = sum(`J'aime`)) %>% 
  knitr::kable() %>%
  clipr::write_clip()

mois <- full %>% 
  group_by(mois) %>%
  summarise(`Volume` = n(), 
            `Impressions Totales` = sum(impressions), 
            `Engagements Totaux` = sum(engagements), 
            `Retweets Totaux` = sum(Retweets), 
            `Réponses Totales` = sum(réponses), 
            `J'aime Totaux` = sum(`J'aime`)) 

ggplot(mois) + 
  aes(mois, Volume) + 
  geom_col(fill = databzh$colour1)  +
  labs(title = "Tweets par mois en 2018", 
       subtitle = "Données via Twitter", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  
ggplot(mois) + 
  aes(mois, `Impressions Totales`) + 
  geom_col(fill = databzh$colour2)  +
  labs(title = "Impressions Totales par mois en 2018", 
       subtitle = "Données via Twitter", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  
ggplot(mois) + 
  aes(mois, `Engagements Totaux`) + 
  geom_col(fill = databzh$colour3)  +
  labs(title = "Engagements Totaux par mois en 2018", 
       subtitle = "Données via Twitter", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  
ggplot(mois) + 
  aes(mois, `Retweets Totaux`) + 
  geom_col(fill = databzh$colour4)  +
  labs(title = "Retweets Totaux par mois en 2018", 
       subtitle = "Données via Twitter", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  
ggplot(mois) + 
  aes(mois, `Réponses Totales`) + 
  geom_col(fill = databzh$colour5)  +
  labs(title = "Réponses Totales par mois en 2018", 
       subtitle = "Données via Twitter", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  
ggplot(mois) + 
  aes(mois, `J'aime Totaux`) + 
  geom_col(fill = databzh$colour6)  +
  labs(title = "J'aime Totaux par mois en 2018", 
       subtitle = "Données via Twitter", 
       caption = "http://data-bzh.fr") + 
  databzhTheme()
  
full <- full %>% 
  mutate(hash = str_extract_all(`Texte du Tweet`, "#[A-Za-z]* "))

# Hahstags

full %>% 
  unnest() %>% 
  select(hash) %>% 
  mutate(hash = tolower(hash)) %>%
  count(hash, sort = TRUE) %>% 
  top_n(10) %>%
  knitr::kable() %>%
  clipr::write_clip()

# tweets les plus 

full %>% 
  top_n(1, Retweets) %>%
  select(`Identifiant du Tweet`, heure, Retweets, `Permalien du Tweet`) %>%
  knitr::kable() %>% 
  clipr::write_clip()

# tweets les plus 

full %>% 
  top_n(1, `J'aime`) %>%
  select(`Identifiant du Tweet`, heure, `J'aime`, `Permalien du Tweet`) %>%
  knitr::kable() %>% 
  clipr::write_clip()


