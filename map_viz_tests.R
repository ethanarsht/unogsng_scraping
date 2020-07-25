library(tidyverse)
library(sf)
library(leaflet)
library("rnaturalearth")
library("rnaturalearthdata")

titles <- read_csv('all_details_26-5.csv', guess_max = 10000)

df_titles <- titles %>%
  select(clist, country) %>%
  drop_na(country) %>%
  separate_rows(clist, sep = ',') %>%
  mutate(
    country_name = str_extract(clist, '(?<=\\:").*'),
    country_name = str_remove(country_name, '"')
  ) %>%
  group_by(country, country_name) %>%
  summarize(
    count = n()
  ) %>%
  ungroup() %>%
  filter(!str_detect(country_name, '\\+'))



world <- ne_countries(scale = "medium", returnclass = "sf") %>% select(admin)

df_c <- st_centroid(world) %>% st_coordinates %>% as.data.frame()

world <- bind_cols(world, df_c) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  mutate(
    admin = str_replace(admin, 'United States of America', 'United States'),
    admin = str_replace(admin, 'UK', 'United Kingdom')
  )

df_combined <- df_titles %>% left_join(world, by = c('country_name' = 'admin')) %>%
  filter(country %in% c('India', 'USA', 'UK')) %>%
  mutate(
    n_count = count / max(count)
  )


map <- leaflet(df_combined) %>% addProviderTiles(providers$CartoDB.Positron)

for (i in 1:nrow(df_combined)) {
  if (df_combined[[i, 'country']] == 'USA') {
    lng_begin <- -112.461674
    lat_begin <- 45.67955
    color <- 'Blue'
  } else if (df_combined[[i, 'country']] == 'UK') {
    lng_begin <- -2.865632	
    lat_begin <- 54.12387
    color <- 'Red'
  } else if (df_combined[[i, 'country']] == 'India') {
    lng_begin <- 79.61198
    lat_begin <- 22.88578
    color <- 'Orange'
  }
  map <- addPolylines(map,
                      lng = c(lng_begin, df_combined[[i, 'X']]),
                      lat = c(lat_begin, df_combined[[i, 'Y']]),
                      weight = df_combined[[i, 'n_count']] * 6,
                      color = color,
                      label = paste0(
                        '<b>Country of origin: </b>', df_combined[[i, 'country']], '<br>',
                        '<b>Netflix catalog: </b>', df_combined[[i, 'country_name']]
                        
                      )
                      )
}
map 

df_india <- titles %>% filter(IN == TRUE) %>%
  mutate(
    local = ifelse(str_detect(country, 'India|Bangladesh|Pakistan'), TRUE, FALSE)
  ) %>% 
  drop_na(country) %>%
  select(title, local, rating) %>%
  mutate(
    rating = str_extract(rating, '.*/'),
    rating = str_remove(rating, '/'),
    rating = as.numeric(rating)
  )

ggplot(df_india, aes(x=rating, color=local)) +
  geom_histogram(fill="white")

library("ggpubr")
ggboxplot(df_india, x = "local", y = "rating", 
          color = "local", palette = c("#00AFBB", "#E7B800"),
          # order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")
