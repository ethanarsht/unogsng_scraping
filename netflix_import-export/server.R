library(tidyverse)
library(rnaturalearth)
library(sf)
library(leaflet)
titles <- read_csv('all_details_26-5.csv', guess_max = 10000)

df_titles <- titles %>%
  select(clist, country) %>%
  drop_na(country) %>%
  separate_rows(clist, sep = ',') %>%
  mutate(
    country_name = str_extract(clist, '(?<=\\:").*'),
    country_name = str_remove(country_name, '"'),
    country = str_replace(country, 'USA', 'United States'),
    country = str_replace(country, 'UK', 'United Kingdom')
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
  mutate(
    n_count = count / max(count)
  ) %>%
  left_join(world, by = c('country' = 'admin')) %>%
  rename(
    catalog_X = X.x,
    catalog_Y = Y.x,
    coo_X = X.y,
    coo_Y = Y.y
  ) %>%
  drop_na(coo_X, coo_Y, catalog_X, catalog_Y) %>%
  rename(catalog = country_name, coo = country)

shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
      leaflet(df_combined) %>%
        addTiles() %>%
        addPolylines(map,
                     lat = ~c(coo_Y, catalog_Y),
                     lng = ~c(coo_X, catalog_X),
                     weight =  ~n_count*60,
                     popup = paste0(
                       '<b>Country of origin: </b>', df_combined['coo'], '<br>',
                       '<b>Netflix catalog: </b>', df_combined['catalog']
                     )
        )
  })
    observeEvent({input$coo
                  input$cat
    },
                 {
                   req(input$coo)
                   df_coo <- df_combined %>% 
                     filter(coo == input$coo) %>%
                     drop_na()
                   
                   print(df_coo)
                   
                   leafletProxy('map') %>%
                     clearShapes()
                   
                   for (i in seq(1, nrow(df_coo))) {
                     
                     print(i)
                   
                   leafletProxy('map') %>%
                     addPolylines(
                       lat = c(df_coo[[i, 'coo_Y']], df_coo[[i, 'catalog_Y']]),
                       lng = c(df_coo[[i, 'coo_X']], df_coo[[i, 'catalog_X']]),
                       weight = df_coo[[i, 'n_count']]*10
                     )
                   }
                 }
              )
    observeEvent({input$cat
                  input$coo
    },
                 {
                   req(input$cat)
                   df_cat <- df_combined %>% 
                     filter(catalog == input$cat) %>%
                     drop_na()
                   
                   print(df_cat)
                   
                   leafletProxy('map') %>%
                     clearShapes()
                   
                   for (i in seq(1, nrow(df_cat))) {
                     
                     print(i)
                     
                     leafletProxy('map') %>%
                       addPolylines(
                         lat = c(df_cat[[i, 'coo_Y']], df_cat[[i, 'catalog_Y']]),
                         lng = c(df_cat[[i, 'coo_X']], df_cat[[i, 'catalog_X']]),
                         weight = df_cat[[i, 'n_count']]*10
                       )
                   }
                 })
    observeEvent({input$coo
                  input$cat},
                 {
                   if (input$coo != '' & input$cat != '') {
                   
                     df_both <- df_combined %>%
                       filter(catalog == input$cat & coo == input$coo)
                     
                     leafletProxy('map') %>%
                       clearShapes()
                     
                     for (i in seq(1, nrow(df_both))) {
                       
                       print(i)
                       
                       leafletProxy('map') %>%
                         addPolylines(
                           lat = c(df_both[[i, 'coo_Y']], df_both[[i, 'catalog_Y']]),
                           lng = c(df_both[[i, 'coo_X']], df_both[[i, 'catalog_X']]),
                           weight = df_both[[i, 'n_count']]*10
                         )
                     }
                   }
                 })
})