library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(networkD3)

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
  filter(!str_detect(country_name, '\\+')) %>%
  rename(coo = country, netflix = country_name) %>%
  mutate(
    coo = str_replace(coo, 'USA', 'United States'),
    netflix = str_replace(netflix, 'UK', 'United Kingdom')
  )

shinyUI(
  fluidPage(
    titlePanel('Netflix Catalog Explorer', windowTitle = 'Netflix Catalog'),
    sidebarPanel(
      selectInput('coo',
                  'Country of production',
                  choices = c('', unique(df_titles$coo))
                  ),
      selectInput('cat',
                  'Catalog',
                  choices = c('', unique(df_titles$netflix))
                  )
    ),
    mainPanel(
      leafletOutput('map'),
      sankeyNetworkOutput('sankey')
    )
  )
)
