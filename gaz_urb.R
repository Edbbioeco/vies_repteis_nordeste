# Pacotes ----

library(sf)

library(tidyverse)

library(geobr)

# Shapefile da caatinga ----

## Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

## Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black")

# Shapefile das áreas urbanas ----

## Importar ----

urb <- geobr::read_urban_area(year = 2022)
