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

# Shapefile das unidades de conservação ----

## Importar ----

uc <- geobr::read_conservation_units(date = 202503) |>
  sf::st_make_valid()
