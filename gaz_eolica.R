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

# Shapefile das usinas eólicas ----

## Importar ----

eol <- sf::st_read("Aerogeradores.shp")

## Visualizar ----

eol

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = eol, color = "black")
