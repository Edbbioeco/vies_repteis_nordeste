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

## Visualizar ----

urb

ggplot() +
  geom_sf(data = urb, color = "black")

# Tratar os shapefiles ----

## Recortar shapefile ----

urb_caa <- urb |>
  sf::st_intersection(caa)

urb_caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = urb_caa, color = "red")

## Exportar ----

urb_caa |> sf::st_write("gaz_urb.gpkg")
