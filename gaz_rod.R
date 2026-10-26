# Pacotes ----

library(sf)

library(tidyverse)

library(sf)

library(googledrive)

# Shapefile da caatinga ----

## Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

## Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black")

# Shapefile das rodovias ----

## Importar ----

rod <- list.files(pattern = "^GEOFT_.*\\.shp$") |>
  purrr::map_dfr(
    \(arquivo){

      sf::st_read(arquivo)

      },
    .progress = TRUE)

## Visualizar ----

rod

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = rod, color = "black")

# Tratar os shapefiles ----

## Recortar shapefile ----

rod_caa <- rod |>
  sf::st_intersection(caa)

rod_caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = rod_caa, color = "red")
