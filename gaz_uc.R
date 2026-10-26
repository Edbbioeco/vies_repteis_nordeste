# Pacotes ----

library(sf)

library(tidyverse)

library(geobr)

library(googledrive)

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

## Visualizar ----

uc

ggplot() +
  geom_sf(data = uc, color = "black")

# Tratar os shapefiles ----

## Recortar shapefile ----

uc_caa <- uc |>
  sf::st_intersection(caa)

uc_caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = uc_caa, color = "red")

## Exportar ----

uc_caa |> sf::st_write("gaz_uc.gpkg")

## Enviar para o Google Drive ----

"gaz_uc.gpkg" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "gaz_uc.gpkg")
