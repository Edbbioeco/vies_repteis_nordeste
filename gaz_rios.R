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

rios <- sf::st_read("geoft_bho_curso_dagua.gpkg")

## Visualizar ----

rios

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = rios, color = "black")

# Tratar os shapefiles ----

## Recortar shapefile ----

rios_caa <- rios |>
  sf::st_intersection(caa)

rios_caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = rios_caa, color = "blue")

## Exportar ----

rios_caa |> sf::st_write("gaz_rios.gpkg")

## Enviar para o Google Drive ----

"gaz_rios.gpkg" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "gaz_rios.gpkg")
