# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(sf)

library(terra)

library(sampbias)

library(performance)

library(broom)

library(flextable)

library(tidytext)

library(ggbeeswarm)

library(ggview)

library(segmented)

library(tidyterra)

library(patchwork)

library(spdep)

# Registros de ocorrência ----

## Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar os dados ----

registros <- purrr::map_dfr(
  c("gbif",
    "sibbr",
    "specieslink",
    "inaturalist",
    "herpetohelp"),
  \(reg){

    "repteis_caatinga" |>
      googledrive::drive_get() |>
      googledrive::drive_ls() |>
      dplyr::filter(name == paste0("registros_",
                                   reg,
                                   ".xlsx")) |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx() |>
      dplyr::mutate(source = reg) |>
      dplyr::rename("decimalLongitude" = longitude,
                    "decimalLatitude" = latitude)

    },
  .progress = TRUE)

## Visualizar dados ----

registros

registros |> dplyr::glimpse()

# Shapefile da Caatinga ----

## Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

## Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = registros,
             aes(Longitude, Latitude, color = order))

# Gazetteers ----

## Importar ----

gaz <- "repteis_caatinga" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name  |> stringr::str_detect("^gaz_")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(gaz){

      gaz |>
        googledrive::drive_download(file = _,
                                    path = tempfile(fileext = ".shp")) |>
        dplyr::pull(local_path) |>
        sf::st_read()

      },
    .progress = TRUE) |>
  setNames("repteis_caatinga" |>
             googledrive::drive_get() |>
             googledrive::drive_ls() |>
             dplyr::filter(name  |> stringr::str_detect("^gaz_")) |>
             dplyr::pull(name) |>
             stringr::str_remove_all("gaz_|.gpkg"))

## Visualizar ----

gaz

purrr::map(
  gaz,
  \(gaz){

    ggplot() ++
      geom_sf(data = caa) +
      geom_sf(data = gaz)

    },
  .progress = TRUE)

# Calcular viés ----

## Criar molde ----

caa_rast <- terra::rast(caa |>
                          terra::vect() |>
                          terra::ext(),
                        resolution = (10 / 111.3194),
                        crs = "EPSG:4326") |>
  terra::rasterize(caa |>
                     terra::vect(),
                   y = _,
                   field = 1)

caa_rast

caa_rast |> plot()

