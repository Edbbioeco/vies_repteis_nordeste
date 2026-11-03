# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(sf)

library(ggspatial)

# Dados ----

## Registros de ocorrência ----

### Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

### Importar ----

tempdir() |>
  dir.create(showWarnings = FALSE, recursive = TRUE)

registros <- purrr::pmap_dfr(
  "repteis_caatinga" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name |> stringr::str_detect("^registros_")) |>
    dplyr::select(id, name) |>
    dplyr::rename(arquivo = 1,
                  nome = 2),
  \(arquivo, nome){

    arquivo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx() |>
      dplyr::mutate(Fonte = nome |>
                      stringr::str_remove_all("registros_|.xlsx"))

    },
  .progress = TRUE)

### Visualizar ----

registros

registros |> dplyr::glimpse()

## Shapefile do Brasil ----

### Importar ----

br <- sf::st_read("biomas.gpkg") |>
  sf::st_make_valid() |>
  sf::st_union() |>
  smoothr::fill_holes(threshold = units::set_units(1e6, km2))

### Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Shapefile da Caatinga ----

### Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

### Visualizar ----

caa

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = caa, color = "black", fill = "gold")

