# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Dados de ocorrência ----

### Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

### Importar dados ----

caminho <- "repteis_caatinga" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("gbif")) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".csv")) |>
  dplyr::pull(local_path)

gbif <- readr::read_csv2(caminho)

## Visualizar dados ----

gbif

gbif |> dplyr::glimpse()

## Shapefile da caatinga ----

### Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

### Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black")

# Recortar os registros para a Caatinga ----

## Tratar as coordenadas ----

### Checar a aquantidade de dígitos de latitude e longitude ----

gbif |>
  dplyr::filter(!decimalLatitude |> is.na()) |>
  dplyr::pull(decimalLatitude) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

gbif |>
  dplyr::filter(!decimalLatitude |> is.na()) |>
  ggplot(aes(decimalLatitude)) +
  geom_histogram()
