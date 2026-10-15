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

gbif <- readr::read_tsv(caminho,
                        quote = "",
                        na = "")

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

gbif |>
  dplyr::filter(!decimalLongitude |> is.na()) |>
  dplyr::pull(decimalLongitude) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

gbif |>
  dplyr::filter(!decimalLongitude |> is.na()) |>
  ggplot(aes(decimalLongitude)) +
  geom_histogram()

### Gráfico ----

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = gbif |>
               dplyr::filter(!decimalLongitude |> is.na() &
                               !decimalLatitude |> is.na()),
             aes(decimalLongitude, decimalLatitude))

## Transformar os registros em shapefile ----

gbif_sf <- gbif |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = 4674)

gbif_sf

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = gbif_sf)

## Recortar a área para a Caatinga ----

gbif_sf_crop <- gbif_sf |>
  sf::st_intersection(caa)

gbif_sf_crop

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = gbif_sf_crop)
