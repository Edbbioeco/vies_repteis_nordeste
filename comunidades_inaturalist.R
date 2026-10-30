# Pacotes ----

library(googledrive)

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
  dplyr::filter(name |> stringr::str_detect("^inaturalist")) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".csv")) |>
  dplyr::pull(local_path)

inaturalist <- readr::read_csv(caminho)


### Visualizar dados ----

inaturalist

inaturalist |> dplyr::glimpse()

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

inaturalist |>
  dplyr::filter(!latitude |> is.na()) |>
  dplyr::pull(latitude) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

inaturalist |>
  dplyr::filter(!latitude |> is.na()) |>
  ggplot(aes(latitude)) +
  geom_histogram()

inaturalist |>
  dplyr::filter(!longitude |> is.na()) |>
  dplyr::pull(longitude) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

inaturalist |>
  dplyr::filter(!longitude |> is.na()) |>
  ggplot(aes(longitude)) +
  geom_histogram()

### Gráfico ----

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = inaturalist |>
               dplyr::filter(!longitude |> is.na() &
                               !latitude |> is.na()),
             aes(longitude, latitude))

## Transformar os registros em shapefile ----

inaturalist_sf <- inaturalist |>
  dplyr::filter(!longitude |> is.na() &
                  !latitude |> is.na() &
                  !taxon_order_name |> is.na()) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4674)

inaturalist_sf

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = inaturalist_sf, aes(color = taxon_order_name))
