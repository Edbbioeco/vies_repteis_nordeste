# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

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
  dplyr::filter(name |> stringr::str_detect("speciesLink")) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path)

specieslink <- readxl::read_xlsx(caminho)

### Visualizar dados ----

specieslink

specieslink |> dplyr::glimpse()

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

specieslink |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!latitude |> is.na()) |>
  dplyr::pull(latitude) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

specieslink |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!latitude |> is.na()) |>
  ggplot(aes(latitude)) +
  geom_histogram()

specieslink |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!longitude |> is.na()) |>
  dplyr::pull(longitude) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

specieslink |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!longitude |> is.na()) |>
  ggplot(aes(longitude)) +
  geom_histogram()

### Gráfico ----

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = gbif |>
               dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                                           .fns = ~.x |> as.numeric())) |>
               dplyr::filter(!longitude |> is.na() &
                               !latitude |> is.na()),
             aes(longitude, latitude))

## Transformar os registros em shapefile ----

specieslink_sf <- specieslink |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric()))|>
  dplyr::filter(!longitude |> is.na() &
                  !latitude |> is.na()) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4674)

specieslink_sf

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = specieslink_sf)

## Recortar a área para a Caatinga ----

specieslink_sf_crop <- specieslink_sf |>
  sf::st_intersection(caa)

specieslink_sf_crop

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = specieslink_sf_crop)

## Listar espécies ----

specieslink_sf_crop |>
  dplyr::filter(!scientificname |> is.na()) |>
  dplyr::pull(scientificname) |>
  unique()
