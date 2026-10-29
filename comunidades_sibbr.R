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
  dplyr::filter(name |> stringr::str_detect("^sibbr")) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".csv")) |>
  dplyr::pull(local_path)

sibbr <- readr::read_csv(caminho,
                          col_types = readr::cols(.default = readr::col_character()),
                          show_col_types = FALSE)

### Visualizar dados ----

sibbr

sibbr |> dplyr::glimpse()

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

sibbr |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!`Decimal latitude (WGS84)` |> is.na()) |>
  dplyr::pull(`Decimal latitude (WGS84)`) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

sibbr |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!`Decimal latitude (WGS84)` |> is.na()) |>
  ggplot(aes(`Decimal latitude (WGS84)`)) +
  geom_histogram()

sibbr |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!`Decimal longitude (WGS84)` |> is.na()) |>
  dplyr::pull(`Decimal longitude (WGS84)`) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

sibbr |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric())) |>
  dplyr::filter(!`Decimal longitude (WGS84)` |> is.na()) |>
  ggplot(aes(`Decimal longitude (WGS84)`)) +
  geom_histogram()
