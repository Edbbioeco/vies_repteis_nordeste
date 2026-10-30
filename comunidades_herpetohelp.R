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
  dplyr::filter(name |> stringr::str_detect("^herpetohelp")) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path)

herpetohelp <- readxl::read_xlsx(caminho,
                                 sheet = 2)

### Visualizar dados ----

herpetohelp

herpetohelp |> dplyr::glimpse()

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

herpetohelp |>
  dplyr::filter(!`Latitude original` |> is.na()) |>
  dplyr::pull(`Latitude original`) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

herpetohelp |>
  dplyr::filter(!`Latitude original` |> is.na()) |>
  ggplot(aes(`Latitude original`)) +
  geom_histogram()

herpetohelp |>
  dplyr::filter(!`Longitude original` |> is.na()) |>
  dplyr::pull(`Longitude original`) |>
  as.character() |>
  nchar() |>
  unique() |>
  sort()

herpetohelp |>
  dplyr::filter(!`Longitude original` |> is.na()) |>
  ggplot(aes(`Longitude original`)) +
  geom_histogram()

### Gráfico ----

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = herpetohelp,
             aes(`Longitude original`, `Latitude original`))

## Transformar os registros em shapefile ----

herpetohelp_sf <- herpetohelp |>
  sf::st_as_sf(coords = c("Longitude original", "Latitude original"),
               crs = 4674)

herpetohelp_sf

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = herpetohelp_sf |>
            dplyr::filter(Ordem %notin% c("Anura", "Gymnophiona")),
          aes(color = Ordem))
