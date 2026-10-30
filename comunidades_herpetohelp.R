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

## Listar espécies ----

herpetohelp_sf |>
  dplyr::filter(Ordem %notin% c("Anura", "Gymnophiona")) |>
  dplyr::pull(Espécie) |>
  unique()

## Excluir espécies de fora da Caatinga ----

herpetohelp_sf <- herpetohelp_sf |>
  dplyr::filter(!Espécie %in% c(
    "Amphisbaena bedai",
    "Chironius scurrula",
    "Helicops nentur",
    "Phalotris mertensi",
    "Micrurus paraensis",
    "Rhachisaurus brachylepis",
    "Vanzosaura rubricauda",
    "Cnemidophorus cryptus",
    "Bothrops atrox",
    "Bothrops marmoratus"
  ))

herpetohelp_sf

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = herpetohelp_sf |>
            dplyr::filter(Ordem %notin% c("Anura", "Gymnophiona")),
          aes(color = Ordem))

## Extrair as coordenadas ----

herpetohelp_sf_coord <- herpetohelp_sf |>
  dplyr::filter(Ordem %notin% c("Anura", "Gymnophiona")) |>
  dplyr::mutate(sf::st_coordinates(geometry) |>
                  as.data.frame() |>
                  dplyr::rename("Longitude" = X,
                                "Latitude" = Y),
                genus = Espécie |> stringr::word(1)) |>
  dplyr::select(order = Ordem,
                family = Família,
                genus,
                species = Espécie,
                Longitude,
                Latitude) |>
  sf::st_drop_geometry()

herpetohelp_sf_coord

## Exportar data frame ----

herpetohelp_sf_coord |> writexl::write_xlsx("registros_herpetohelp.xlsx")
