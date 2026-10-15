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

## Listar espécies ----

gbif_sf_crop |>
  dplyr::filter(!species |> is.na()) |>
  dplyr::pull(species) |>
  unique()

## Excluir espécies de fora da Caatinga ----

gbif_sf_crop <- gbif_sf_crop |>
  dplyr::filter(!species %in% c(
    "Mauremys leprosa",
    "Crocodylus porosus",
    "Lygodactylus conraui",
    "Cynisca leucura",
    "Santanaraptor placidus",
    "Trachemys scripta",
    "Tropidurus bogerti",
    "Apostolepis mariae",
    "Mabuya mabouya",
    "Salvator rufescens",
    "Amphisbaena eleutheria",
    "Erythrolamprus cobella",
    "Anolis chrysolepis",
    "Norops chrysolepis",
    "Pseudogonatodes guianensis",
    "Cercosaura ocellata",
    "Bothrops atrox",
    "Uranoscodon superciliosus",
    "Thecadactylus rapicauda",
    "Mabuya bistriata",
    "Lygophis lineatus",
    "Lachesis muta",
    "Phrynops tuberosus",
    "Bothrops brazili",
    "Clelia clelia",
    "Melanosuchus niger",
    "Podocnemis expansa",
    "Xenodon severus",
    "Cnemidophorus cryptus",
    "Chironius scurrula",
    "Micrurus paraensis",
    "Lepidoblepharis heyerorum",
    "Plica umbra",
    "Chatogekko amazonicus",
    "Loxopholis guianense",
    "Arthrosaura reticulata",
    "Vanzosaura rubricauda",
    "Tropidurus guarani",
    "Ameivula cipoensis",
    "Helicops nentur",
    "Bothrops fonsecai",
    "Urostrophus grilli",
    "Atractus pantostictus",
    "Cercosaura schreibersii",
    "Boiruna maculata",
    "Micrurus frontalis",
    "Thamnodynastes strigatus"))

gbif_sf_crop

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = gbif_sf_crop)

## Extrair as coordenadas ----

gbif_sps_coord <- gbif_sf_crop |>
  dplyr::mutate(sf::st_coordinates(geometry) |>
                  as.data.frame() |>
                  dplyr::rename("Longitude" = X,
                                "Latitude" = Y)) |>
  dplyr::select(order:species, dplyr::contains("tude")) |>
  sf::st_drop_geometry()

gbif_sps_coord

## Exportar data frame ----

gbif_sps_coord |> writexl::write_xlsx("registros_gbif.xlsx")
