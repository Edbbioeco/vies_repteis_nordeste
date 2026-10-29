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

### Gráfico ----

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = sibbr |>
               dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                                           .fns = ~.x |> as.numeric())) |>
               dplyr::filter(!`Decimal longitude (WGS84)` |> is.na() &
                               !`Decimal latitude (WGS84)` |> is.na()),
             aes(`Decimal longitude (WGS84)`, `Decimal latitude (WGS84)`))

## Transformar os registros em shapefile ----

sibbr_sf <- sibbr |>
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~.x |> as.numeric()))|>
  dplyr::filter(!`Decimal longitude (WGS84)` |> is.na() &
                  !`Decimal latitude (WGS84)` |> is.na() &
                  !Order |> is.na()) |>
  sf::st_as_sf(coords = c("Decimal longitude (WGS84)", "Decimal latitude (WGS84)"),
               crs = 4674)

sibbr_sf

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = sibbr_sf, aes(color = Order))

## Recortar a área para a Caatinga ----

sibbr_sf_crop <- sibbr_sf |>
  sf::st_intersection(caa)

sibbr_sf_crop

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = sibbr_sf_crop, aes(color = Order))

## Listar espécies ----

sibbr_sf_crop |>
  dplyr::filter(!Species |> is.na()) |>
  dplyr::pull(Species) |>
  unique()

## Excluir espécies de fora da Caatinga ----

sibbr_sf_crop <- sibbr_sf_crop |>
  dplyr::filter(!Species %in% c(
    "Salvator rufescens",
    "Vanzosaura rubricauda",
    "Lachesis muta",
    "Loxopholis guianense",
    "Bothrops atrox",
    "Chironius scurrula",
    "Phrynops tuberosus",
    "Podocnemis expansa",
    "Chatogekko amazonicus",
    "Mabuya bistriata",
    "Plica umbra",
    "Lygophis lineatus",
    "Arthrosaura reticulata",
    "Lepidoblepharis heyerorum",
    "Xenodon severus",
    "Melanosuchus niger",
    "Uranoscodon superciliosus",
    "Pseudogonatodes guianensis",
    "Thecadactylus rapicauda",
    "Clelia clelia",
    "Bothrops brazili",
    "Cercosaura ocellata",
    "Micrurus spixii",
    "Micrurus frontalis",
    "Anisolepis grilli",
    "Bothrops fonsecai",
    "Thamnodynastes strigatus",
    "Ameivula cipoensis",
    "Boiruna maculata",
    "Cercosaura schreibersii"
    ))

sibbr_sf_crop

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = sibbr_sf_crop, aes(color = Order))

## Extrair as coordenadas ----

sibbr_sf_coord <- sibbr_sf_crop |>
  dplyr::mutate(sf::st_coordinates(geometry) |>
                  as.data.frame() |>
                  dplyr::rename("Longitude" = X,
                                "Latitude" = Y)) |>
  dplyr::select(order = Order,
                family = Family,
                genus = Genus,
                species = Species,
                Longitude,
                Latitude) |>
  sf::st_drop_geometry()

sibbr_sf_coord
