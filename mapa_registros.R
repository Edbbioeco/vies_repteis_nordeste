# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(sf)

library(smoothr)

library(units)

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

## Figras ----

### Importar ----

figuras <- tibble::tibble(figuras = list.files(path = "figuras",
                                               pattern = ".png$",
                                               full.names = TRUE),
                          order = list.files(path = "figuras",
                                             pattern = ".png$") |>
                            stringr::str_remove_all("figura_|.png") |>
                            forcats::fct_relevel(c("Crocodylia",
                                                   "Testudines",
                                                   "Squamata")),
                          longitude = -36,
                          latitude = -14)

### Visualizar ----

figuras

figuras |> dplyr::glimpse()

# Mapa ----

ggplot() +
  geom_sf(data = br, color = "black", linewidth = 1,
          aes(fill = "Brazil")) +
  geom_sf(data = caa, color = "black", linewidth = 1,
          aes(fill = "Caatinga")) +
  geom_sf(data = registros |>
            dplyr::mutate(order = order |>
                            forcats::fct_relevel(c("Crocodylia",
                                                   "Testudines",
                                                   "Squamata"))) |>
            sf::st_as_sf(coords = c(5:6),
                         crs = caa |> sf::st_crs()),
          aes(color = "Species records")) +
  facet_wrap(~order, ncol = 2) +
  coord_sf(xlim = caa |> sf::st_bbox() %>% .[c(1, 3)],
           ylim = caa |> sf::st_bbox() %>% .[c(2, 4)]) +
  scale_fill_manual(breaks = c("Brazil", "Caatinga", "Species records"),
                    values = c("gray", "gold", "black"),
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = "black") +
  ggspatial::annotation_scale(location = "br",
                              tex_color = "black",
                              text_cex = 1.5,
                              text_face = "bold",
                              bar_cols = c("black", "gold")) +
  labs(fill = NULL,
       color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 17.5, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.position = c(0.7, 0.25),
        legend.key = element_blank(),
        strip.text = element_text(size = 15, color = "black"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1),
        panel.background = element_rect(linewidth = 1,
                                        color = "black"))

ggsave(filename = "mapa_registros.png",
       height = 12, width = 12)

"mapa_registros.png" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "mapa_registros.png",
                            overwrite = TRUE)
