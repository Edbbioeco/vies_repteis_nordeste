# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(sf)

library(terra)

library(sampbias)

library(performance)

library(broom)

library(flextable)

library(tidytext)

library(ggbeeswarm)

library(ggview)

library(segmented)

library(tidyterra)

library(patchwork)

library(spdep)

# Registros de ocorrência ----

## Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar os dados ----

registros <- purrr::map_dfr(
  c("gbif",
    "sibbr",
    "specieslink",
    "inaturalist",
    "herpetohelp"),
  \(reg){

    "repteis_caatinga" |>
      googledrive::drive_get() |>
      googledrive::drive_ls() |>
      dplyr::filter(name == paste0("registros_",
                                   reg,
                                   ".xlsx")) |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx() |>
      dplyr::mutate(source = reg) |>
      dplyr::rename("decimalLongitude" = Longitude,
                    "decimalLatitude" = Latitude)

    },
  .progress = TRUE)

## Visualizar dados ----

registros

registros |> dplyr::glimpse()

# Shapefile da Caatinga ----

## Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

## Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = registros,
             aes(Longitude, Latitude, color = order))

# Gazetteers ----

## Importar ----

gaz <- "repteis_caatinga" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name  |> stringr::str_detect("^gaz_")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(gaz){

      gaz |>
        googledrive::drive_download(file = _,
                                    path = tempfile(fileext = ".shp")) |>
        dplyr::pull(local_path) |>
        terra::vect()

      },
    .progress = TRUE) |>
  setNames("repteis_caatinga" |>
             googledrive::drive_get() |>
             googledrive::drive_ls() |>
             dplyr::filter(name  |> stringr::str_detect("^gaz_")) |>
             dplyr::pull(name) |>
             stringr::str_remove_all("gaz_|.gpkg"))

## Visualizar ----

gaz

purrr::map(
  gaz,
  \(gaz){

    ggplot() +
      geom_sf(data = caa) +
      geom_sf(data = gaz |>
                sf::st_as_sf())

    },
  .progress = TRUE)

# Calcular viés ----

## Criar molde ----

caa_rast <- terra::rast(caa |>
                          terra::vect() |>
                          terra::ext(),
                        resolution = (10 / 111.3194),
                        crs = "EPSG:4326") |>
  terra::rasterize(caa |>
                     terra::vect(),
                   y = _,
                   field = 1)

caa_rast

caa_rast |> plot()

## Calcular viés por ordem ----

vies_ordem <- purrr::map(
  c("Crocodylia", "Testudines", "Squamata"),
  \(ordem){

    registros |>
      dplyr::filter(order == ordem) |>
      sampbias::calculate_bias(gaz = gaz,
                               inp_raster = caa_rast,
                               res = (10 / 111.3194),
                               terrestrial = TRUE)

  },
  .progress = TRUE) |>
  setNames(c("Crocodylia", "Testudines", "Squamata"))

vies_ordem

## Exportar os modelos ----

purrr::imap(
  vies_ordem,
  \(modelo, ordem){

    modelo$summa$extent <- modelo$summa$extent |> as.vector()

    modelo$occurrences <- modelo$occurrences |> terra::wrap()

    modelo$distance_rasters <- modelo$distance_rasters |> terra::wrap()

    modelo |>
      readr::write_rds(file = paste0("modelo_vies_", ordem, ".rds"))

  },
  .progress = TRUE)

## Subir para o Google Drive ----

purrr::map(
  list.files(pattern = "modelo_vies_"),
  \(modelo){

    modelo |>
      googledrive::drive_upload(path = "repteis_caatinga",
                                media = modelo)

    },
  .progress = TRUE)

## Importar modelos ----

modelos_vies <- purrr::map(
  c("Crocodylia", "Testudines", "Squamata"),
  \(ordem){

    modelo <- paste0("modelo_vies_", ordem, ".rds") |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".rds")) |>
      dplyr::pull(local_path) |>
      readr::read_rds()

    modelo$summa$extent <- modelo$summa$extent |> terra::ext()

    modelo$occurrences <- modelo$occurrences |> terra::unwrap()

    modelo$distance_rasters <- modelo$distance_rasters |> terra::unwrap()

    modelo

    },
  .progress = TRUE) |>
  setNames(c("Crocodylia", "Testudines", "Squamata"))

modelos_vies

# Pesos dos modelos ----

## Criar data frame ----

df_pesos <- purrr::imap_dfr(
  modelos_vies,
  \(modelo, ordem){

    modelo$bias_estimate |>
      tidyr::pivot_longer(cols = dplyr::contains("w_"),
                          names_to = "Factor",
                          values_to = "Bias weight") |>
      dplyr::mutate(Factor = Factor |>
                      stringr::str_remove("w_") |>
                      stringr::str_replace_all("\\.",
                                               " "),
                    Order = ordem)

    },
  .progress = TRUE)

df_pesos

## Calcular médias ----

### Sem fator ----

df_pesos |>
  dplyr::summarise(media = `Bias weight` |> mean() |> round(2),
                   sd = `Bias weight` |> sd() |> round(2),
                   .by = Order)

### Com fator ----

df_pesos_media <- df_pesos |>
  dplyr::summarise(sd = `Bias weight` |> sd(),
                   `Bias weight` = `Bias weight` |> mean(),
                   .by = c(Order, Factor)) |>
  dplyr::mutate(Order = Order |>
                  forcats::fct_relevel(c("Crocodylia",
                                         "Testudines",
                                         "Squamata")),
                Factor = tidytext::reorder_within(Factor, `Bias weight`, Order))

df_pesos_media

## Criar gráfico ----

df_pesos |>
  dplyr::mutate(Order = Order |>
                  forcats::fct_relevel(c("Crocodylia",
                                         "Testudines",
                                         "Squamata")),
                Factor = tidytext::reorder_within(Factor, `Bias weight`, Order)) |>
  ggplot(aes(`Bias weight`, Factor)) +
  ggbeeswarm::geom_quasirandom() +
  geom_point(data = df_pesos_media,
             aes(`Bias weight`, Factor),
             size = 2.5,
             color = "red") +
  geom_errorbar(data = df_pesos_media,
                aes(xmin = `Bias weight` - sd,
                    xmax = `Bias weight` + sd,
                    y = Factor),
                linewidth = 1,
                color = "red") +
  facet_wrap(~Order, ncol = 1, scales = "free") +
  tidytext::scale_y_reordered() +
  labs(y = "Gazetteer") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 30, color = "black"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1),
        panel.background = element_rect(linewidth = 1,
                                        color = "black"),
        plot.title = element_text(size = 20, color = "black"),
        plot.subtitle = element_text(size = 17.5, color = "black"),
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5))
