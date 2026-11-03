# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(sf)

library(smoothr)

library(units)

library(terra)

library(sampbias)

library(broom)

library(flextable)

library(tidytext)

library(ggbeeswarm)

library(ggimage)

library(segmented)

library(tidyterra)

library(patchwork)

library(spdep)

# Registros de ocorrência ----

## Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar os dados ----

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
      dplyr::mutate(source = nome |>
                      stringr::str_remove_all("registros_|.xlsx")) |>
      dplyr::rename("decimalLongitude" = Longitude,
                    "decimalLatitude" = Latitude)

    },
  .progress = TRUE)

## Visualizar dados ----

registros

registros |> dplyr::glimpse()

# Shapefile do Brasil ----

## Importar ----

br <- sf::st_read("biomas.gpkg") |>
  sf::st_make_valid() |>
  sf::st_union() |>
  smoothr::fill_holes(threshold = units::set_units(1e6, "km2"))

## Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")

# Shapefile da Caatinga ----

## Importar ----

caa <- sf::st_read("biomas.gpkg") |>
  dplyr::filter(name_biome == "Caatinga")

## Visualizar ----

caa

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_point(data = registros,
             aes(decimalLongitude, decimalLatitude, color = order))

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
  purrr::set_names("repteis_caatinga" |>
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

    gaz |> plot()

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
  purrr::set_names(c("Crocodylia", "Testudines", "Squamata"))

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
                                media = modelo,
                                overwrite = TRUE)

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
  purrr::set_names(c("Crocodylia", "Testudines", "Squamata"))

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

## Data Frame das figuras ----

df_figuras <- tibble::tibble(figura = list.files(path = "./figuras",
                                                 pattern = "^figura_.*\\.png$",
                                                 full.names = TRUE),
                             Order = list.files(path = "./figuras",
                                                pattern = "^figura_.*\\.png$") |>
                               stringr::str_remove_all("figura_|.png") |>
                               stringr::str_to_title() |>
                               forcats::fct_relevel(c("Crocodylia",
                                                      "Testudines",
                                                      "Squamata")))

df_figuras

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
  ggimage::geom_image(data = df_figuras |>
                        dplyr::mutate(`Bias weight` = 0.9,
                                      Factor = 3),
                      aes(`Bias weight`, Factor, image = figura),
                      size = 0.75)+
  facet_wrap(~Order, ncol = 1, scales = "free_y") +
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

ggsave(filename = "grafico_distribuição_pesos.png", height = 12, width = 12)

"grafico_distribuição_pesos.png" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "grafico_distribuição_pesos.png",
                            overwrite = TRUE)

# Taxa de amostragem por MCMC ----

## Valor máximo de distância geodésica possível na Caatinga ----

dist_caa <- caa |>
  sf::st_boundary() |>
  sf::st_cast("POINT") |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::arrange(dplyr::desc(Y)) |>
  dplyr::slice(c(1, dplyr::n())) |>
  sf::st_as_sf(coords = c(1:2), crs = 4674) |>
  sf::st_distance() |>
  max() |>
  as.numeric() / 1e3

dist_caa

## Valore médios de estimativas dos modelos ----

medias_vies <- purrr::map(
  modelos_vies,
  \(modelo){

    modelo |>
      purrr::pluck("bias_estimate") |>
      colMeans()

    },
  .progress = TRUE)

medias_vies

## Vetor de distância ----

dist_caa_vetor <- seq(0, dist_caa, 1)

dist_caa_vetor

## Data frame dos valores preditos de sampling rate ----

df_sr <- purrr::imap_dfr(
  medias_vies,
  \(medias, ordem){

    purrr::map(
      5:9,
      \(vetor){

        nome <- medias[vetor] |> names()

        tibble::tibble(
          `Min distance to the factor (km)` = dist_caa_vetor,
          `Factor sampling rate` = medias[["q"]] * exp(-medias[[nome]] * `Min distance to the factor (km)`),
          Factor = nome |> stringr::str_remove("w_"),
          Order = ordem
        )

        }
    )

    },
  .progress = TRUE)

df_sr

## Média dos valores de sampling rate ----

df_sr |>
  dplyr::summarise(media = `Factor sampling rate` |> mean(),
                   sd = `Factor sampling rate` |> sd(),
                   .by = Order)

## Modelos de segmentação ----

### Criar os modelos ----

modelos_seg <- purrr::pmap(
    tidyr::expand_grid(
      ordem = c("Crocodylia",
                "Testudines",
                "Squamata"),
      gaz = df_sr$Factor |> unique()),
    \(ordem, gaz){

    dados <- df_sr |>
      dplyr::filter(Order == ordem & Factor == gaz) |>
      dplyr::rename(sampling_rate = `Factor sampling rate`,
                    dist = `Min distance to the factor (km)`)

    lm(sampling_rate ~ dist, data = dados) |>
      segmented::davies.test(seg.Z = ~dist, k = 10)

    },
  .progress = TRUE) |>
  purrr::set_names(
    tidyr::expand_grid(
      ordem = c("Crocodylia",
                "Testudines",
                "Squamata"),
      gaz = df_sr$Factor |> unique()) |>
      dplyr::mutate(nome = paste0(ordem, "_", gaz)) |>
      dplyr::pull(nome))

modelos_seg

### Exportar modelos ----

purrr::imap(
  modelos_seg,
  \(modelo, id){

    modelo |> readr::write_rds(paste0("modelo_", id, "_seg.rds"))

    },
  .progress = TRUE)

### Subir para o Google Drive ----

purrr::map(
  list.files(pattern = "_seg.rds$"),
  \(modelo){

    modelo |>
      googledrive::drive_upload(path = "repteis_caatinga",
                                name = modelo,
                                overwrite = TRUE)

    },
  .progress = TRUE)

### Importar do Google Drive ----

modelos_seg <- purrr::map(
  "repteis_caatinga" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name |> stringr::str_detect("_seg.rds$")) |>
    dplyr::pull(name),
  \(modelo){

    modelo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".rds")) |>
      dplyr::pull(local_path) |>
      readr::read_rds()

    },
  .progress = TRUE) |>
  purrr::set_names("repteis_caatinga" |>
                     googledrive::drive_get() |>
                     googledrive::drive_ls() |>
                     dplyr::filter(name |> stringr::str_detect("_seg.rds$")) |>
                     dplyr::pull(name) |>
                     stringr::str_remove_all("modelo_|_seg.rds"))

modelos_seg

## Modelos de quebra ----

### Criar os modelos ----

modelos_quebra <- purrr::pmap(
  tidyr::expand_grid(
    ordem = c("Crocodylia",
              "Testudines",
              "Squamata"),
    gaz = df_sr$Factor |> unique()),
  \(ordem, gaz){

    dados <- df_sr |>
      dplyr::filter(Order == ordem & Factor == gaz) |>
      dplyr::rename(sampling_rate = `Factor sampling rate`,
                    dist = `Min distance to the factor (km)`)

    lm(sampling_rate ~ dist, data = dados) |>
      segmented::selgmented(seg.Z = ~dist, Kmax = 10, type = "bic")

  },
  .progress = TRUE) |>
  purrr::set_names(
    tidyr::expand_grid(
      ordem = c("Crocodylia",
                "Testudines",
                "Squamata"),
      gaz = df_sr$Factor |> unique()) |>
      dplyr::mutate(nome = paste0(ordem, "_", gaz)) |>
      dplyr::pull(nome))

modelos_quebra

### Exportar modelos ----

purrr::imap(
  modelos_quebra,
  \(modelo, id){

    modelo |> readr::write_rds(paste0("modelo_", id, "_quebra.rds"))

  },
  .progress = TRUE)

### Subir para o Google Drive ----

purrr::map(
  list.files(pattern = "_quebra.rds$"),
  \(modelo){

    modelo |>
      googledrive::drive_upload(path = "repteis_caatinga",
                                name = modelo,
                                overwrite = TRUE)

    },
  .progress = TRUE)

### Importar do Google Drive ----

modelos_quebra <- purrr::map(
  "repteis_caatinga" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name |> stringr::str_detect("_quebra.rds$")) |>
    dplyr::pull(name),
  \(modelo){

    modelo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".rds")) |>
      dplyr::pull(local_path) |>
      readr::read_rds()

  },
  .progress = TRUE) |>
  purrr::set_names("repteis_caatinga" |>
                     googledrive::drive_get() |>
                     googledrive::drive_ls() |>
                     dplyr::filter(name |> stringr::str_detect("_quebra.rds$")) |>
                     dplyr::pull(name) |>
                     stringr::str_remove_all("modelo_|_quebra.rds"))

modelos_quebra

## Estatísticas dos modelos ----

sts_dist_davies <- purrr::pmap_dfr(
  list(modelos_seg,
       modelos_quebra,
       modelos_seg |> names()),
  \(seg, quebra, nome){

    seg |>
      broom::tidy() |>
      dplyr::rename("M" = 1,
                    "p value" = 2) |>
      dplyr::select(-c(3:5)) |>
      dplyr::mutate(Model = nome |>
                      stringr::str_replace("\\.", " "),
                    .before = 1,
                    M = M |> round(2)) |>
      dplyr::mutate(`p value` = dplyr::case_when(

        `p value` < 0.01 ~ "< 0.01",
        .default = `p value` |> round(2) |> as.character()
      ),
      `Distance` = quebra$psi[1, 2] |> round(2)) |>
      tidyr::separate(col = Model,
                      into = c("Order", "Factor"),
                      sep = "_")

    },
  .progress = TRUE)

sts_dist_davies

## Média da menor distância pelo teste de Davies ----

### Por ordem ----

sts_dist_davies |>
  dplyr::summarise(media = Distance |> mean() |> round(2),
                   sd = Distance |> sd() |> round(2),
                   .by = Order)
### Por fator ----

sts_dist_davies |>
  dplyr::summarise(media = Distance |> mean() |> round(2),
                   sd = Distance |> sd() |> round(7),
                   .by = Factor) |>
  dplyr::arrange(media)

## Tabela flextable ----

sts_dist_davies_flex <- sts_dist_davies |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all")

sts_dist_davies_flex

sts_dist_davies_flex |>
  flextable::save_as_docx(path = "sts_dist_davies.docx")

"sts_dist_davies.docx" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "sts_dist_davies.docx",
                            overwrite = TRUE)

## Gráfico ----

df_sr |>
  dplyr::mutate(Order = Order |>
                  forcats::fct_relevel(c("Crocodylia",
                                         "Testudines",
                                         "Squamata")),
                Factor = Factor |> stringr::str_replace("\\.",
                                                        " ")) |>
  ggplot(aes(`Min distance to the factor (km)`, `Factor sampling rate`,
             color = Factor)) +
  geom_line(linewidth = 2) +
  geom_vline(data = sts_dist_davies |>
               dplyr::mutate(Order = Order |>
                               forcats::fct_relevel(c("Crocodylia",
                                                      "Testudines",
                                                      "Squamata"))),
             aes(xintercept = Distance, color = Factor),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_manual(breaks = c("rios",
                                "uc",
                                "eolica",
                                "rod",
                                "urb"),
                     values = c("blue",
                                "forestgreen",
                                "gold4",
                                "black",
                                "orange")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  ggimage::geom_image(data = df_figuras |>
                        dplyr::mutate(`Min distance to the factor (km)` = 1300,
                                      `Factor sampling rate` = c(0.075,
                                                                 35,
                                                                 1)),
                      aes(`Min distance to the factor (km)`,
                          `Factor sampling rate`,
                          image = figura),
                      inherit.aes = FALSE,
                      size = 0.75) +
  facet_wrap(~Order, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1500, 150),
                     expand = c(0.01, 1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
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
        plot.subtitle = element_text(size = 17.5, color = "black"))

ggsave(filename = "grafico_sampling_rate.png", height = 12, width = 12)

"grafico_sampling_rate.png" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "grafico_sampling_rate.png",
                            overwrite = TRUE)

# Projeção espacial ----

## Calcular projeção ----

rast_proj <- purrr::map(
  modelos_vies,
  \(modelo){

    modelo |>
      sampbias::project_bias() |>
      terra::mask(caa) |>
      terra::crop(caa)

    },
  .progress = TRUE)

rast_proj

purrr::map(
  rast_proj,
  \(raster){

    ggplot() +
      tidyterra::geom_spatraster(data = raster[[1:5]]) +
      facet_wrap(~lyr) +
      scale_fill_viridis_c(na.value = "transparent")

    },
  .progress = TRUE)

## Calcular valores mínimos, máximos e médios de sampling rate ----

purrr::imap_dfr(
  rast_proj,
  \(raster, ordem){

    tibble::tibble(Ordem = ordem,
                   min = raster |>
                     tidyterra::select(
                       -dplyr::contains(c("percent",
                                          "occ"))) |>
                     terra::values() |>
                     na.omit() |>
                     min(),
                   max = raster |>
                     tidyterra::select(
                       -dplyr::contains(c("percent",
                                          "occ"))) |>
                     terra::values() |>
                     na.omit() |>
                     max(),
                   media = raster |>
                     tidyterra::select(
                       -dplyr::contains(c("percent",
                                          "occ"))) |>
                     terra::values() |>
                     na.omit() |>
                     mean(),
                   sd = raster |>
                     tidyterra::select(
                       -dplyr::contains(c("percent",
                                          "occ"))) |>
                     terra::values() |>
                     na.omit() |>
                     sd())

  },
  .progress = TRUE)

## Mapas ----

purrr::imap(
  rast_proj,
  \(raster, ordem){

    ggplot() +
      tidyterra::geom_spatraster(data = raster |>
                                   tidyterra::select(
                                     -dplyr::contains(c("percent",
                                                        "occ")))) +
      scale_fill_viridis_c(option = "inferno",
                           na.value = "transparent",
                           guide = guide_colourbar(
                             title.hjust = 0.5,
                             barheight = 12,
                             frame.colour = "black",
                             ticks.colour = "black",
                             ticks.linewidth = 0.5)) +
      facet_wrap(~lyr) +
      labs(title = paste0("Order: ", ordem),
           fill = "Sampling rate") +
      theme_bw() +
      theme(axis.text = element_text(size = 10, color = "black"),
            legend.text = element_text(size = 15, color = "black"),
            legend.title = element_text(size = 15, color = "black"),
            legend.position = c(0.8, 0.25),
            strip.text = element_text(size = 12.5, color = "black"),
            strip.background = element_rect(color = "black",
                                            linewidth = 1),
            panel.background = element_rect(linewidth = 1,
                                            color = "black"),
            plot.title = element_text(size = 20, color = "black", face = "bold"),
            plot.subtitle = element_text(size = 17.5, color = "black")) +
      ggview::canvas(height = 10, width = 12)

  },
  .progress = TRUE) |>
  patchwork::wrap_plots() +
  patchwork::plot_layout(ncol = 2)

ggsave("mapas_projecao.png",
       height = 15, width = 15)

"mapas_projecao.png" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "mapas_projecao.png",
                            overwrite = TRUE)

## Agregação espacial ----

### Criar janela ----

janela <- caa |>
  sf::st_make_grid(cellsize = (10 / 111.3194)) |>
  sf::st_as_sf() |>
  sf::st_filter(
    y = caa,
    .predicate = sf::st_intersects
  ) |>
  spdep::poly2nb() |>
  spdep::nb2listw(style = "W")

janela

### Calcular I de Moran ----

moran_orden_var <- purrr::imap(
  rast_proj,
  \(raster, ordem){

    variavel <- raster |>
      terra::names() |>
      stringr::str_subset("Total_percentage|occurrences",
                          negate = TRUE)

    purrr::map(
      variavel,
      \(var){

        valores <- raster[[var]] |>
          terra::values(mat = FALSE)

        celulas <- which(!is.na(valores))

        janela <- raster |>
          terra::xyFromCell(celulas) |>
          spdep::knearneigh(k = 8) |>
          spdep::knn2nb() |>
          spdep::nb2listw(styl = "W")

        spdep::moran.mc(valores[celulas],
                        janela,
                        nsim = 10000)

        }
      ) |>
      purrr::set_names(paste0(ordem, "_", variavel))

    },
  .progress = TRUE) |>
  purrr::flatten()

moran_orden_var

### Data frame dos valores de I de Moran ----

df_moran <- purrr::imap_dfr(
  moran_orden_var,
  \(teste, nome){

    tibble::tibble(Model = nome,
                   "Moran's I" = teste$statistic |>
                     round(2),
                   "Rank" = teste$parameter,
                   "p" = teste$p.value)

    },
  .progress = TRUE) |>
  dplyr::mutate(p = dplyr::case_when(
    p < 0.01 ~ "< 0.01",
    .default = p |> round(2) |> as.character()
  ),
  Model = Model |>
    stringr::str_replace("\n", " ")) |>
  tidyr::separate(col = Model,
                  into = c("Order", "Factor interaction"),
                  sep = "_")

df_moran

### Tabela flextable ----

tabela_moran <- df_moran |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = 2, width = 2) |>
  flextable::width(j = 3, width = 1)

tabela_moran

tabela_moran |>
  flextable::save_as_docx(path = "tabela_moran.docx")

"tabela_moran.docx" |>
  googledrive::drive_upload(path = "repteis_caatinga",
                            name = "tabela_moran.docx",
                            overwrite = TRUE)

## Calcular Moran Médio por ordem ----

df_moran |>
  dplyr::summarise(Media = `Moran's I` |> mean() |> round(2),
                   sd = `Moran's I` |> sd() |> round(2),
                   .by = Order)
