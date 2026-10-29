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

inaturalist <- readr::read_csv(caminho,
                               col_types = readr::cols(.default = readr::col_character()),
                               show_col_types = FALSE)
