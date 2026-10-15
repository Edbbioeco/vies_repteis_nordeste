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

gbif <- readr::read_csv2(caminho)

## Visualizar dados ----

gbif

gbif |> dplyr::glimpse()
