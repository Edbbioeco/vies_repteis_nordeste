# Pacotes ----

library(googledrive)

library(tidyverse)

library(sf)

library(ggspatial)

# Dados ----

## Registros de ocorrência ----

### Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

### Importar ----

tempdir() |>
  dir.create(showWarnings = FALSE, recursive = TRUE)

registros <- purrr::map_dfr(
  "repteis_caatinga" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name |> stringr::str_detect("^registros_")) |>
    dplyr::pull(id),
  \(arquivo){

    arquivo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx() |>
      dplyr::mutate(Fonte = arquivo |>
                      stringr::str_remove_all("registros_|.xlsx"))

    },
  .progress = TRUE)
