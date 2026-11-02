# Pacotes ----

library(tidyverse)

library(purrr)

# Remover fundo ----

imagens <- purrr::map(
  list.files(path = "./figuras",
             pattern = ".jpeg$",
             full.names = TRUE),
  \(figuras){

    figuras |>
      magick::image_read() |>
      magick::image_transparent(color = "white",
                                fuzz = 25)

    },
  .progress = TRUE) |>
  purrr::set_names(list.files(path = "./figuras",
                              pattern = ".jpeg") |>
                     stringr::str_remove(".jpeg$"))

imagens

purrr::map(
  1:3,
  \(id){

    imagens[[1]]

    },
  .progress = TRUE)
