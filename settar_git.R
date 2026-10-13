# Pacotes ----

library(usethis)

library(gert)

# Iniciar git ----

usethis::use_git()

# Primeiro commit ----

gert::git_add(files = ".gitignore")

gert::git_commit(message = "Firts commit")

# Criar repositório ----

usethis::use_github()

