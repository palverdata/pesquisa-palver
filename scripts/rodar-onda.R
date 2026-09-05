# Rodar uma onda.

onda <- "2026-08-10"

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

source("R/onda.R")

resultado <- rodar_onda(onda)

# `resultado` fica no ambiente: $crosstabs, $sample, $base, $fit$design.
