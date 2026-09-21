# ondas/<onda>/: config.yaml, questionario.yaml, display.yaml, dados/, output/.

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
})

if (!file.exists("DESCRIPTION")) {
  stop("rode a partir da raiz do repositorio (a pasta com DESCRIPTION).\n",
       "diretorio atual: ", getwd())
}

source("R/calibracao.R")
source("R/resultados.R")
source("R/propensao.R")

ler_yaml <- function(caminho) {

  preservar <- function(x) {
    if (tolower(x) %in% c("true", "false")) as.logical(toupper(x)) else x
  }

  yaml::read_yaml(caminho,
                  handlers = list(`bool#yes` = preservar,
                                  `bool#no` = preservar))
}

carregar_config <- function(onda) {

  pasta <- file.path("ondas", onda)

  if (!dir.exists(pasta)) {
    stop("onda nao encontrada: ", pasta, "\nondas disponiveis: ",
         paste(basename(list.dirs("ondas", recursive = FALSE)),
               collapse = ", "))
  }

  cfg <- ler_yaml(file.path(pasta, "config.yaml"))

  faltando <- setdiff(c("registro", "data_divulgacao"), names(cfg$onda))
  if (length(faltando) > 0) {
    stop("config.yaml de ", onda, ": bloco `onda` sem ",
         paste(faltando, collapse = " e "), ".")
  }

  # duas divulgacoes no mesmo dia precisam de pastas distintas: data + onda
  if (!startsWith(onda, cfg$onda$data_divulgacao)) {
    stop("a pasta da onda deve comecar pela data de divulgacao ",
         "(AAAA-MM-DD-onda-N).\n",
         "  pasta           : ", onda, "\n",
         "  data_divulgacao : ", cfg$onda$data_divulgacao)
  }

  arquivos <- cfg$dados$arquivo %||%
    list.files(file.path(pasta, "dados"), pattern = "[.]xlsx$")

  if (length(arquivos) != 1) {
    stop(length(arquivos), " arquivos .xlsx em ", pasta, "/dados. ",
         "Declare qual usar em `dados.arquivo` do config.yaml.")
  }

  cfg$onda$slug <- onda
  cfg$caminhos <- list(
    questionario = file.path(pasta, "questionario.yaml"),
    dados = file.path(pasta, "dados", arquivos),
    display = file.path(pasta, "display.yaml"),
    output = file.path(pasta, "output")
  )

  cfg
}

rodar_onda <- function(onda) {

  cfg <- carregar_config(onda)

  qst <- carregar_questionario(cfg$caminhos$questionario)
  base <- montar_respondentes(ler_bruto(cfg$caminhos$dados, qst), qst)
  base <- cortar_amostra(base, cfg$amostra)

  municipios <- carregar_municipios()
  base <- padronizar_geografia(base, municipios)

  cat(sprintf("\nonda %s | registro %s | %d respondentes\n",
              cfg$onda$nome %||% onda, cfg$onda$registro, nrow(base)))

  # toda fonte de `margens` fica disponivel; `calibracao.margens` diz qual entra
  fontes <- purrr::map(cfg$margens, carregar_margens)

  propensao <- NULL
  if (isTRUE(cfg$propensao$ativo)) {
    if (is.null(fontes$pnadc)) {
      stop("propensao.ativo exige margens.pnadc no config.yaml", call. = FALSE)
    }
    p <- cfg$propensao
    propensao <- montar_propensao(base, fontes$pnadc$tabela,
                                  unlist(p$variaveis),
                                  p$semente %||% 1234, p$arvores %||% 1000,
                                  p$no_minimo %||% 20, p$peso_fallback %||% 1)
    base <- propensao$base
  }

  alvos <- montar_alvos(fontes, cfg$calibracao$margens)

  fit <- rake_weights(base, alvos, cfg$calibracao$tolerancia,
                      cfg$calibracao$max_iteracoes)

  fit$propensao <- propensao$diagnostico

  fit <- trim_weights(fit, cfg$trimming$teto, cfg$trimming$piso,
                      cfg$trimming$strict)

  estrutura <- exportar_onda(fit, qst, cfg)

  invisible(c(list(cfg = cfg, base = base, fit = fit), estrutura))
}
