# Do peso calibrado aos arquivos de divulgacao.
#
# Escreve sete arquivos em ondas/<onda>/output/:
#
#   <prefixo>.xlsx             Stratification, Results e ResultsStrat
#   <prefixo>_decimal.xlsx     as mesmas abas sem arredondamento
#   <prefixo>_N.xlsx           as mesmas abas com o N nao ponderado
#   <prefixo>_localidades.xlsx entrevistas por municipio
#   <prefixo>_microdados.xlsx  uma linha por respondente, com o peso calibrado
#   diagnostico-margens.csv    margem ponderada vs cota, por categoria
#   ambiente.txt               versoes, margens usadas e o resultado da onda
#
# Nenhum vai para o git: output/ esta no .gitignore. O de microdados tambem nao
# pode ser publicado -- ver a politica de dados no README.

# ==============================================================================
# ESTIMATIVAS
# ==============================================================================

estimate_question <- function(questao, desenho, nivel_confianca = 0.95) {

  if (!questao %in% names(desenho$variables)) {
    warning("variavel nao encontrada: ", questao)
    return(NULL)
  }

  respostas <- desenho$variables[[questao]]
  validas <- !is.na(respostas) & trimws(as.character(respostas)) != ""

  if (!any(validas)) {
    warning("nenhuma resposta valida para: ", questao)
    return(NULL)
  }

  sub <- desenho[validas, ]

  sub$variables$.resposta <- factor(
    as.character(sub$variables[[questao]]),
    levels = if (is.factor(respostas)) levels(droplevels(respostas[validas]))
             else unique(as.character(respostas[validas]))
  )

  est <- survey::svymean(~.resposta, design = sub, na.rm = TRUE)
  ic <- stats::confint(est, level = nivel_confianca)

  tibble::tibble(
    question = questao,
    response = stringr::str_remove(names(stats::coef(est)), "^\\.resposta"),
    mean = as.numeric(stats::coef(est)),
    conf_low = as.numeric(ic[, 1]),
    conf_high = as.numeric(ic[, 2])
  )
}

estimate_svyby <- function(questao, recorte, desenho, nivel_confianca = 0.95) {

  ausentes <- setdiff(c(questao, recorte), names(desenho$variables))
  if (length(ausentes) > 0) {
    warning("variaveis nao encontradas: ", paste(ausentes, collapse = ", "))
    return(NULL)
  }

  valores <- desenho$variables[[questao]]
  estratos <- desenho$variables[[recorte]]

  validas <- !is.na(valores) & trimws(as.character(valores)) != "" &
    !is.na(estratos) & trimws(as.character(estratos)) != ""

  if (!any(validas)) return(NULL)

  sub <- desenho[validas, ]

  niveis <- function(x) {
    if (is.factor(x)) levels(droplevels(x[validas]))
    else unique(as.character(x[validas]))
  }

  sub$variables$.resposta <- factor(as.character(sub$variables[[questao]]),
                                    levels = niveis(valores))
  sub$variables$.estrato <- factor(as.character(sub$variables[[recorte]]),
                                   levels = niveis(estratos))

  resultado <- survey::svyby(
    formula = ~.resposta, by = ~.estrato, design = sub, FUN = survey::svymean,
    na.rm = TRUE, vartype = c("se", "ci"), level = nivel_confianca,
    keep.names = FALSE, drop.empty.groups = TRUE
  ) %>%
    tibble::as_tibble()

  colunas <- names(resultado)[stringr::str_detect(names(resultado),
                                                  "^\\.resposta")]

  purrr::map_dfr(colunas, function(coluna) {
    tibble::tibble(
      question = questao,
      response = stringr::str_remove(coluna, "^\\.resposta"),
      stratification_variable = recorte,
      stratification_category = as.character(resultado$.estrato),
      mean = as.numeric(resultado[[coluna]]),
      conf_low = as.numeric(resultado[[paste0("ci_l.", coluna)]]),
      conf_high = as.numeric(resultado[[paste0("ci_u.", coluna)]])
    )
  })
}

formatar_percentuais <- function(tabela, decimais = 0) {

  tabela <- dplyr::mutate(
    tabela,
    mean_percent = mean * 100,
    conf_low_percent = conf_low * 100,
    conf_high_percent = conf_high * 100
  )

  if (!is.null(decimais)) {
    fmt <- paste0("%.", decimais, "f%% (%.", decimais, "f%%–%.", decimais,
                  "f%%)")
    tabela <- dplyr::mutate(
      tabela,
      dplyr::across(dplyr::ends_with("_percent"), ~ round(.x, decimais)),
      estimate_ci = sprintf(fmt, mean_percent, conf_low_percent,
                            conf_high_percent)
    )
  }

  dplyr::select(tabela, -mean, -conf_low, -conf_high)
}

# ==============================================================================
# AS TRES ABAS
# ==============================================================================

niveis_declarados <- function(qst) {

  nomes <- unique(c(names(qst$derivadas), names(qst$questoes)))

  purrr::map(purrr::set_names(nomes),
             ~ unlist(qst$derivadas[[.x]]$niveis %||% qst$questoes[[.x]]$niveis))
}

montar_results <- function(desenho, ordem, metadados, nivel) {

  purrr::map_dfr(ordem, estimate_question, desenho = desenho,
                 nivel_confianca = nivel) %>%
    dplyr::mutate(question = factor(question, levels = ordem, ordered = TRUE)) %>%
    dplyr::arrange(question) %>%
    dplyr::mutate(question = as.character(question)) %>%
    dplyr::left_join(metadados, by = c("question" = "question_id")) %>%
    dplyr::rename(question_id = question) %>%
    dplyr::select(question_id, question_text, response, mean, conf_low,
                  conf_high)
}

montar_results_strat <- function(desenho, ordem, recortes, niveis, nivel) {

  posicao <- function(lista, valor) {
    if (is.null(lista)) NA_integer_ else match(valor, lista)
  }

  tidyr::crossing(question = ordem, stratification_variable = recortes) %>%
    purrr::pmap_dfr(function(question, stratification_variable) {
      estimate_svyby(question, stratification_variable, desenho, nivel)
    }) %>%
    dplyr::mutate(
      question = factor(question, levels = ordem, ordered = TRUE),
      stratification_variable = factor(stratification_variable,
                                       levels = recortes, ordered = TRUE),
      response_order = purrr::map2_int(as.character(question), response,
                                       ~ posicao(niveis[[.x]], .y)),
      stratification_order = purrr::map2_int(
        as.character(stratification_variable), stratification_category,
        ~ posicao(niveis[[.x]], .y)
      )
    ) %>%
    dplyr::arrange(question, stratification_variable, response_order,
                   stratification_order) %>%
    dplyr::mutate(
      question = as.character(question),
      stratification_variable = as.character(stratification_variable)
    ) %>%
    dplyr::select(question, response, stratification_variable,
                  stratification_category, mean, conf_low, conf_high)
}

# ==============================================================================
# N NAO PONDERADO
# ==============================================================================

contar_localidades <- function(desenho) {

  geo <- c("regiao_arquivo", "uf", "municipio", "bairro")
  faltando <- setdiff(geo, names(desenho$variables))
  if (length(faltando) > 0) {
    stop("coluna de geografia ausente na base: ",
         paste(faltando, collapse = ", "))
  }

  desenho$variables %>%
    dplyr::mutate(bairro = dplyr::coalesce(bairro, "(nao informado)")) %>%
    dplyr::count(dplyr::across(dplyr::all_of(geo)), name = "n_entrevistas") %>%
    dplyr::arrange(dplyr::desc(n_entrevistas), uf, municipio, bairro) %>%
    dplyr::select(regiao = regiao_arquivo, estado = uf, municipio, bairro,
                  n_entrevistas)
}

contar_ns <- function(desenho, ordem, recortes) {

  dados <- desenho$variables

  valida <- function(x) !is.na(x) & trimws(as.character(x)) != ""

  por_questao <- function(questao, recorte = NULL) {

    if (!all(c(questao, recorte) %in% names(dados))) return(NULL)

    ok <- valida(dados[[questao]])
    if (!is.null(recorte)) ok <- ok & valida(dados[[recorte]])
    if (!any(ok)) return(NULL)

    fora <- dados[ok, c(questao, recorte), drop = FALSE]
    names(fora) <- c("response", if (!is.null(recorte)) "stratification_category")

    fora <- dplyr::count(fora, dplyr::across(dplyr::everything()), name = "n",
                         .drop = FALSE)

    if (!is.null(recorte)) fora <- dplyr::group_by(fora,
                                                  stratification_category)

    fora %>%
      dplyr::mutate(n_base = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character),
                    question = questao,
                    stratification_variable = recorte %||% NA_character_)
  }

  colunas <- function(x, ...) dplyr::select(x, ...)

  list(
    Stratification = purrr::map_dfr(recortes, por_questao) %>%
      colunas(question, response, n, n_base),
    Results = purrr::map_dfr(ordem, por_questao) %>%
      colunas(question_id = question, response, n, n_base),
    ResultsStrat = tidyr::crossing(questao = ordem, recorte = recortes) %>%
      purrr::pmap_dfr(function(questao, recorte) por_questao(questao, recorte)) %>%
      colunas(question, response, stratification_variable,
              stratification_category, n, n_base)
  )
}

# ==============================================================================
# MICRODADOS PONDERADOS
# ==============================================================================

# peso: soma = populacao dos alvos. peso_norm: peso / media, soma = n.
montar_microdados <- function(desenho, qst) {

  fora <- desenho$variables
  pesos <- as.numeric(stats::weights(desenho))

  fora$peso <- pesos
  fora$peso_norm <- pesos / mean(pesos)

  frente <- c("respondent_id", "regiao_arquivo", "uf", "municipio", "bairro",
              "tipo_municipio")

  colunas <- unique(c(
    intersect(frente, names(fora)),
    "peso", "peso_norm",
    intersect(purrr::map_chr(qst$estratificacao, "variavel"), names(fora)),
    intersect(unlist(qst$exportar_ordem), names(fora)),
    names(fora)
  ))

  fora[, colunas] %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character)) %>%
    tibble::as_tibble()
}

dicionario_microdados <- function(qst, colunas) {

  perguntas <- c(qst$questoes, qst$questoes_nao_exportadas)
  metadados <- unlist(qst$colunas)

  niveis_de <- function(spec) {
    if (length(spec$niveis) == 0) return(NA_character_)
    paste(unlist(spec$niveis), collapse = " | ")
  }

  origens_de <- function(spec) {
    vars <- unlist(c(spec$origem, spec$origem_voto,
                     spec$filtro_comparecimento$variavel,
                     purrr::map(spec$ordem_avaliacao, "variavel")))
    if (length(vars) == 0) NA_character_ else
      paste("derivada de", paste(unique(vars), collapse = ", "))
  }

  descrever <- function(v) {

    if (v == "peso") {
      return(c("peso", "peso calibrado (soma = populacao dos alvos)",
               NA_character_))
    }
    if (v == "peso_norm") {
      return(c("peso", "peso / media (soma = n calibrado)", NA_character_))
    }
    if (v %in% metadados) {
      return(c("metadado", names(metadados)[match(v, metadados)],
               NA_character_))
    }
    if (!is.null(perguntas[[v]])) {
      spec <- perguntas[[v]]
      tipo <- if (is.null(spec$harmonizada_de)) "questao" else
        paste("harmonizada de", spec$harmonizada_de)
      return(c(tipo, spec$titulo %||% spec$texto %||% NA_character_,
               niveis_de(spec)))
    }
    if (!is.null(qst$derivadas[[v]])) {
      spec <- qst$derivadas[[v]]
      return(c(paste0("derivada (", spec$tipo, ")"), origens_de(spec),
               niveis_de(spec)))
    }

    c("(nao declarada)", NA_character_, NA_character_)
  }

  campos <- purrr::map(colunas, descrever)

  tibble::tibble(
    variavel = colunas,
    tipo = purrr::map_chr(campos, 1),
    enunciado = purrr::map_chr(campos, 2),
    niveis = purrr::map_chr(campos, 3)
  )
}

# ==============================================================================
# SAIDA
# ==============================================================================

exportar_onda <- function(fit, qst, cfg) {

  dir_saida <- cfg$caminhos$output
  dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)

  recortes <- purrr::map_chr(qst$estratificacao, "variavel")
  niveis <- niveis_declarados(qst)
  ordem <- unlist(qst$exportar_ordem)
  nivel <- cfg$diagnosticos$nivel_confianca

  metadados <- tibble::tibble(
    question_id = names(qst$questoes),
    question_text = purrr::map_chr(qst$questoes,
                                   ~ .x$titulo %||% .x$texto)
  )

  cru <- list(
    Stratification = purrr::map_dfr(recortes, estimate_question,
                                    desenho = fit$design,
                                    nivel_confianca = nivel),
    Results = montar_results(fit$design, ordem, metadados, nivel),
    ResultsStrat = montar_results_strat(fit$design, ordem, recortes, niveis,
                                        nivel)
  )

  tabelas <- purrr::map(cru, formatar_percentuais)

  xlsx <- file.path(dir_saida, paste0(cfg$outputs$prefixo, ".xlsx"))
  writexl::write_xlsx(tabelas, path = xlsx)

  writexl::write_xlsx(purrr::map(cru, formatar_percentuais, decimais = NULL),
                      path = file.path(dir_saida,
                                       paste0(cfg$outputs$prefixo,
                                              "_decimal.xlsx")))

  ns <- contar_ns(fit$design, ordem, recortes)
  writexl::write_xlsx(ns, path = file.path(
    dir_saida, paste0(cfg$outputs$prefixo, "_N.xlsx")))

  localidades <- contar_localidades(fit$design)
  writexl::write_xlsx(list(Localidades = localidades), path = file.path(
    dir_saida, paste0(cfg$outputs$prefixo, "_localidades.xlsx")))

  microdados <- if (isFALSE(cfg$outputs$microdados)) NULL else {
    montar_microdados(fit$design, qst)
  }

  if (!is.null(microdados)) {
    writexl::write_xlsx(
      list(Microdados = microdados,
           Dicionario = dicionario_microdados(qst, names(microdados))),
      path = file.path(dir_saida,
                       paste0(cfg$outputs$prefixo, "_microdados.xlsx")))
  }

  diagnostico <- convergence_report(fit) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 6)))

  readr::write_csv(diagnostico, file.path(dir_saida, "diagnostico-margens.csv"))
  escrever_ambiente(fit, cfg, diagnostico, dir_saida)

  cat(sprintf("\nescrito em %s/\n  %s.xlsx  (%d + %d + %d linhas)\n",
              dir_saida, cfg$outputs$prefixo, nrow(tabelas$Stratification),
              nrow(tabelas$Results), nrow(tabelas$ResultsStrat)))
  cat(sprintf("  %s_decimal.xlsx  (as mesmas abas sem arredondamento)\n",
              cfg$outputs$prefixo))
  cat(sprintf("  %s_N.xlsx  (o N nao ponderado de cada estimativa)\n",
              cfg$outputs$prefixo))
  cat(sprintf("  %s_localidades.xlsx  (%d municipios)\n", cfg$outputs$prefixo,
              nrow(localidades)))
  if (is.null(microdados)) {
    cat("  (sem microdados: outputs.microdados = false)\n")
  } else {
    cat(sprintf("  %s_microdados.xlsx  (%d respondentes x %d colunas)\n",
                cfg$outputs$prefixo, nrow(microdados), ncol(microdados)))
  }
  cat("  diagnostico-margens.csv\n  ambiente.txt\n")

  invisible(c(tabelas, list(Microdados = microdados)))
}

escrever_ambiente <- function(fit, cfg, diagnostico, dir_saida) {

  pacotes <- c("dplyr", "purrr", "readr", "readxl", "stringr", "survey",
               "tibble", "tidyr", "writexl", "yaml")

  ef <- design_effect(fit$design)
  desvio <- max(abs(diagnostico$desvio_pp))

  dentro <- desvio <= cfg$diagnosticos$tolerancia_pp &&
    ef$moe_pp <= cfg$diagnosticos$moe_maxima_pp

  veredito <- sprintf(
    "%s (desvio %.2f de %.1f pp, margem de erro %.2f de %.1f pp)",
    if (dentro) "ATENDE" else "NAO ATENDE", desvio,
    cfg$diagnosticos$tolerancia_pp, ef$moe_pp, cfg$diagnosticos$moe_maxima_pp
  )

  if (!dentro) {
    warning("a onda nao atende os criterios declarados: ", veredito,
            call. = FALSE)
  }

  writeLines(
    c(
      sprintf("onda: %s", cfg$onda$nome %||% cfg$onda$slug),
      sprintf("registro: %s", cfg$onda$registro),
      sprintf("data_divulgacao: %s", cfg$onda$data_divulgacao),
      sprintf("gerado_em: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("R: %s (%s)", getRversion(), R.version$platform),
      "",
      "pacotes:",
      sprintf("  %-8s %s", pacotes,
              vapply(pacotes, function(p) as.character(packageVersion(p)), "")),
      "",
      "margens:",
      sprintf("  %s", cfg$margens$pnadc),
      sprintf("  %s", cfg$margens$tse %||% "(sem margem de voto)"),
      "",
      "resultado:",
      sprintf("  amostra registrada : %s",
              cfg$amostra$registrada %||% "(sem corte)"),
      sprintf("  n                  : %d", fit$n_calibrado),
      sprintf("  n efetivo (Kish)   : %.0f", ef$n_eff),
      sprintf("  efeito de desenho  : %.2f", ef$deff),
      sprintf("  peso maximo        : %.1fx a media", ef$razao_max),
      sprintf("  margem de erro     : +/- %.2f pp", ef$moe_pp),
      sprintf("  desvio max vs cota : %.4f pp", desvio),
      sprintf("  aparo              : %s",
              if (isTRUE(fit$trimming$aplicado))
                sprintf("teto %gx a media", cfg$trimming$teto) else "nenhum"),
      sprintf("  criterios          : %s", veredito)
    ),
    file.path(dir_saida, "ambiente.txt")
  )
}
