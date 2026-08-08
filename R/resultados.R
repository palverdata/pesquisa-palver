# Do peso calibrado aos arquivos de divulgacao.
#
# Escreve cinco arquivos em ondas/<onda>/output/:
#
#   <prefixo>.xlsx             Stratification, Results e ResultsStrat
#   <prefixo>_N.xlsx           as mesmas abas com o N nao ponderado
#   <prefixo>_localidades.xlsx entrevistas por municipio
#   diagnostico-margens.csv    margem ponderada vs cota, por categoria
#   ambiente.txt               versoes, margens usadas e o resultado da onda

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

formatar_percentuais <- function(tabela) {
  tabela %>%
    dplyr::mutate(
      mean_percent = round(mean * 100, 0),
      conf_low_percent = round(conf_low * 100, 0),
      conf_high_percent = round(conf_high * 100, 0),
      estimate_ci = sprintf("%.0f%% (%.0f%%–%.0f%%)", mean_percent,
                            conf_low_percent, conf_high_percent)
    )
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
    formatar_percentuais() %>%
    dplyr::mutate(question = as.character(question)) %>%
    dplyr::left_join(metadados, by = c("question" = "question_id")) %>%
    dplyr::rename(question_id = question) %>%
    dplyr::select(question_id, question_text, response, mean_percent,
                  conf_low_percent, conf_high_percent, estimate_ci)
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
    formatar_percentuais() %>%
    dplyr::arrange(question, stratification_variable, response_order,
                   stratification_order) %>%
    dplyr::mutate(
      question = as.character(question),
      stratification_variable = as.character(stratification_variable)
    ) %>%
    dplyr::select(question, response, stratification_variable,
                  stratification_category, mean_percent, conf_low_percent,
                  conf_high_percent, estimate_ci)
}

# ==============================================================================
# N NAO PONDERADO
# ==============================================================================

contar_localidades <- function(desenho) {

  geo <- c("regiao_arquivo", "uf", "municipio", "tipo_municipio")
  faltando <- setdiff(geo, names(desenho$variables))
  if (length(faltando) > 0) {
    stop("coluna de geografia ausente na base: ",
         paste(faltando, collapse = ", "))
  }

  desenho$variables %>%
    dplyr::count(dplyr::across(dplyr::all_of(geo)), name = "n_entrevistas") %>%
    dplyr::arrange(dplyr::desc(n_entrevistas), uf, municipio) %>%
    dplyr::select(regiao = regiao_arquivo, estado = uf, municipio,
                  tipo_municipio, n_entrevistas)
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

  tabelas <- list(
    Stratification = purrr::map_dfr(recortes, estimate_question,
                                    desenho = fit$design,
                                    nivel_confianca = nivel) %>%
      formatar_percentuais() %>%
      dplyr::select(question, response, mean_percent, conf_low_percent,
                    conf_high_percent, estimate_ci),
    Results = montar_results(fit$design, ordem, metadados, nivel),
    ResultsStrat = montar_results_strat(fit$design, ordem, recortes, niveis,
                                        nivel)
  )

  xlsx <- file.path(dir_saida, paste0(cfg$outputs$prefixo, ".xlsx"))
  writexl::write_xlsx(tabelas, path = xlsx)

  ns <- contar_ns(fit$design, ordem, recortes)
  writexl::write_xlsx(ns, path = file.path(
    dir_saida, paste0(cfg$outputs$prefixo, "_N.xlsx")))

  localidades <- contar_localidades(fit$design)
  writexl::write_xlsx(list(Localidades = localidades), path = file.path(
    dir_saida, paste0(cfg$outputs$prefixo, "_localidades.xlsx")))

  diagnostico <- convergence_report(fit) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 6)))

  readr::write_csv(diagnostico, file.path(dir_saida, "diagnostico-margens.csv"))
  escrever_ambiente(fit, cfg, diagnostico, dir_saida)

  cat(sprintf("\nescrito em %s/\n  %s.xlsx  (%d + %d + %d linhas)\n",
              dir_saida, cfg$outputs$prefixo, nrow(tabelas$Stratification),
              nrow(tabelas$Results), nrow(tabelas$ResultsStrat)))
  cat(sprintf("  %s_N.xlsx  (o N nao ponderado de cada estimativa)\n",
              cfg$outputs$prefixo))
  cat(sprintf("  %s_localidades.xlsx  (%d municipios)\n", cfg$outputs$prefixo,
              nrow(localidades)))
  cat("  diagnostico-margens.csv\n  ambiente.txt\n")

  invisible(tabelas)
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
