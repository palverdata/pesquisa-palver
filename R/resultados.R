# Do peso calibrado aos arquivos da onda.
#
# Escreve cinco arquivos em ondas/<onda>/output/:
#
#   <id>_pesquisa_<AAAA>_<MM>_<DD>.json  os cruzamentos, para os graficos e
#                                        para a plataforma
#   <prefixo>_localidades.xlsx           entrevistas por municipio
#   <prefixo>_microdados.xlsx            uma linha por respondente, com o peso
#   diagnostico-margens.csv              margem ponderada vs cota, por categoria
#   ambiente.txt                         versoes, margens usadas e o resultado
#
# Nenhum vai para o git: output/ esta no .gitignore. Ver a politica de dados no
# README.

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


niveis_declarados <- function(qst) {

  nomes <- unique(c(names(qst$derivadas), names(qst$questoes)))

  purrr::map(purrr::set_names(nomes),
             ~ unlist(qst$derivadas[[.x]]$niveis %||% qst$questoes[[.x]]$niveis))
}


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

# ==============================================================================
# O JSON DA ONDA
# ==============================================================================


carregar_display <- function(caminho, onda) {

  disp <- ler_yaml(caminho)

  faltando <- setdiff(c("meta", "secoes", "recortes"), names(disp))
  if (length(faltando) > 0) {
    stop("display.yaml sem as chaves: ", paste(faltando, collapse = ", "),
         call. = FALSE)
  }

  if (!identical(disp$meta$onda, onda)) {
    stop("display.yaml: meta.onda e a pasta da onda tem de ser iguais.\n",
         "  pasta     : ", onda, "\n",
         "  meta.onda : ", disp$meta$onda %||% "(ausente)", call. = FALSE)
  }

  texto <- function(x, oque) {
    if (is.null(x) || !nzchar(trimws(as.character(x)))) {
      stop("display.yaml: ", oque, call. = FALSE)
    }
    as.character(x)
  }

  questoes <- list()

  for (secao in disp$secoes) {

    titulo <- texto(secao$titulo, "secao sem titulo")

    if (length(secao$questoes) == 0) {
      stop("display.yaml: secao '", titulo, "' sem questoes", call. = FALSE)
    }

    for (q in secao$questoes) {
      variavel <- texto(q$variavel, paste0("questao sem variavel na secao '",
                                           titulo, "'"))
      if (!is.null(q$harmonizar) && length(q$harmonizar$mapa) == 0) {
        stop("display.yaml: harmonizar sem `mapa` -> ", variavel, call. = FALSE)
      }

      if (!is.null(q$base)) {
        faltando <- setdiff(c("variavel", "valores"), names(q$base))
        if (length(faltando) > 0) {
          stop("display.yaml: `base` de ", variavel, " sem ",
               paste(faltando, collapse = " e "), call. = FALSE)
        }
      }

      if (!is.null(q$excluir)) {
        stop("display.yaml: `excluir` vale so em recorte, nao em questao -> ",
             variavel, ". Tirar uma resposta faria o grupo nao somar 100%.",
             call. = FALSE)
      }

      if (!is.null(q$ordenar) && !is.null(q$respostas)) {
        stop("display.yaml: `ordenar` e `respostas` na mesma questao -> ",
             variavel, ". `respostas` ja e a ordem final.", call. = FALSE)
      }

      if (!is.null(q$ordenar) &&
          !q$ordenar %in% c("decrescente", "declarado")) {
        stop("display.yaml: `ordenar` aceita 'decrescente' ou 'declarado' -> ",
             variavel, " tem '", q$ordenar, "'", call. = FALSE)
      }

      questoes[[length(questoes) + 1]] <- list(
        variavel = variavel,
        rotulo = texto(q$rotulo, paste0("questao sem rotulo -> ", variavel)),
        secao = titulo,
        harmonizar = q$harmonizar,
        base = if (is.null(q$base)) NULL else
          list(variavel = q$base$variavel, valores = unlist(q$base$valores)),
        ordenar = q$ordenar,
        fixar_no_fim = if (length(q$fixar_no_fim) == 0) NULL else
          unlist(q$fixar_no_fim),
        respostas = if (length(q$respostas) == 0) NULL else unlist(q$respostas)
      )
    }
  }

  recortes <- purrr::map(disp$recortes, function(r) {
    variavel <- texto(r$variavel, "recorte sem variavel")
    if (!is.null(r$harmonizar) && length(r$harmonizar$mapa) == 0) {
      stop("display.yaml: harmonizar sem `mapa` -> ", variavel, call. = FALSE)
    }
    list(
      variavel = variavel,
      rotulo = texto(r$rotulo, paste0("recorte sem rotulo -> ", variavel)),
      harmonizar = r$harmonizar,
      excluir = if (length(r$excluir) == 0) NULL else unlist(r$excluir),
      grupos = if (length(r$grupos) == 0) NULL else unlist(r$grupos)
    )
  })

  # A chave e o rotulo pos-harmonizacao. Hex invalido nao da erro na plataforma,
  # so pinta errado.
  cores <- if (length(disp$cores) == 0) NULL else {
    valores <- unlist(disp$cores)
    ruins <- names(valores)[!grepl("^#[0-9A-Fa-f]{6}$", valores)]
    if (length(ruins) > 0) {
      stop("display.yaml: cor fora do formato #RRGGBB -> ",
           paste(sprintf("%s: '%s'", ruins, valores[ruins]), collapse = ", "),
           call. = FALSE)
    }
    as.list(valores)
  }

  amostra <- purrr::map(disp$amostra, function(a) {
    variavel <- texto(a$variavel, "item de `amostra` sem variavel")
    list(
      variavel = variavel,
      rotulo = texto(a$rotulo, paste0("item de `amostra` sem rotulo -> ",
                                      variavel))
    )
  })

  if (length(amostra) == 0) {
    stop("display.yaml: bloco `amostra` ausente ou vazio. Ele e o resumo que ",
         "abre a divulgacao.", call. = FALSE)
  }

  if (length(questoes) == 0) stop("display.yaml: nenhuma questao", call. = FALSE)

  for (campo in list(list(questoes, "questao"), list(recortes, "recorte"))) {
    vars <- purrr::map_chr(campo[[1]], "variavel")
    if (anyDuplicated(vars)) {
      stop("display.yaml: ", campo[[2]], " declarada duas vezes -> ",
           paste(unique(vars[duplicated(vars)]), collapse = ", "),
           call. = FALSE)
    }
  }

  list(questoes = questoes, recortes = recortes, amostra = amostra,
       cores = cores)
}


kish <- function(pesos) sum(pesos)^2 / sum(pesos^2)

resposta_valida <- function(x) !is.na(x) & trimws(as.character(x)) != ""

# Sobrescrita que omite valor observado e erro: a celula desapareceria da tela.
ordenar_valores <- function(observados, declarados, sobrescrita, contexto) {

  if (!is.null(sobrescrita)) {
    fora <- setdiff(observados, sobrescrita)
    if (length(fora) > 0) {
      stop(contexto, ": a ordem declarada no display.yaml omite valor ",
           "observado -> ", paste(sprintf("'%s'", fora), collapse = ", "),
           call. = FALSE)
    }
    return(sobrescrita[sobrescrita %in% observados])
  }

  ordem <- declarados[declarados %in% observados]
  c(ordem, setdiff(observados, ordem))
}

recortar <- function(x) pmin(pmax(x, 0), 1)

# Precedencia: `respostas` manda; senao `ordenar`; `fixar_no_fim` sai da
# ordenacao. A ordenacao usa o share do total, nao o do recorte. Empate cai na
# ordem declarada: order() e estavel.
ordenar_respostas <- function(est, q, declarados, contexto) {

  observados <- est$response

  if (!is.null(q$respostas)) {
    return(ordenar_valores(observados, declarados, q$respostas, contexto))
  }

  fim <- q$fixar_no_fim

  if (!is.null(fim)) {
    fora <- setdiff(fim, declarados)
    if (length(fora) > 0) {
      stop(contexto, ": fixar_no_fim cita resposta que a questao nao declara ",
           "-> ", paste(sprintf("'%s'", fora), collapse = ", "), call. = FALSE)
    }
  }

  ordem <- if (identical(q$ordenar, "decrescente")) {
    est$response[order(-est$mean)]
  } else {
    ordenar_valores(observados, declarados, NULL, contexto)
  }

  c(setdiff(ordem, fim), intersect(fim, observados))
}


# Agrupa niveis ANTES de estimar: somar `share` depois nao serve, porque o
# intervalo da soma nao e a soma dos intervalos.
harmonizar_resposta <- function(valores, spec, declarados, contexto) {

  mapa <- unlist(spec$mapa)

  fora <- setdiff(names(mapa), declarados)
  if (length(fora) > 0) {
    stop(contexto, ": harmonizar.mapa cita nivel que a questao nao declara -> ",
         paste(sprintf("'%s'", fora), collapse = ", "), call. = FALSE)
  }

  rotulos <- character(0)
  for (nivel in declarados) {
    if (nivel %in% names(mapa)) {
      rotulos <- c(rotulos, unname(mapa[[nivel]]))
    } else if (is.null(spec$resto)) {
      rotulos <- c(rotulos, nivel)
    }
  }
  if (!is.null(spec$resto)) rotulos <- c(rotulos, spec$resto)
  rotulos <- unique(rotulos)

  originais <- as.character(valores)
  idx <- match(originais, names(mapa))

  novo <- rep(NA_character_, length(originais))
  novo[!is.na(idx)] <- unname(mapa[idx[!is.na(idx)]])

  sobrou <- is.na(idx) & !is.na(originais)
  novo[sobrou] <- if (is.null(spec$resto)) originais[sobrou] else spec$resto

  list(valores = factor(novo, levels = rotulos, ordered = TRUE),
       niveis = rotulos)
}


# Questao condicional. O filtro corta o DESENHO, entao vale igual no total e
# em todo recorte, e a estimativa se refaz entre quem foi perguntado.
filtrar_base <- function(design, spec, contexto) {

  dados <- design$variables

  if (!spec$variavel %in% names(dados)) {
    stop(contexto, ": `base.variavel` nao existe na base -> ", spec$variavel,
         call. = FALSE)
  }

  observados <- unique(as.character(dados[[spec$variavel]]))
  fora <- setdiff(spec$valores, observados)
  if (length(fora) > 0) {
    stop(contexto, ": `base.valores` cita valor que ", spec$variavel,
         " nao tem -> ", paste(sprintf("'%s'", fora), collapse = ", "),
         call. = FALSE)
  }

  dentro <- as.character(dados[[spec$variavel]]) %in% spec$valores

  if (!any(dentro)) {
    stop(contexto, ": `base` nao deixou nenhum respondente", call. = FALSE)
  }

  list(design = design[dentro, ], n_fora = sum(!dentro))
}


# recorte = NULL devolve o total, como um recorte de um grupo so.
montar_cruzamento <- function(q, recorte, design, nivel, answers, wave_id,
                              statement) {

  dados <- design$variables
  pesos <- as.numeric(stats::weights(design))

  # Subconjunto de desenho calibrado zera o peso em vez de remover a linha,
  # entao o dominio e `peso > 0` e nao a contagem de linhas.
  ok <- resposta_valida(dados[[q$coluna]]) & pesos > 0
  grupo <- rep("__total__", nrow(dados))

  if (!is.null(recorte)) {
    ok <- ok & resposta_valida(dados[[recorte$coluna]])
    grupo <- as.character(dados[[recorte$coluna]])
  }

  if (!any(ok)) return(NULL)

  est <- if (is.null(recorte)) {
    estimate_question(q$coluna, design, nivel) %>%
      dplyr::mutate(group = "__total__")
  } else {
    estimate_svyby(q$coluna, recorte$coluna, design, nivel) %>%
      dplyr::rename(group = stratification_category)
  }

  if (is.null(est) || nrow(est) == 0) return(NULL)

  groups <- if (is.null(recorte)) "__total__" else {
    ordenar_valores(unique(grupo[ok]), recorte$niveis, recorte$grupos,
                    paste0("recorte ", recorte$variavel, " em ", q$variavel))
  }

  # Grupo excluido sai da tela e da base; os que ficam nao mudam, porque cada
  # grupo e estimado dentro de si.
  excluidos <- intersect(recorte$excluir, groups)

  registro <- purrr::map(excluidos, function(g) {
    list(group = g, n = sum(ok & grupo == g))
  })

  if (length(excluidos) > 0) {
    groups <- setdiff(groups, excluidos)
    if (length(groups) == 0) {
      stop("recorte ", recorte$variavel, ": `excluir` removeu todos os grupos",
           call. = FALSE)
    }
    ok <- ok & !(grupo %in% excluidos)
  }

  resposta <- as.character(dados[[q$coluna]])
  contagem <- table(grupo[ok], resposta[ok])

  n_de <- function(g, a) {
    if (!g %in% rownames(contagem) || !a %in% colnames(contagem)) return(0L)
    as.integer(contagem[g, a])
  }

  cells <- list()

  for (g in groups) {
    for (a in answers) {
      linha <- est[est$group == g & est$response == a, ]
      cells[[length(cells) + 1]] <- list(
        group = g,
        answer = a,
        share = if (nrow(linha) == 1) linha$mean else 0,
        low = if (nrow(linha) == 1) recortar(linha$conf_low) else 0,
        high = if (nrow(linha) == 1) recortar(linha$conf_high) else 0,
        n = n_de(g, a)
      )
    }
  }

  group_stats <- purrr::map(groups, function(g) {
    linhas <- ok & grupo == g
    list(group = g, n = sum(linhas), n_eff = kish(pesos[linhas]))
  })

  fora <- list(
    wave_id = wave_id,
    question_key = q$variavel,
    question_label = q$rotulo,
    question_statement = statement,
    question_section = q$secao,
    breakdown_key = if (is.null(recorte)) NA else recorte$variavel,
    breakdown_label = if (is.null(recorte)) NA else recorte$rotulo,
    groups = I(groups),
    answers = I(answers),
    cells = I(cells),
    group_stats = I(group_stats),
    base = sum(ok),
    weighted = TRUE
  )

  if (length(registro) > 0) {
    fora <- append(fora, list(excluded = I(registro)),
                   after = match("base", names(fora)))
  }

  if (!is.null(q$base)) {
    fora <- append(fora, list(base_filter = list(
      variable = q$base$variavel,
      values = I(q$base$valores),
      excluded_n = q$base$n_fora
    )), after = match("base", names(fora)))
  }

  fora
}


# A distribuicao das variaveis que a calibracao ajusta. Sem intervalo: margem
# de calibracao tem intervalo de largura zero por construcao.
montar_resumo <- function(design, amostra, nivel) {

  dados <- design$variables

  purrr::map(amostra, function(item) {

    est <- estimate_question(item$variavel, design, nivel)
    valores <- as.character(dados[[item$variavel]])
    ok <- resposta_valida(valores)
    contagem <- table(valores[ok])

    answers <- ordenar_valores(est$response, item$niveis, NULL,
                               paste("amostra", item$variavel))

    cells <- purrr::map(answers, function(a) {
      linha <- est[est$response == a, ]
      list(
        answer = a,
        share = if (nrow(linha) == 1) linha$mean else 0,
        n = if (a %in% names(contagem)) as.integer(contagem[[a]]) else 0L
      )
    })

    list(
      key = item$variavel,
      label = item$rotulo,
      answers = I(answers),
      cells = I(cells),
      base = sum(ok)
    )
  })
}

conferir_resumo <- function(amostra, margens) {

  declaradas <- purrr::map_chr(amostra, "variavel")

  # uma margem e marginal ou cruzada; o conjunto e a uniao das variaveis citadas
  das_margens <- unique(unlist(purrr::map(margens, function(m) {
    if (is.list(m) && !is.null(m$variaveis)) unlist(m$variaveis) else unlist(m)
  })))

  sobrando <- setdiff(declaradas, das_margens)
  if (length(sobrando) > 0) {
    stop("display.yaml: `amostra` declara variavel que nao e margem de ",
         "calibracao -> ", paste(sobrando, collapse = ", "), call. = FALSE)
  }

  faltando <- setdiff(das_margens, declaradas)
  if (length(faltando) > 0) {
    stop("display.yaml: `amostra` esqueceu margem de calibracao -> ",
         paste(faltando, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}


montar_json <- function(fit, qst, display, cfg) {

  design <- fit$design
  dados <- design$variables
  nivel <- cfg$diagnosticos$nivel_confianca %||% 0.95
  niveis <- niveis_declarados(qst)

  sequencia <- cfg$onda$sequencia
  if (is.null(sequencia)) {
    stop("config.yaml: bloco `onda` sem `sequencia`. O JSON da plataforma ",
         "precisa dela para wave.id e wave.sequence.", call. = FALSE)
  }
  wave_id <- sprintf("%02d", as.integer(sequencia))

  conferir_resumo(display$amostra, cfg$calibracao$margens)

  declaradas <- c(purrr::map_chr(display$questoes, "variavel"),
                  purrr::map_chr(display$recortes, "variavel"),
                  purrr::map_chr(display$amostra, "variavel"))
  ausentes <- setdiff(declaradas, names(dados))
  if (length(ausentes) > 0) {
    stop("display.yaml cita variavel que a base nao tem: ",
         paste(ausentes, collapse = ", "), call. = FALSE)
  }

  # `titulo` antes de `texto`: em coluna normalizada o cabecalho e um nome
  # interno, e sem esta ordem ele vaza para a tela.
  enunciado_de <- function(variavel) {
    spec <- qst$questoes[[variavel]]
    enunciado <- spec$titulo %||% spec$texto
    if (is.null(enunciado) || !nzchar(trimws(enunciado))) {
      stop("questao sem `titulo` nem `texto` no questionario.yaml -> ",
           variavel, call. = FALSE)
    }
    enunciado
  }

  # Coluna de trabalho por variavel harmonizada, com prefixo distinto para
  # questao e recorte. A coluna original nunca e tocada: ela pode ser margem.
  preparar <- function(spec, prefixo, tipo) {

    spec$coluna <- spec$variavel
    spec$niveis <- niveis[[spec$variavel]]

    if (!is.null(spec$harmonizar)) {
      harmonizada <- harmonizar_resposta(dados[[spec$variavel]],
                                        spec$harmonizar, spec$niveis,
                                        paste(tipo, spec$variavel))
      spec$coluna <- paste0(prefixo, spec$variavel)
      spec$niveis <- harmonizada$niveis
      spec$valores <- harmonizada$valores
    }

    # `excluir` cita o rotulo pos-harmonizacao, que so existe aqui.
    fora <- setdiff(spec$excluir, spec$niveis)
    if (length(fora) > 0) {
      stop("recorte ", spec$variavel, ": `excluir` cita grupo que o recorte ",
           "nao tem -> ", paste(sprintf("'%s'", fora), collapse = ", "),
           call. = FALSE)
    }

    spec
  }

  # o resumo usa a coluna original, sem harmonizar: so precisa dos niveis
  display$amostra <- purrr::map(display$amostra, function(item) {
    item$niveis <- niveis[[item$variavel]]
    item
  })

  display$recortes <- purrr::map(display$recortes, preparar, ".recorte_",
                                 "recorte")
  display$questoes <- purrr::map(display$questoes, preparar, ".questao_",
                                 "questao")

  for (spec in c(display$recortes, display$questoes)) {
    if (!is.null(spec$valores)) {
      design$variables[[spec$coluna]] <- spec$valores
    }
  }

  crosstabs <- list()
  questions <- list()

  for (q in display$questoes) {

    statement <- enunciado_de(q$variavel)

    design_q <- design

    if (!is.null(q$base)) {
      filtrado <- filtrar_base(design, q$base, paste("questao", q$variavel))
      design_q <- filtrado$design
      q$base$n_fora <- filtrado$n_fora
    }

    est_total <- estimate_question(q$coluna, design_q, nivel)
    if (is.null(est_total) || nrow(est_total) == 0) {
      stop("questao sem resposta valida na onda -> ", q$variavel, call. = FALSE)
    }

    answers <- ordenar_respostas(est_total, q, q$niveis,
                                 paste("questao", q$variavel))

    crosstabs[[paste0(q$variavel, "|")]] <- montar_cruzamento(
      q, NULL, design_q, nivel, answers, wave_id, statement)

    for (recorte in display$recortes) {
      cruzamento <- montar_cruzamento(q, recorte, design_q, nivel, answers,
                                      wave_id, statement)
      if (is.null(cruzamento)) {
        stop("combinacao declarada sem cruzamento -> ", q$variavel, " x ",
             recorte$variavel, call. = FALSE)
      }
      crosstabs[[paste0(q$variavel, "|", recorte$variavel)]] <- cruzamento
    }

    questions[[length(questions) + 1]] <- list(
      key = q$variavel, label = q$rotulo, statement = statement,
      section = q$secao
    )
  }

  breakdowns <- purrr::map(display$recortes, function(recorte) {
    coluna <- design$variables[[recorte$coluna]]
    observados <- unique(as.character(coluna[resposta_valida(coluna)]))
    niveis_menu <- setdiff(
      ordenar_valores(observados, recorte$niveis, recorte$grupos,
                      paste("recorte", recorte$variavel)),
      recorte$excluir)

    list(
      key = recorte$variavel,
      label = recorte$rotulo,
      levels = I(niveis_menu)
    )
  })

  conferir_cores(display$cores, crosstabs)

  estrutura <- list(
    wave = list(
      id = wave_id,
      sequence = as.integer(sequencia),
      date = paste0(cfg$onda$data_divulgacao, "T00:00:00.000Z")
    ),
    questions = I(questions),
    breakdowns = I(breakdowns),
    sample = I(montar_resumo(design, display$amostra, nivel)),
    crosstabs = crosstabs
  )

  if (length(display$cores) > 0) {
    estrutura <- append(estrutura, list(colors = display$cores),
                        after = match("sample", names(estrutura)))
  }

  conferir_json(estrutura, display)

  estrutura
}


# Cor sem rotulo correspondente nao quebra a plataforma, mas costuma ser erro
# de digitacao: avisa e segue.
conferir_cores <- function(cores, crosstabs) {

  if (length(cores) == 0) return(invisible(NULL))

  rotulos <- unique(unlist(purrr::map(crosstabs, function(ct) {
    c(as.character(ct$answers), as.character(ct$groups))
  })))

  orfas <- setdiff(names(cores), rotulos)

  if (length(orfas) > 0) {
    warning("display.yaml: ", length(orfas), " cor(es) para rotulo que o ",
            "arquivo nao tem -> ", paste(sprintf("'%s'", orfas),
                                         collapse = ", "),
            call. = FALSE)
  }

  invisible(orfas)
}


# A plataforma nao verifica nada em runtime: a conferencia e antes de escrever.
conferir_json <- function(estrutura, display, tolerancia = 1e-9) {

  esperadas <- purrr::map(display$questoes, function(q) {
    c(paste0(q$variavel, "|"),
      paste0(q$variavel, "|", purrr::map_chr(display$recortes, "variavel")))
  })

  faltando <- setdiff(unlist(esperadas), names(estrutura$crosstabs))
  if (length(faltando) > 0) {
    stop("cruzamento declarado e ausente do JSON: ",
         paste(faltando, collapse = ", "), call. = FALSE)
  }

  for (chave in names(estrutura$crosstabs)) {

    ct <- estrutura$crosstabs[[chave]]

    vazio <- purrr::keep(
      c("question_label", "question_statement", "question_section"),
      ~ !nzchar(trimws(ct[[.x]] %||% "")))
    if (length(vazio) > 0) {
      stop(chave, ": campo de texto vazio -> ", paste(vazio, collapse = ", "),
           call. = FALSE)
    }

    shares <- purrr::map_dbl(ct$cells, "share")
    grupos <- purrr::map_chr(ct$cells, "group")

    for (g in unique(grupos)) {
      soma <- sum(shares[grupos == g])
      if (abs(soma - 1) > tolerancia) {
        stop(chave, ": o grupo '", g, "' soma ", format(soma, digits = 15),
             ", nao 1. Erro de agregacao.", call. = FALSE)
      }
    }

    fora <- purrr::map_dbl(ct$cells, "low") < 0 |
      purrr::map_dbl(ct$cells, "high") > 1
    if (any(fora)) stop(chave, ": low ou high fora de [0, 1]", call. = FALSE)
  }

  invisible(TRUE)
}


nome_arquivo_json <- function(cfg) {
  sprintf("%02d_pesquisa_%s.json", as.integer(cfg$onda$sequencia),
          gsub("-", "_", cfg$onda$data_divulgacao))
}

# auto_unbox colapsaria vetor de um elemento em escalar, e por isso os arrays
# vao marcados com I() na montagem. digits = NA guarda a precisao cheia;
# na = "null" e o que faz breakdown_key do total sair nulo.
escrever_json <- function(estrutura, cfg, dir_saida) {

  caminho <- file.path(dir_saida, nome_arquivo_json(cfg))

  jsonlite::write_json(estrutura, path = caminho, auto_unbox = TRUE,
                       digits = NA, na = "null", null = "null",
                       pretty = FALSE)

  caminho
}

# Sem display.yaml nao ha JSON: avisa e devolve NULL.
exportar_json <- function(fit, qst, cfg, dir_saida) {

  caminho <- cfg$caminhos$display %||% ""

  if (!nzchar(caminho) || !file.exists(caminho)) {
    warning("sem ", if (nzchar(caminho)) caminho else "display.yaml",
            ": o JSON da plataforma nao foi escrito.", call. = FALSE)
    return(NULL)
  }

  display <- carregar_display(caminho, cfg$onda$slug)
  estrutura <- montar_json(fit, qst, display, cfg)

  escrever_json(estrutura, cfg, dir_saida)

  estrutura
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

  estrutura <- exportar_json(fit, qst, cfg, dir_saida)

  diagnostico <- convergence_report(fit) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 6)))

  readr::write_csv(diagnostico, file.path(dir_saida, "diagnostico-margens.csv"))
  escrever_ambiente(fit, cfg, diagnostico, dir_saida)

  cat(sprintf("
escrito em %s/
", dir_saida))
  if (is.null(estrutura)) {
    cat("  (sem JSON: a onda nao tem display.yaml)
")
  } else {
    cat(sprintf("  %s  (%d cruzamentos, %d questoes)
",
                nome_arquivo_json(cfg), length(estrutura$crosstabs),
                length(estrutura$questions)))
  }
  cat(sprintf("  %s_localidades.xlsx  (%d municipios)
", cfg$outputs$prefixo,
              nrow(localidades)))
  if (is.null(microdados)) {
    cat("  (sem microdados: outputs.microdados = false)
")
  } else {
    cat(sprintf("  %s_microdados.xlsx  (%d respondentes x %d colunas)
",
                cfg$outputs$prefixo, nrow(microdados), ncol(microdados)))
  }
  cat("  diagnostico-margens.csv
  ambiente.txt
")

  invisible(estrutura)
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
