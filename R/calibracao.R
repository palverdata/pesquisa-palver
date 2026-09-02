
# ==============================================================================
# QUESTIONARIO E BASE
# ==============================================================================

carregar_questionario <- function(caminho) {

  qst <- ler_yaml(caminho)

  faltando <- setdiff(c("meta", "colunas", "questoes", "derivadas",
                        "estratificacao", "exportar_ordem"), names(qst))
  if (length(faltando) > 0) {
    stop("questionario.yaml sem as chaves: ", paste(faltando, collapse = ", "))
  }

  sem_nivel <- purrr::keep(names(qst$questoes),
                           ~ length(qst$questoes[[.x]]$niveis) == 0)
  if (length(sem_nivel) > 0) {
    stop("questoes sem niveis declarados: ", paste(sem_nivel, collapse = ", "))
  }

  for (nome in names(qst$matrizes)) {
    if (length(qst$matrizes[[nome]]$itens) != qst$matrizes[[nome]]$n_itens) {
      stop(nome, ": n_itens = ", qst$matrizes[[nome]]$n_itens,
           " mas foram declarados ", length(qst$matrizes[[nome]]$itens),
           " itens")
    }
  }

  expandir_harmonizacao(qst)
}

expandir_harmonizacao <- function(qst) {

  for (bloco in names(qst$harmonizacao)) {

    spec <- qst$harmonizacao[[bloco]]
    mapa <- unlist(spec$mapa)

    for (origem in unlist(spec$aplicar_a)) {

      if (!origem %in% names(qst$questoes)) {
        stop("harmonizacao ", bloco, ": questao inexistente -> ", origem)
      }

      niveis <- unlist(qst$questoes[[origem]]$niveis)
      fora <- setdiff(names(mapa), niveis)
      if (length(fora) > 0) {
        stop("harmonizacao ", bloco, " x ", origem, ": o mapa cita nivel que a ",
             "questao nao declara -> ", paste(fora, collapse = ", "))
      }

      rotulos <- unname(ifelse(niveis %in% names(mapa), mapa[niveis], niveis))

      nome <- paste0(origem, "_h")
      qst$questoes[[nome]] <- list(
        texto = qst$questoes[[origem]]$texto,
        titulo = qst$questoes[[origem]]$titulo,
        harmonizada_de = origem,
        mapa = as.list(stats::setNames(rotulos, niveis)),
        # a ordem dos rotulos segue a ordem dos niveis da questao original
        niveis = as.list(unique(rotulos))
      )

      ordem <- unlist(qst$exportar_ordem)
      pos <- match(origem, ordem)
      if (!is.na(pos)) {
        qst$exportar_ordem <- as.list(append(ordem, nome, after = pos))
      }
    }
  }

  qst
}

# `texto` reproduz o cabecalho do arquivo caractere por caractere: nao ha
# normalizacao.
enunciados_do_arquivo <- function(qst) {

  mapa <- character(0)

  for (bloco in c("questoes", "questoes_nao_exportadas")) {
    for (nome in names(qst[[bloco]])) {
      # questao harmonizada nao existe no arquivo: e derivada da origem
      if (!is.null(qst[[bloco]][[nome]]$harmonizada_de)) next
      texto <- qst[[bloco]][[nome]]$texto
      if (is.null(texto)) next
      if (texto %in% names(mapa) && !identical(mapa[[texto]], nome)) {
        stop("duas variaveis declaram o mesmo enunciado (", mapa[[texto]],
             " e ", nome, "): ", texto)
      }
      mapa[[texto]] <- nome
    }
  }

  mapa
}

ler_bruto <- function(caminho, qst) {

  # trim_ws = FALSE: o readxl apara espaco por padrao, e isso faria o motor ler
  # um valor diferente do que o arquivo tem.
  bruto <- readxl::read_excel(caminho, sheet = 1, col_types = "text",
                              trim_ws = FALSE, .name_repair = "minimal")

  metadados <- unlist(qst$colunas)
  enunciados <- enunciados_do_arquivo(qst)
  de_questao <- enunciados[names(bruto)]

  interno <- ifelse(
    names(bruto) %in% names(metadados), metadados[names(bruto)],
    ifelse(is.na(de_questao), NA_character_, de_questao)
  )

  if (anyNA(interno)) {
    message("colunas do arquivo ignoradas por nao estarem declaradas: ",
            paste(names(bruto)[is.na(interno)], collapse = ", "))
    bruto <- bruto[, !is.na(interno)]
    interno <- interno[!is.na(interno)]
  }

  ausentes <- setdiff(unique(c(unname(metadados), unname(enunciados))), interno)
  if (length(ausentes) > 0) {
    stop(length(ausentes), " item(ns) declarado(s) e ausente(s) do arquivo:\n  ",
         paste(ausentes, collapse = "\n  "))
  }

  if (anyDuplicated(interno)) {
    stop("duas colunas do arquivo casam com a mesma variavel: ",
         paste(unique(interno[duplicated(interno)]), collapse = ", "))
  }

  names(bruto) <- interno

  if (anyDuplicated(bruto$respondent_id)) stop("respondent_id repetido")

  dplyr::mutate(
    bruto,
    dplyr::across(dplyr::where(is.character),
                  ~ dplyr::if_else(trimws(.x) == "", NA_character_, .x))
  )
}

montar_respondentes <- function(base, qst) {

  for (nome in names(qst$questoes)) {

    spec <- qst$questoes[[nome]]

    # As harmonizadas vem depois das originais na lista, entao a origem ja e fator
    if (!is.null(spec$harmonizada_de)) {
      base[[nome]] <- factor(
        unlist(spec$mapa)[as.character(base[[spec$harmonizada_de]])],
        levels = unlist(spec$niveis), ordered = TRUE
      )
      next
    }

    if (!nome %in% names(base)) {
      stop("questao declarada e ausente na base montada: ", nome)
    }

    valores <- as.character(base[[nome]])
    niveis <- unlist(spec$niveis)
    orfas <- setdiff(unique(valores[!is.na(valores)]), niveis)
    if (length(orfas) > 0) {
      stop(nome, ": resposta fora dos niveis declarados -> ",
           paste(sprintf("'%s'", orfas), collapse = ", "),
           ". O questionario.yaml tem de descrever o arquivo como ele e.")
    }

    base[[nome]] <- factor(valores, levels = niveis, ordered = TRUE)
  }

  for (nome in names(qst$derivadas)) {
    base <- aplicar_derivada(base, nome, qst$derivadas[[nome]])
  }

  base
}

cortar_amostra <- function(base, spec) {

  n <- spec$registrada
  if (is.null(n)) return(base)

  chave <- spec$ordenar_por %||% "end_timestamp"

  if (!chave %in% names(base)) {
    stop("amostra.ordenar_por: coluna ausente na base -> ", chave)
  }
  if (nrow(base) < n) {
    stop("amostra.registrada = ", n, ", mas o arquivo tem ", nrow(base),
         " respondentes.")
  }

  # A fracao de segundo vem ausente quando e zero; sem padronizar, a ordem do
  # texto nao seria a cronologica.
  quando <- sub("^(\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}:\\d{2})([^.].*|)$",
                "\\1.000000\\2", as.character(base[[chave]]))

  if (anyNA(quando) ||
      !all(grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}:\\d{2}[.]", quando))) {
    stop("amostra.ordenar_por: ", chave, " precisa estar em ",
         "AAAA-MM-DD HH:MM:SS para o corte ser replicavel.")
  }

  # respondent_id desempata, para o corte nao depender da ordem do arquivo
  cronologica <- order(quando, base$respondent_id)

  cat(sprintf("amostra registrada: %d dos %d respondentes do arquivo",
              n, nrow(base)))
  cat(sprintf(" (ultimo a entrar concluiu em %s)\n", quando[cronologica[n]]))

  # devolve na ordem do arquivo, nao na do corte
  base[sort(cronologica[seq_len(n)]), ]
}

aplicar_derivada <- function(base, nome, spec) {

  exige <- function(colunas) {
    faltando <- setdiff(colunas, names(base))
    if (length(faltando) > 0) {
      stop("derivada ", nome, ": coluna de origem ausente -> ",
           paste(faltando, collapse = ", "))
    }
  }

  if (spec$tipo %in% c("mapa", "mapa_com_resto")) {

    exige(spec$origem)
    origem <- as.character(base[[spec$origem]])
    idx <- match(origem, names(spec$mapa))
    novo <- rep(NA_character_, length(origem))
    novo[!is.na(idx)] <- unlist(spec$mapa)[idx[!is.na(idx)]]

    if (identical(spec$tipo, "mapa_com_resto")) {
      novo[is.na(idx) & !is.na(origem)] <- spec$resto
    }

  } else if (identical(spec$tipo, "agrupamento")) {

    exige(spec$origem)
    tabela <- purrr::imap_dfr(spec$grupos,
                              ~ tibble::tibble(de = unlist(.x), para = .y))
    idx <- match(as.character(base[[spec$origem]]), tabela$de)
    novo <- rep(NA_character_, nrow(base))
    novo[!is.na(idx)] <- tabela$para[idx[!is.na(idx)]]

  } else if (identical(spec$tipo, "voto_pregresso")) {

    exige(c(spec$origem_voto, spec$filtro_comparecimento$variavel))
    voto <- as.character(base[[spec$origem_voto]])
    idx <- match(voto, names(spec$mapa))

    novo <- rep(NA_character_, nrow(base))
    novo[!is.na(idx)] <- unlist(spec$mapa)[idx[!is.na(idx)]]
    novo[is.na(voto)] <- spec$vazio_para
    novo[as.character(base[[spec$filtro_comparecimento$variavel]]) ==
           spec$filtro_comparecimento$valor_nao_votou] <-
      spec$filtro_comparecimento$destino

  } else if (identical(spec$tipo, "limiar")) {

    exige(purrr::map_chr(spec$ordem_avaliacao, "variavel"))
    novo <- rep(spec$resto, nrow(base))

    # ordem inversa: a primeira regra declarada tem precedencia
    for (regra in rev(spec$ordem_avaliacao)) {
      novo[as.character(base[[regra$variavel]]) %in% unlist(spec$limiar)] <-
        regra$valor
    }

  } else {
    stop("tipo de derivada nao suportado: ", spec$tipo)
  }

  base[[nome]] <- if (is.null(spec$niveis)) novo else {
    factor(novo, levels = unlist(spec$niveis))
  }

  base
}

# ==============================================================================
# GEOGRAFIA (MUNICIPIOS)
# ==============================================================================

# Crosswalk curado de municipio (codigo IBGE, codigo TSE, populacao,
# classificacao_pnadc) -- ver insumos/municipios_brasil.yaml. classificacao_pnadc
# foi construida para bater com a mesma definicao de Capital/Regiao Metropolitana/
# Interior que a PNADc usa via Capital/RM_RIDE (ver R/propensao.R).
carregar_municipios <- function(caminho = "insumos/municipios_brasil.yaml") {

  municipios <- ler_yaml(caminho)$municipios %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    dplyr::mutate(.chave = paste(toupper(municipio), uf))

  if (anyDuplicated(municipios$.chave)) {
    stop(caminho, ": municipio/UF duplicado no crosswalk")
  }

  municipios
}

# Grafias do municipio que a plataforma de coleta usa e que divergem do IBGE --
# particularidade da plataforma, não da onda, por isso mora aqui e não em
# questionario.yaml. Verificado uma a uma contra insumos/municipios_brasil.yaml
# (nao e so um reaproveitamento as cegas de uma lista antiga: duas ou tres grafias
# que pareciam precisar de correcao na verdade ja batiam com o crosswalk atual).
correcoes_municipio <- tibble::tribble(
  ~uf,  ~de,                          ~para,
  "SC", "PIÇARRAS",                   "BALNEÁRIO PIÇARRAS",
  "MS", "AMAMBAÍ",                    "AMAMBAI",
  "SP", "EMBU",                       "EMBU DAS ARTES",
  "PE", "BELÉM DE SÃO FRANCISCO",     "BELÉM DO SÃO FRANCISCO",
  "RJ", "PARATI",                     "PARATY",
  "RJ", "TRAJANO DE MORAIS",          "TRAJANO DE MORAES",
  "PE", "SÃO VICENTE FERRER",         "SÃO VICENTE FÉRRER",
  "RO", "ALTO ALEGRE DO PARECIS",     "ALTO ALEGRE DOS PARECIS",
  "RJ", "ARMAÇÃO DE BÚZIOS",          "ARMAÇÃO DOS BÚZIOS",
  "SP", "MOGI-GUAÇU",                 "MOGI GUAÇU",
  "PE", "IGUARACI",                   "IGUARACY",
  "RN", "AUGUSTO SEVERO",             "CAMPO GRANDE",
  "CE", "ERERÊ",                      "ERERÉ",
  "SC", "LAURO MULLER",               "LAURO MÜLLER",
  "RN", "PRESIDENTE JUSCELINO",       "SERRA CAIADA",
  "SP", "MOGI-MIRIM",                 "MOGI MIRIM",
  "TO", "FORTALEZA DO TABOCÃO",       "TABOCÃO",
  "RS", "SANTANA DO LIVRAMENTO",      "SANT'ANA DO LIVRAMENTO",
  "RN", "OLHO-D'ÁGUA DO BORGES",      "OLHO D'ÁGUA DO BORGES",
  "MT", "POXORÉO",                    "POXORÉU",
  "PE", "ITAMARACÁ",                  "ILHA DE ITAMARACÁ",
  "PA", "SANTA ISABEL DO PARÁ",       "SANTA IZABEL DO PARÁ",
  "GO", "SÃO LUÍZ DO NORTE",          "SÃO LUIZ DO NORTE"
)

# Casa a base do painel contra o crosswalk por municipio + UF e traz codigo_ibge,
# codigo_tse, populacao_ibge e tipo_mun_std (Capital/RM/Interior, mesma escala do
# lado da PNADc em R/propensao.R). Municipio sem casamento e erro, nunca NA
# silencioso -- o painel so deveria trazer municipios validos do IBGE.
padronizar_geografia <- function(base, municipios) {

  municipio_std <- toupper(base$municipio)

  idx_correcao <- match(paste(municipio_std, base$uf),
                        paste(correcoes_municipio$de, correcoes_municipio$uf))
  corrigir <- !is.na(idx_correcao)
  municipio_std[corrigir] <- correcoes_municipio$para[idx_correcao[corrigir]]

  # DF e um unico municipio (Brasilia) para o IBGE/TSE, mas a plataforma reporta
  # a Regiao Administrativa (Taguatinga, Ceilandia, Gama etc.) como municipio.
  municipio_std[base$uf == "DF"] <- "BRASÍLIA"

  chave <- paste(municipio_std, base$uf)
  idx <- match(chave, municipios$.chave)

  # sem_casamento so e computado dentro do if: paste0() com separador de tamanho 1
  # NAO devolve character(0) quando os dois vetores reciclados sao vazios -- devolve
  # "/" (o separador reciclado contra "") -- calcular fora do if faria o stop()
  # disparar mesmo com todo mundo casado.
  if (any(is.na(idx))) {
    sem_casamento <- unique(paste0(base$municipio[is.na(idx)], "/",
                                   base$uf[is.na(idx)]))
    stop(length(sem_casamento), " municipio/UF do painel sem casamento no crosswalk ",
         "(mesmo apos as correcoes de grafia conhecidas em correcoes_municipio):\n  ",
         paste(sem_casamento, collapse = "\n  "))
  }

  base$codigo_ibge <- municipios$codigo_ibge[idx]
  base$codigo_tse <- municipios$codigo_tse[idx]
  base$populacao_ibge <- municipios$populacao[idx]
  base$tipo_mun_std <- dplyr::if_else(
    municipios$classificacao_pnadc[idx] == "Regiao Metropolitana", "RM",
    municipios$classificacao_pnadc[idx]
  )

  base
}

# ==============================================================================
# MARGENS
# ==============================================================================

carregar_margens <- function(caminho) {

  bruto <- ler_yaml(caminho)
  variaveis <- unlist(bruto$meta$variaveis)

  if (is.null(bruto$conjunta)) {
    stop(caminho, ": sem o bloco `conjunta`. Regere com o script em scripts/.")
  }

  tabela <- purrr::map_dfr(bruto$conjunta,
                           ~ tibble::as_tibble(.x[c(variaveis, "freq")]))

  for (v in variaveis) {
    tabela[[v]] <- factor(tabela[[v]], levels = unlist(bruto$niveis[[v]]))
    if (anyNA(tabela[[v]])) {
      stop(caminho, ": categoria de ", v, " fora dos niveis declarados")
    }
  }

  list(variaveis = variaveis, tabela = tabela)
}

montar_alvos <- function(fontes, margens, tolerancia_pp = 0.05) {

  total_ref <- NULL
  alvos <- list()

  for (margem in margens) {

    declarada <- is.list(margem) && !is.null(margem$variaveis)
    variaveis <- unlist(if (declarada) margem$variaveis else margem)
    rotulo <- paste(variaveis, collapse = " x ")

    fonte <- if (declarada && !is.null(margem$fonte)) margem$fonte else {
      cobrem <- names(purrr::keep(fontes, ~ all(variaveis %in% .x$variaveis)))
      if (length(cobrem) == 0) {
        stop("nenhuma fonte tem todas as variaveis de [", rotulo, "]. ",
             "Disponiveis: ",
             paste(purrr::imap_chr(fontes, ~ sprintf("%s (%s)", .y,
                   paste(.x$variaveis, collapse = ", "))), collapse = " | "))
      }
      if (length(cobrem) > 1) {
        stop("[", rotulo, "] existe em mais de uma fonte (",
             paste(cobrem, collapse = ", "), ") e elas discordam. Declare:\n",
             "  - variaveis: [", paste(variaveis, collapse = ", "), "]\n",
             "    fonte: ", cobrem[[1]])
      }
      cobrem
    }

    alvo <- fontes[[fonte]]$tabela %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(variaveis))) %>%
      dplyr::summarise(Freq = sum(freq), .groups = "drop") %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(variaveis)))

    if (is.null(total_ref)) total_ref <- sum(alvo$Freq)
    alvo$Freq <- arredondar_preservando_total(
      alvo$Freq / sum(alvo$Freq) * total_ref
    )

    alvos[[rotulo]] <- as.data.frame(alvo)
  }

  conferir_marginais(alvos, tolerancia_pp)
  validar_alvos(alvos)

  alvos
}

conferir_marginais <- function(alvos, tolerancia_pp) {

  pct <- function(alvo, v) {
    total <- tapply(alvo$Freq, as.character(alvo[[v]]), sum)
    total / sum(total)
  }

  variaveis <- unlist(purrr::map(alvos, ~ setdiff(names(.x), "Freq")))

  for (v in unique(variaveis[duplicated(variaveis)])) {

    com_v <- purrr::keep(alvos, ~ v %in% names(.x))
    ref <- pct(com_v[[1]], v)

    for (nome in names(com_v)[-1]) {
      dif <- 100 * max(abs(ref - pct(com_v[[nome]], v)[names(ref)]))
      if (dif > tolerancia_pp) {
        stop("margens incompativeis em ", v, ": '", names(com_v)[[1]], "' e '",
             nome, "' implicam distribuicoes diferentes (divergencia maxima ",
             sprintf("%.2f", dif), " pp). A PNADc mede populacao residente e ",
             "o TSE mede comparecimento. Use a mesma fonte para ", v, " em ",
             "todas as margens, ou tire ", v, " de uma delas.", call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

arredondar_preservando_total <- function(x) {

  piso <- floor(x)
  faltam <- round(sum(x)) - sum(piso)

  if (faltam > 0) {
    ordem <- order(x - piso, decreasing = TRUE)
    piso[ordem[seq_len(faltam)]] <- piso[ordem[seq_len(faltam)]] + 1
  }

  piso
}

validar_alvos <- function(alvos, tolerancia = 1e-6) {

  totais <- purrr::map_dbl(alvos, ~ sum(.x$Freq))

  if (max(totais) / min(totais) - 1 > tolerancia) {
    stop("totais populacionais inconsistentes entre margens:\n",
         paste(sprintf("  %-30s %.0f", names(totais), totais), collapse = "\n"),
         call. = FALSE)
  }

  invisible(totais)
}

# ==============================================================================
# RAKING E APARO
# ==============================================================================

rake_weights <- function(dados, alvos, tolerancia = 1e-7, max_iteracoes = 200) {

  if (length(alvos) == 0) stop("nenhuma margem declarada em calibracao.margens")

  por_alvo <- purrr::map(alvos, ~ setdiff(names(.x), "Freq"))
  usadas <- unique(unlist(por_alvo))

  por_alvo <- purrr::map(por_alvo, ~ .x[order(match(.x, usadas))])

  faltando <- setdiff(usadas, names(dados))
  if (length(faltando) > 0) {
    stop("variavel de calibracao ausente na base: ",
         paste(faltando, collapse = ", "))
  }

  completos <- dados %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(usadas), ~ !is.na(.x)))

  if (nrow(completos) == 0) {
    stop("nenhum caso completo nas variaveis de calibracao")
  }

  for (v in usadas) {
    if (is.factor(completos[[v]])) {
      completos[[v]] <- droplevels(factor(completos[[v]], ordered = FALSE))
    }
  }

  conferir_celulas(completos, alvos, por_alvo)

  # Sem propensao configurada (ver R/propensao.R), completos nunca tem peso_inicial
  # e o comportamento e identico ao de sempre: peso uniforme antes do raking.
  peso_formula <- if ("peso_inicial" %in% names(completos)) ~peso_inicial else ~1

  desenho <- survey::calibrate(
    design = survey::svydesign(ids = ~1, data = completos, weights = peso_formula),
    formula = unname(purrr::map(
      por_alvo, ~ stats::as.formula(paste("~", paste(.x, collapse = " + ")))
    )),
    population = unname(alvos),
    calfun = "raking",
    maxit = max_iteracoes, epsilon = tolerancia, verbose = FALSE
  )

  list(design = desenho, alvos = alvos, n_calibrado = nrow(completos))
}

conferir_celulas <- function(dados, alvos, por_alvo) {

  for (nome in names(alvos)) {

    vars <- por_alvo[[nome]]

    amostra <- dados %>%
      dplyr::count(dplyr::across(dplyr::all_of(vars))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.character))

    cruzado <- alvos[[nome]] %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.character)) %>%
      dplyr::left_join(amostra, by = vars) %>%
      dplyr::mutate(n = dplyr::coalesce(n, 0L))

    sem_caso <- sum(cruzado$Freq > 0 & cruzado$n == 0)
    sem_pop <- sum(cruzado$Freq == 0 & cruzado$n > 0)

    if (sem_caso > 0) {
      stop("margem '", nome, "': ", sem_caso, " de ", nrow(cruzado),
           " celulas tem populacao e nenhum respondente. Cruze menos ",
           "variaveis, agrupe categorias, ou use margens marginais.",
           call. = FALSE)
    }

    if (sem_pop > 0) {
      stop("margem '", nome, "': ", sem_pop, " celula(s) com respondente e sem ",
           "populacao no alvo. Agrupe categorias: nenhum peso zera um caso.",
           call. = FALSE)
    }
  }

  invisible(TRUE)
}

trim_weights <- function(fit, teto = NULL, piso = NULL, strict = TRUE) {

  if (is.null(teto) && is.null(piso)) {
    fit$trimming <- list(aplicado = FALSE)
    return(fit)
  }

  pesos <- stats::weights(fit$design)
  media <- mean(pesos)
  limite_superior <- if (is.null(teto)) Inf else teto * media
  limite_inferior <- if (is.null(piso)) 0 else piso * media

  if (limite_inferior >= limite_superior) stop("piso maior ou igual ao teto")

  fit$design <- survey::trimWeights(fit$design, upper = limite_superior,
                                    lower = limite_inferior, strict = strict)

  fit$trimming <- list(aplicado = TRUE, teto = teto, piso = piso)

  fit
}

# ==============================================================================
# DIAGNOSTICOS
# ==============================================================================

convergence_report <- function(fit) {

  pesos <- stats::weights(fit$design)
  dados <- fit$design$variables

  purrr::imap_dfr(fit$alvos, function(alvo, nome) {

    vars <- setdiff(names(alvo), "Freq")

    observado <- dados %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      dplyr::mutate(.peso = pesos) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(vars))) %>%
      dplyr::summarise(estimado = sum(.peso), .groups = "drop") %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.character))

    alvo %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.character)) %>%
      dplyr::rename(alvo = Freq) %>%
      dplyr::left_join(observado, by = vars) %>%
      dplyr::mutate(
        margem = nome,
        categoria = do.call(paste, c(dplyr::across(dplyr::all_of(vars)),
                                     sep = " x ")),
        estimado = dplyr::coalesce(estimado, 0),
        pct_alvo = alvo / sum(alvo),
        pct_estimado = estimado / sum(estimado),
        desvio_pp = 100 * (pct_estimado - pct_alvo)
      ) %>%
      dplyr::select(margem, categoria, alvo, estimado, pct_alvo, pct_estimado,
                    desvio_pp)
  })
}

design_effect <- function(desenho) {

  pesos <- stats::weights(desenho)
  n_eff <- sum(pesos)^2 / sum(pesos^2)

  list(
    n_eff = n_eff,
    deff = length(pesos) / n_eff,
    razao_max = max(pesos) / mean(pesos),
    moe_pp = 100 * 1.96 * sqrt(0.25 / n_eff)
  )
}
