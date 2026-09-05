# Peso inicial (1-p)/p, com p a probabilidade OOB de um random forest que
# empilha o painel contra a conjunta da PNADc. Opcional; o raking segue igual.

montar_propensao <- function(base, conjunta, variaveis, semente = 1234,
                             arvores = 1000, no_minimo = 20,
                             peso_fallback = 1) {

  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("propensao.ativo exige o pacote ranger: install.packages(\"ranger\")",
         call. = FALSE)
  }

  faltando <- setdiff(variaveis, names(base))
  if (length(faltando) > 0) {
    stop("propensao.variaveis ausente na base do painel: ",
         paste(faltando, collapse = ", "), call. = FALSE)
  }
  faltando <- setdiff(variaveis, names(conjunta))
  if (length(faltando) > 0) {
    stop("propensao.variaveis ausente na conjunta da PNADc: ",
         paste(faltando, collapse = ", "),
         ". Regere as margens com scripts/gerar-margens-pnadc.R.",
         call. = FALSE)
  }

  painel <- base %>%
    dplyr::mutate(.linha_painel = dplyr::row_number(),
                  participated_panel = "panel",
                  .peso_caso = 1,
                  dplyr::across(dplyr::all_of(variaveis), as.character)) %>%
    dplyr::select(.linha_painel, participated_panel, .peso_caso,
                  dplyr::all_of(variaveis))

  # Uma linha por celula, com peso; a populacao pesa o mesmo que o painel. Com
  # covariaveis categoricas isso e toda a informacao do microdado.
  populacao <- conjunta %>%
    dplyr::filter(freq > 0) %>%
    dplyr::mutate(.linha_painel = NA_integer_,
                  participated_panel = "pnadc",
                  .peso_caso = freq / sum(freq) * nrow(painel),
                  dplyr::across(dplyr::all_of(variaveis), as.character)) %>%
    dplyr::select(.linha_painel, participated_panel, .peso_caso,
                  dplyr::all_of(variaveis))

  # fatorar depois de empilhar: as duas fontes tem de partilhar os niveis
  empilhado <- dplyr::bind_rows(painel, populacao) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(variaveis), factor),
      participated_panel = factor(participated_panel,
                                  levels = c("pnadc", "panel"))
    )

  completo <- empilhado %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(variaveis), ~ !is.na(.x)))

  e_painel <- completo$participated_panel == "panel"
  n_fallback <- nrow(painel) - sum(e_painel)
  if (n_fallback > 0) {
    message(sprintf("propensao: %d respondente(s) sem uma das variaveis (%s) ",
                    n_fallback, paste(variaveis, collapse = ", ")),
            sprintf("ficam com peso inicial %s", peso_fallback))
  }

  modelo <- ranger::ranger(
    formula = participated_panel ~ .,
    data = dplyr::select(completo, participated_panel,
                         dplyr::all_of(variaveis)),
    case.weights = completo$.peso_caso,
    probability = TRUE, num.trees = arvores, min.node.size = no_minimo,
    respect.unordered.factors = "order", seed = semente
  )

  # OOB: o peso nao pode refletir o ajuste do modelo aos proprios dados.
  p <- pmin(pmax(modelo$predictions[e_painel, "panel"], 1e-6), 1 - 1e-6)
  peso_bruto <- (1 - p) / p
  peso_inicial <- peso_bruto / mean(peso_bruto)

  base$peso_inicial <- peso_fallback
  base$peso_inicial[completo$.linha_painel[e_painel]] <- peso_inicial

  list(
    base = base,
    diagnostico = list(
      n_celulas = nrow(populacao),
      n_painel_incluido = sum(e_painel),
      n_painel_fallback = n_fallback,
      erro_oob = modelo$prediction.error,
      peso_min = min(peso_inicial),
      peso_max = max(peso_inicial)
    )
  )
}
