
# ==============================================================================
# PROPENSAO A PARTICIPACAO (PESO INICIAL POR RANDOM FOREST)
#
# Etapa opcional: quando a onda declara `propensao` no config.yaml, o peso inicial
# do svydesign (ver rake_weights em R/calibracao.R) deixa de ser uniforme e passa a
# refletir a propensao de cada respondente a ter participado do painel, estimada por
# um random forest que empilha o painel contra microdados individuais da PNADc. O
# raking, que ja existia, continua sendo a etapa que ajusta esse peso contra as
# margens declaradas -- esta etapa so troca o ponto de partida.
# ==============================================================================

baixar_pnadc_propensao <- function(ano, entrevista, sm) {

  # get_pnadc() faz lookups internos que exigem o pacote anexado (mesma exigencia
  # de scripts/gerar-margens-pnadc.R) -- so :: nao basta.
  library(PNADcIBGE)

  vars <- c("ID_DOMICILIO", "Ano", "Trimestre", "UF", "Capital", "RM_RIDE",
           "V2007", "V2009", "VD3004", "VD5007")

  message(sprintf("baixando PNADc %d, entrevista %d para o modelo de propensao ...",
                  ano, entrevista))

  pnadc <- get_pnadc(year = ano, interview = entrevista, vars = vars,
                     design = FALSE)

  # Mesmos recodes de sex_std, reg_std e edu_std usados em
  # scripts/gerar-margens-pnadc.R -- divergir aqui desalinharia o modelo de
  # propensao das margens que a mesma onda usa para o raking.
  pnadc %>%
    dplyr::rename(sexo = V2007, idade = V2009, esc = VD3004, renda_fam = VD5007) %>%
    dplyr::filter(idade >= 16) %>%
    dplyr::mutate(
      sex_std = dplyr::if_else(sexo %in% c("Homem", "Mulher"), as.character(sexo),
                               NA_character_),

      # As 5 faixas originais da PNADc, sem o colapso que sex_std/age_std usam para
      # as margens -- o modelo de propensao usa a granularidade fina.
      age = dplyr::case_when(
        idade %in% 16:24 ~ "16 a 24 anos",
        idade %in% 25:34 ~ "25 a 34 anos",
        idade %in% 35:44 ~ "35 a 44 anos",
        idade %in% 45:59 ~ "45 a 59 anos",
        idade >= 60 ~ "60 anos ou mais"
      ),

      edu_std = dplyr::case_when(
        esc %in% c("Sem instrução e menos de 1 ano de estudo",
                  "Fundamental incompleto ou equivalente",
                  "Fundamental completo ou equivalente",
                  "Médio incompleto ou equivalente") ~ "Fundamental",
        esc %in% c("Médio completo ou equivalente",
                  "Superior incompleto ou equivalente") ~ "Médio",
        esc == "Superior completo" ~ "Superior"
      ),

      inc_std_2 = dplyr::case_when(
        renda_fam / sm <= 1 ~ "0-1 SM",
        renda_fam / sm <= 2 ~ "1-2 SM",
        renda_fam / sm <= 3 ~ "2-3 SM",
        renda_fam / sm <= 5 ~ "3-5 SM",
        renda_fam / sm > 5 ~ "5+ SM"
      ),

      reg_std = dplyr::case_when(
        UF %in% c("Amazonas", "Pará", "Roraima", "Amapá", "Rondônia", "Acre",
                  "Tocantins") ~ "N",
        UF %in% c("Piauí", "Maranhão", "Pernambuco", "Rio Grande do Norte",
                  "Paraíba", "Ceará", "Bahia", "Alagoas", "Sergipe") ~ "NE",
        UF %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás",
                  "Distrito Federal") ~ "CO",
        UF %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo",
                  "Minas Gerais") ~ "SE",
        UF %in% c("Rio Grande do Sul", "Paraná", "Santa Catarina") ~ "S"
      ),

      tipo_mun_std = dplyr::case_when(
        !is.na(Capital) ~ "Capital",
        !is.na(RM_RIDE) ~ "RM",
        TRUE ~ "Interior"
      ),

      participated_panel = "pnadc"
    ) %>%
    dplyr::select(sex_std, age, edu_std, inc_std_2, reg_std, tipo_mun_std,
                 participated_panel) %>%
    tibble::as_tibble()
}

# Empilha o painel (`base`, ja com as variaveis padronizadas do questionario.yaml)
# contra os microdados da PNADc, ajusta um random forest de classificacao
# (participated_panel: panel x pnadc) e devolve `base` com uma coluna `peso_inicial`
# -- (1-p)/p a partir da probabilidade OOB de cada respondente do painel, normalizada
# para media 1. Quem fica de fora do ajuste (variavel de propensao ausente) recebe
# `peso_fallback` (neutro = 1, configuravel via propensao.peso_fallback).
montar_propensao <- function(base, pnadc, variaveis, semente = 1234,
                             arvores = 1000, no_minimo = 20, peso_fallback = 1) {

  faltando_base <- setdiff(variaveis, names(base))
  if (length(faltando_base) > 0) {
    stop("propensao.variaveis ausente na base do painel: ",
         paste(faltando_base, collapse = ", "))
  }
  faltando_pnadc <- setdiff(variaveis, names(pnadc))
  if (length(faltando_pnadc) > 0) {
    stop("propensao.variaveis ausente nos microdados da PNADc: ",
         paste(faltando_pnadc, collapse = ", "))
  }

  painel <- base %>%
    dplyr::mutate(.linha_painel = dplyr::row_number(),
                  participated_panel = "panel",
                  dplyr::across(dplyr::all_of(variaveis), as.character)) %>%
    dplyr::select(.linha_painel, participated_panel, dplyr::all_of(variaveis))

  pnadc <- pnadc %>%
    dplyr::mutate(.linha_painel = NA_integer_,
                  dplyr::across(dplyr::all_of(variaveis), as.character)) %>%
    dplyr::select(.linha_painel, participated_panel, dplyr::all_of(variaveis))

  # As categorias das duas fontes so ficam consistentes depois de empilhar --
  # fatorar aqui, e nao antes, evita que painel e pnadc criem niveis diferentes.
  empilhado <- dplyr::bind_rows(painel, pnadc) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(variaveis), factor),
      participated_panel = factor(participated_panel, levels = c("pnadc", "panel"))
    )

  completo <- empilhado %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(variaveis), ~ !is.na(.x)))

  n_painel_excluido <- sum(painel$participated_panel == "panel") -
    sum(completo$participated_panel == "panel")
  if (n_painel_excluido > 0) {
    message(sprintf(
      "propensao: %d respondente(s) do painel sem uma das variaveis (%s) -- ",
      n_painel_excluido, paste(variaveis, collapse = ", ")),
      sprintf("ficam com peso inicial %s", peso_fallback))
  }

  modelo <- ranger::ranger(
    formula = participated_panel ~ .,
    data = dplyr::select(completo, participated_panel, dplyr::all_of(variaveis)),
    probability = TRUE, num.trees = arvores, min.node.size = no_minimo,
    respect.unordered.factors = "order", importance = "permutation", seed = semente
  )

  # ranger devolve a probabilidade OOB para as linhas de treino -- e o que evita
  # que o peso reflita apenas o ajuste (sobreajustado) do proprio modelo aos dados
  # que ele mesmo usou.
  p <- pmin(pmax(modelo$predictions[, "panel"], 1e-6), 1 - 1e-6)

  pesos_painel <- tibble::tibble(
    .linha_painel = completo$.linha_painel,
    participated_panel = completo$participated_panel,
    peso_bruto = (1 - p) / p
  ) %>%
    dplyr::filter(participated_panel == "panel")

  pesos_painel$peso_inicial <- pesos_painel$peso_bruto /
    mean(pesos_painel$peso_bruto)

  base$peso_inicial <- peso_fallback
  base$peso_inicial[pesos_painel$.linha_painel] <- pesos_painel$peso_inicial

  list(
    base = base,
    diagnostico = list(
      n_pnadc = sum(empilhado$participated_panel == "pnadc"),
      n_painel_incluido = nrow(pesos_painel),
      n_painel_fallback = n_painel_excluido,
      erro_oob = modelo$prediction.error,
      peso_min = min(pesos_painel$peso_inicial),
      peso_max = max(pesos_painel$peso_inicial)
    )
  )
}
