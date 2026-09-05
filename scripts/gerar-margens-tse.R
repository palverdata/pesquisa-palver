# Gera margens/tse-2022-turno2.yaml: voto no 2o turno por regiao, na escala da
# populacao regional da PNADc. Rode depois de gerar-margens-pnadc.R.

arq_candidatos <- "insumos/tse/votacao_candidato_munzona_2022_BRASIL.csv"
arq_detalhe    <- "insumos/tse/detalhe_votacao_munzona_2022_BR.csv"
margens_pnadc  <- "margens/pnadc-2024-visita5.yaml"

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

turno <- 2L
cargo <- "Presidente"

# Exterior fica fora: nao existe no universo da PNADc.
uf_excluidas <- c("ZZ")

candidatos <- c(
  "LUIZ INÁCIO LULA DA SILVA" = "Lula",
  "JAIR MESSIAS BOLSONARO" = "Jair Bolsonaro"
)

saida <- "margens/tse-2022-turno2.yaml"

# 1. MOTOR

# Reusa ler_yaml (sem a coercao de booleano do YAML 1.1, que transformaria a
# regiao "N" em FALSE), carregar_margens e arredondar_preservando_total.
source("R/onda.R")
suppressPackageStartupMessages(library(yaml))

regiao_de <- c(
  AC = "N", AP = "N", AM = "N", PA = "N", RO = "N", RR = "N", TO = "N",
  AL = "NE", BA = "NE", CE = "NE", MA = "NE", PB = "NE", PE = "NE",
  PI = "NE", RN = "NE", SE = "NE",
  DF = "CO", GO = "CO", MT = "CO", MS = "CO",
  ES = "SE", MG = "SE", RJ = "SE", SP = "SE",
  PR = "S", RS = "S", SC = "S"
)

niveis_regiao <- c("N", "NE", "CO", "SE", "S")
niveis_voto <- c("Lula", "Jair Bolsonaro", "Branco/Nulo")

# 2. VOTOS NOMINAIS (leitura em blocos)

# 50 colunas; le turno (6), UF (11), cargo (18), candidato (21), votos (46).
col_spec <- "-----i----c------c--c------------------------i----"

acumulado <- NULL

acumular <- function(bloco, pos) {

  parcial <- bloco %>%
    filter(
      NR_TURNO == turno,
      DS_CARGO == cargo,
      !SG_UF %in% uf_excluidas,
      NM_CANDIDATO %in% names(candidatos)
    ) %>%
    mutate(vote_std = unname(candidatos[NM_CANDIDATO])) %>%
    group_by(SG_UF, vote_std) %>%
    summarise(votos = sum(QT_VOTOS_NOMINAIS, na.rm = TRUE), .groups = "drop")

  acumulado <<- bind_rows(acumulado, parcial)
}

message("lendo votos nominais em blocos: ", arq_candidatos)

read_delim_chunked(
  arq_candidatos,
  callback = DataFrameCallback$new(function(x, pos) {
    acumular(x, pos)
    NULL
  }),
  chunk_size = 500000,
  delim = ";",
  col_names = TRUE,
  col_types = col_spec,
  locale = locale(encoding = "latin1", decimal_mark = ",", grouping_mark = "."),
  escape_double = FALSE,
  trim_ws = TRUE,
  progress = FALSE
)

votos_uf <- acumulado %>%
  group_by(SG_UF, vote_std) %>%
  summarise(votos = sum(votos), .groups = "drop")

# 3. BRANCOS, NULOS E COMPARECIMENTO

message("lendo detalhe: ", arq_detalhe)

detalhe <- read_delim(
  arq_detalhe,
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",", grouping_mark = "."),
  escape_double = FALSE,
  trim_ws = TRUE,
  progress = FALSE,
  show_col_types = FALSE
) %>%
  filter(
    NR_TURNO == turno,
    DS_CARGO == cargo,
    !SG_UF %in% uf_excluidas
  )

brancos_nulos_uf <- detalhe %>%
  group_by(SG_UF) %>%
  summarise(
    votos = sum(QT_VOTOS_BRANCOS, na.rm = TRUE) +
      sum(QT_VOTOS_NULOS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(vote_std = "Branco/Nulo")

# Referencia para a decisao sobre abstencao (nao entra na margem).
comparecimento_uf <- detalhe %>%
  group_by(SG_UF) %>%
  summarise(
    aptos = sum(QT_APTOS, na.rm = TRUE),
    comparecimento = sum(QT_COMPARECIMENTO, na.rm = TRUE),
    abstencoes = sum(QT_ABSTENCOES, na.rm = TRUE),
    .groups = "drop"
  )

# 4. MONTAGEM

margem <- bind_rows(votos_uf, brancos_nulos_uf) %>%
  mutate(reg_std = unname(regiao_de[SG_UF])) %>%
  filter(!is.na(reg_std)) %>%
  group_by(reg_std, vote_std) %>%
  summarise(votos = sum(votos), .groups = "drop") %>%
  group_by(reg_std) %>%
  mutate(pct_na_regiao = votos / sum(votos)) %>%
  ungroup() %>%
  arrange(match(reg_std, niveis_regiao), match(vote_std, niveis_voto))

uf_sem_regiao <- setdiff(unique(c(votos_uf$SG_UF, brancos_nulos_uf$SG_UF)),
                         c(names(regiao_de), uf_excluidas))
if (length(uf_sem_regiao) > 0) {
  stop("UF sem regiao definida: ", paste(uf_sem_regiao, collapse = ", "))
}

faltando <- setdiff(
  paste(rep(niveis_regiao, each = 3), niveis_voto),
  paste(margem$reg_std, margem$vote_std)
)
if (length(faltando) > 0) {
  stop("celula regiao x voto ausente: ", paste(faltando, collapse = "; "))
}

referencia <- comparecimento_uf %>%
  mutate(reg_std = unname(regiao_de[SG_UF])) %>%
  filter(!is.na(reg_std)) %>%
  group_by(reg_std) %>%
  summarise(across(c(aptos, comparecimento, abstencoes), sum), .groups = "drop") %>%
  arrange(match(reg_std, niveis_regiao))

# 4b. ESCALA: POPULACAO DA PNADc

pop_regional <- carregar_margens(margens_pnadc)$tabela %>%
  group_by(reg_std = as.character(reg_std)) %>%
  summarise(pop = sum(freq), .groups = "drop")

faltando_reg <- setdiff(niveis_regiao, pop_regional$reg_std)
if (length(faltando_reg) > 0) {
  stop("regiao sem populacao em ", margens_pnadc, ": ",
       paste(faltando_reg, collapse = ", "))
}

margem <- margem %>%
  left_join(pop_regional, by = "reg_std") %>%
  group_by(reg_std) %>%
  mutate(freq = arredondar_preservando_total(pct_na_regiao * pop)) %>%
  ungroup() %>%
  select(reg_std, vote_std, votos, pct_na_regiao, freq)

# 5. DIAGNOSTICO

cat("
--- regiao x voto: voto do TSE, contagem na escala da PNADc ---
")
print(
  margem %>%
    mutate(pct_regiao = sprintf("%.1f%%", 100 * pct_na_regiao)) %>%
    select(reg_std, vote_std, votos, pct_regiao, freq),
  n = 20
)

cat("
--- regiao: a distribuicao tem de bater com a da PNADc ---
")
print(
  margem %>%
    group_by(reg_std) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    left_join(pop_regional, by = "reg_std") %>%
    mutate(pct = sprintf("%.2f%%", 100 * freq / sum(freq)),
           confere = freq == pop)
)

cat("
--- voto no total nacional ---
")
print(
  margem %>%
    group_by(vote_std) %>%
    summarise(votos = sum(votos), freq = sum(freq), .groups = "drop") %>%
    mutate(pct_votos = sprintf("%.2f%%", 100 * votos / sum(votos)),
           pct_alvo = sprintf("%.2f%%", 100 * freq / sum(freq))) %>%
    select(vote_std, pct_votos, pct_alvo)
)

cat("
--- comparecimento (referencia, fora da margem) ---
")
print(referencia)

# 6. SAIDA

conteudo <- list(
  meta = list(
    fonte = "TSE, resultados oficiais do 2o turno de 2022 (munzona)",
    arquivos = list(basename(arq_candidatos), basename(arq_detalhe)),
    cargo = cargo,
    turno = turno,
    definicao = paste(
      "Lula e Jair Bolsonaro: QT_VOTOS_NOMINAIS.",
      "Branco/Nulo: QT_VOTOS_BRANCOS + QT_VOTOS_NULOS.",
      "Abstencao NAO entra: a margem e sobre comparecimento.",
      "Exterior (SG_UF = ZZ) excluido, fora do universo da PNADc."
    ),
    derivado_de = margens_pnadc,
    escala = paste(
      "contagem POPULACIONAL. TABELA CONJUNTA regiao x voto: qualquer",
      "cruzamento, inclusive as marginais, e obtido somando as celulas.",
      "Cada celula e a populacao regional de", margens_pnadc, "distribuida pelo",
      "percentual de voto do TSE dentro daquela regiao -- entao reg_std tem",
      "aqui a mesma distribuicao da PNADc, e as duas fontes podem ser usadas",
      "juntas. `votos` fica em cada celula para auditoria da derivacao."
    ),
    variaveis = list("reg_std", "vote_std"),
    n_celulas = nrow(margem),
    celulas_zeradas = sum(margem$freq == 0),
    n_pop = as.integer(sum(margem$freq)),
    n_votos = as.integer(sum(margem$votos)),
    gerado_por = "scripts/gerar-margens-tse.R",
    gerado_em = format(Sys.Date(), "%Y-%m-%d")
  ),

  niveis = list(
    reg_std = as.list(niveis_regiao),
    vote_std = as.list(niveis_voto)
  ),

  referencia_comparecimento = pmap(
    referencia,
    function(reg_std, aptos, comparecimento, abstencoes) {
      list(reg_std = reg_std, aptos = as.integer(aptos),
           comparecimento = as.integer(comparecimento),
           abstencoes = as.integer(abstencoes))
    }
  ),

  conjunta = pmap(
    margem,
    function(reg_std, vote_std, votos, pct_na_regiao, freq) {
      list(reg_std = reg_std, vote_std = vote_std, freq = as.integer(freq),
           votos = as.integer(votos),
           pct_na_regiao = round(pct_na_regiao, 6))
    }
  )
)

dir.create("margens", showWarnings = FALSE)
write_yaml(conteudo, saida)

cat("\nescrito: ", saida, "\n", sep = "")
