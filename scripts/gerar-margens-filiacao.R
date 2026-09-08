# Gera margens/tse-filiacao-2026-08.yaml: filiacao partidaria (nao filiado, PL,
# PT, Missao, outros filiados) na escala da populacao da PNADc. Rode depois de
# gerar-margens-pnadc.R.

arq_filiacao  <- "insumos/tse/perfil_filiacao_partidaria.csv"
margens_pnadc <- "margens/pnadc-2024-visita5.yaml"
saida         <- "margens/tse-filiacao-2026-08.yaml"

# Partidos com nivel proprio; o resto vira "Outros filiados".
partidos <- c(PL = "PL", PT = "PT", "MISSÃO" = "Missão")
niveis <- c("Não filiado", "PL", "PT", "Missão", "Outros filiados")

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

source("R/onda.R")
suppressPackageStartupMessages(library(yaml))

# 1. FILIADOS POR PARTIDO (leitura em blocos)
# 29 colunas; le NR_ANO_MES (3), SG_PARTIDO (5) e QT_FILIADO (29).
col_spec <- paste0("--c-c", strrep("-", 23), "i")

acumulado <- NULL
message("lendo filiados em blocos: ", arq_filiacao)
read_delim_chunked(
  arq_filiacao,
  callback = DataFrameCallback$new(function(x, pos) {
    parcial <- x %>%
      group_by(NR_ANO_MES, SG_PARTIDO) %>%
      summarise(filiados = sum(QT_FILIADO, na.rm = TRUE), .groups = "drop")
    acumulado <<- bind_rows(acumulado, parcial)
    NULL
  }),
  chunk_size = 1000000,
  delim = ";",
  col_names = TRUE,
  col_types = col_spec,
  locale = locale(encoding = "latin1"),
  escape_double = FALSE,
  trim_ws = TRUE,
  progress = FALSE
)

ano_mes <- unique(acumulado$NR_ANO_MES)
if (length(ano_mes) != 1) stop("mais de um NR_ANO_MES no arquivo: ", paste(ano_mes, collapse = ", "))

por_partido <- acumulado %>%
  group_by(SG_PARTIDO) %>%
  summarise(filiados = sum(filiados), .groups = "drop") %>%
  arrange(desc(filiados))

# 2. MONTAGEM: populacao 16+ da PNADc menos filiados = nao filiados
pop <- sum(purrr::map_dbl(ler_yaml(margens_pnadc)$conjunta, "freq"))

margem <- por_partido %>%
  mutate(filiacao_std = coalesce(unname(partidos[SG_PARTIDO]), "Outros filiados")) %>%
  group_by(filiacao_std) %>%
  summarise(freq = sum(filiados), .groups = "drop") %>%
  bind_rows(tibble(filiacao_std = "Não filiado", freq = pop - sum(.$freq))) %>%
  arrange(match(filiacao_std, niveis))

stopifnot(setequal(margem$filiacao_std, niveis), sum(margem$freq) == pop)

cat("\n--- filiacao no total nacional ---\n")
print(margem %>% mutate(pct = sprintf("%.2f%%", 100 * freq / sum(freq))))

# 3. SAIDA

conteudo <- list(
  meta = list(
    fonte = "TSE, perfil do eleitorado - filiacao partidaria",
    arquivo = basename(arq_filiacao),
    url = "https://cdn.tse.jus.br/estatistica/sead/odsele/filiacao_partidaria/perfil_filiacao_partidaria.zip",
    ano_mes = as.character(ano_mes),
    definicao = paste(
      "Filiados: soma de QT_FILIADO por partido; PL, PT e Missao tem nivel",
      "proprio, os demais partidos somam em Outros filiados.",
      "Nao filiado: populacao 16+ da PNADc menos o total de filiados."
    ),
    derivado_de = margens_pnadc,
    escala = paste(
      "contagem POPULACIONAL, no total nacional de", margens_pnadc,
      "para poder entrar no raking ao lado das outras margens."
    ),
    variaveis = list("filiacao_std"),
    n_celulas = nrow(margem),
    n_pop = as.integer(pop),
    n_filiados = as.integer(sum(por_partido$filiados)),
    gerado_por = "scripts/gerar-margens-filiacao.R",
    gerado_em = format(Sys.Date(), "%Y-%m-%d")
  ),

  niveis = list(filiacao_std = as.list(niveis)),

  # `filiados_por_partido` fica para auditoria; o motor le so a conjunta.
  filiados_por_partido = purrr::map2(por_partido$SG_PARTIDO, por_partido$filiados,
                                     ~ list(partido = .x, filiados = as.integer(.y))),

  conjunta = purrr::map2(margem$filiacao_std, margem$freq,
                         ~ list(filiacao_std = .x, freq = as.integer(.y)))
)

write_yaml(conteudo, saida)
cat("\nescrito:", saida, "\n")
