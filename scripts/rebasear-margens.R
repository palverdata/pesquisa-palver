# Leva as margens de voto 2022 e de filiacao para outra base da PNADc, sem
# reler os insumos do TSE: a margem de voto guarda os votos de cada celula e a de
# filiacao os filiados por partido. Rode depois de gerar-margens-pnadc.R.

margens_pnadc <- "margens/pnadc-2025-visita1.yaml"
sufixo        <- "pnadc2025v1"

margem_tse      <- "margens/tse-2022-turno2.yaml"
margem_filiacao <- "margens/tse-filiacao-2026-08.yaml"

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

source("R/onda.R")
suppressPackageStartupMessages(library(yaml))

pnadc <- carregar_margens(margens_pnadc)$tabela
pop_regiao <- pnadc %>%
  group_by(reg_std = as.character(reg_std)) %>%
  summarise(pop = sum(freq), .groups = "drop")
pop_total <- sum(pnadc$freq)

nova_saida <- function(arquivo) {
  sub("\\.yaml$", paste0("-", sufixo, ".yaml"), arquivo)
}

carimbo <- function(meta, escala) {
  meta$derivado_de <- margens_pnadc
  meta$escala <- escala
  meta$gerado_por <- "scripts/rebasear-margens.R"
  meta$gerado_em <- format(Sys.Date(), "%Y-%m-%d")
  meta
}

# 1. VOTO 2022: o percentual de voto da regiao vale para a populacao nova

tse <- ler_yaml(margem_tse)
celulas <- purrr::map_dfr(tse$conjunta, ~ tibble(reg_std = .x$reg_std,
                                                vote_std = .x$vote_std,
                                                votos = .x$votos)) %>%
  left_join(pop_regiao, by = "reg_std") %>%
  group_by(reg_std) %>%
  mutate(pct_na_regiao = votos / sum(votos),
         freq = arredondar_preservando_total(pct_na_regiao * pop)) %>%
  ungroup()

if (anyNA(celulas$pop)) stop("regiao de ", margem_tse, " ausente em ", margens_pnadc)

tse$meta <- carimbo(tse$meta, paste(
  "contagem POPULACIONAL. TABELA CONJUNTA regiao x voto, rebaseada de",
  paste0(margem_tse, ":"), "o percentual de voto do TSE dentro de cada regiao",
  "e aplicado a populacao regional de", paste0(margens_pnadc, ".")))
tse$meta$n_pop <- as.integer(sum(celulas$freq))
tse$conjunta <- purrr::pmap(celulas, function(reg_std, vote_std, votos, pop,
                                              pct_na_regiao, freq) {
  list(reg_std = reg_std, vote_std = vote_std, freq = as.integer(freq),
       votos = as.integer(votos), pct_na_regiao = round(pct_na_regiao, 6))
})
write_yaml(tse, nova_saida(margem_tse))
cat("escrito:", nova_saida(margem_tse), "\n")

# 2. FILIACAO: os filiados ficam; nao filiado e a populacao nova menos eles

fil <- ler_yaml(margem_filiacao)
niveis <- purrr::map_chr(fil$conjunta, "filiacao_std")
freq <- purrr::map_dbl(fil$conjunta, "freq")
freq[niveis == "Não filiado"] <- pop_total - sum(freq[niveis != "Não filiado"])
if (any(freq <= 0)) stop("filiacao sem populacao em alguma categoria")

fil$meta <- carimbo(fil$meta, paste(
  "contagem POPULACIONAL, rebaseada de", paste0(margem_filiacao, ":"),
  "os filiados por partido ficam; Nao filiado e a populacao de",
  margens_pnadc, "menos o total de filiados."))
fil$meta$n_pop <- as.integer(sum(freq))
fil$conjunta <- purrr::map2(niveis, freq, ~ list(filiacao_std = .x,
                                                 freq = as.integer(.y)))
write_yaml(fil, nova_saida(margem_filiacao))
cat("escrito:", nova_saida(margem_filiacao), "\n")
