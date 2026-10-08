# Gera as margens de voto no 1o turno de 2026, por UF e por regiao, na escala
# da populacao da PNADc. Mesmo desenho de tse-2022-turno2.yaml: o percentual
# entre quem compareceu vale para a populacao inteira, e na amostra quem nao
# votou entra em Branco/Nulo. Rode depois de baixar-resultado-tse.R e de
# gerar-margens-pnadc.R para cada base.

pasta_tse <- "insumos/tse/presidente_2026_t1"

# cada base gera o seu par de margens, com a base no nome do arquivo: misturar
# bases no raking reprova a conferencia de reg_std
bases_pnadc <- list(c(ano = 2024, visita = 5), c(ano = 2025, visita = 1))

# numero na urna -> nivel proprio; os demais candidatos vao para Outros
candidatos <- c("22" = "Flávio Bolsonaro", "13" = "Lula", "70" = "Augusto Cury",
                "14" = "Renan Santos", "55" = "Ronaldo Caiado")

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

source("R/onda.R")
suppressPackageStartupMessages({
  library(yaml)
  library(PNADcIBGE)
})

regiao_de <- c(
  AC = "N", AP = "N", AM = "N", PA = "N", RO = "N", RR = "N", TO = "N",
  AL = "NE", BA = "NE", CE = "NE", MA = "NE", PB = "NE", PE = "NE",
  PI = "NE", RN = "NE", SE = "NE",
  DF = "CO", GO = "CO", MT = "CO", MS = "CO",
  ES = "SE", MG = "SE", RJ = "SE", SP = "SE",
  PR = "S", RS = "S", SC = "S"
)
niveis_regiao <- c("N", "NE", "CO", "SE", "S")
niveis_uf <- names(regiao_de)[order(match(regiao_de, niveis_regiao), names(regiao_de))]
niveis_voto <- c(unname(candidatos), "Outros", "Branco/Nulo")

# 1. RESULTADO DO TSE

totais <- readr::read_csv(file.path(pasta_tse, "totais_por_uf.csv"),
                          show_col_types = FALSE)
votos <- readr::read_csv(file.path(pasta_tse, "votos_por_uf.csv"),
                         col_types = "ccccd")

if (any(totais$totalizacao_final != "s")) {
  message("aviso: totalizacao ainda nao marcada como final no TSE; ",
          "rode baixar-resultado-tse.R de novo quando for")
}

# Exterior fica fora: nao existe no universo da PNADc.
tse_uf <- votos %>%
  filter(abrangencia %in% names(regiao_de)) %>%
  mutate(voto = coalesce(unname(candidatos[numero]), "Outros")) %>%
  group_by(uf = abrangencia, voto) %>%
  summarise(votos = sum(votos), .groups = "drop") %>%
  bind_rows(totais %>%
              filter(abrangencia %in% names(regiao_de)) %>%
              transmute(uf = abrangencia, voto = "Branco/Nulo",
                        votos = brancos + nulos))

conferencia <- tse_uf %>%
  group_by(abrangencia = uf) %>%
  summarise(v = sum(votos)) %>%
  left_join(totais, by = "abrangencia")
if (any(conferencia$v != conferencia$comparecimento)) {
  stop("votos por UF nao somam o comparecimento do TSE")
}

siglas <- c("Rondônia" = "RO", "Acre" = "AC", "Amazonas" = "AM",
            "Roraima" = "RR", "Pará" = "PA", "Amapá" = "AP",
            "Tocantins" = "TO", "Maranhão" = "MA", "Piauí" = "PI",
            "Ceará" = "CE", "Rio Grande do Norte" = "RN", "Paraíba" = "PB",
            "Pernambuco" = "PE", "Alagoas" = "AL", "Sergipe" = "SE",
            "Bahia" = "BA", "Minas Gerais" = "MG", "Espírito Santo" = "ES",
            "Rio de Janeiro" = "RJ", "São Paulo" = "SP", "Paraná" = "PR",
            "Santa Catarina" = "SC", "Rio Grande do Sul" = "RS",
            "Mato Grosso do Sul" = "MS", "Mato Grosso" = "MT", "Goiás" = "GO",
            "Distrito Federal" = "DF")

distribuir <- function(tabela, chave) {
  tabela %>%
    group_by(.data[[chave]]) %>%
    mutate(pct = votos / sum(votos),
           freq = arredondar_preservando_total(pct * first(pop))) %>%
    ungroup()
}

confere <- function(tabela, chave, alvo) {
  soma <- tabela %>% group_by(k = .data[[chave]]) %>% summarise(f = sum(freq))
  ok <- soma %>% left_join(alvo, by = c(k = chave)) %>% with(all(f == pop))
  if (!ok) stop("totais por ", chave, " nao batem com a PNADc")
}

gravar <- function(tabela, chave, niveis_chave, arquivo) {
  tabela <- tabela %>%
    arrange(match(.data[[chave]], niveis_chave), match(voto, niveis_voto))
  if (nrow(tabela) != length(niveis_chave) * length(niveis_voto)) {
    stop(arquivo, ": celula ausente")
  }
  variaveis <- c(chave, "vote_t1_std")
  conteudo <- list(
    meta = list(
      fonte = "TSE, divulgacao oficial do 1o turno de 2026 (resultados.tse.jus.br)",
      arquivos = list(file.path(pasta_tse, "totais_por_uf.csv"),
                      file.path(pasta_tse, "votos_por_uf.csv")),
      gerado_tse = unique(totais$gerado[totais$abrangencia == "BR"]),
      totalizacao_final = unique(totais$totalizacao_final[totais$abrangencia == "BR"]),
      cargo = "Presidente",
      turno = 1L,
      definicao = paste(
        "Candidatos com nivel proprio pelo numero na urna:",
        paste0(paste(sprintf("%s (%s)", candidatos, names(candidatos)),
                     collapse = ", "), ";"),
        "os demais em Outros. Branco/Nulo: brancos + nulos. Abstencao nao",
        "entra: a margem e sobre comparecimento. Exterior (ZZ) excluido, fora",
        "do universo da PNADc."),
      derivado_de = margens_pnadc,
      escala = paste(
        "contagem POPULACIONAL. O percentual de voto do TSE entre quem",
        "compareceu e aplicado a populacao inteira do recorte, como em",
        "tse-2022-turno2.yaml. A populacao por UF e a distribuicao da PNADc",
        ano_pnadc, "dentro de cada regiao, ajustada ao total regional de",
        paste0(margens_pnadc, "."), "`votos` fica em cada celula para auditoria."),
      variaveis = as.list(variaveis),
      n_celulas = nrow(tabela),
      n_pop = as.integer(sum(tabela$freq)),
      n_votos = as.integer(sum(tabela$votos)),
      gerado_por = "scripts/gerar-margens-tse-2026.R",
      gerado_em = format(Sys.Date(), "%Y-%m-%d")
    ),
    niveis = setNames(list(as.list(niveis_chave), as.list(niveis_voto)), variaveis),
    conjunta = purrr::pmap(tabela, function(...) {
      l <- list(...)
      c(setNames(list(l[[chave]], l$voto), variaveis),
        list(freq = as.integer(l$freq), votos = as.integer(l$votos),
             pct = round(l$pct, 6)))
    })
  )
  write_yaml(conteudo, file.path("margens", arquivo))
  cat("escrito: margens/", arquivo, " (", nrow(tabela), " celulas)\n", sep = "")
}

for (base in bases_pnadc) {

  ano_pnadc <- base[["ano"]]
  visita_pnadc <- base[["visita"]]
  margens_pnadc <- sprintf("margens/pnadc-%d-visita%d.yaml", ano_pnadc, visita_pnadc)
  sufixo <- sprintf("pnadc%dv%d", ano_pnadc, visita_pnadc)

  # 2. POPULACAO POR UF NA ESCALA DA PNADc

  # A distribuicao entre as UFs de cada regiao vem da PNADc; o total da regiao e
  # o de margens_pnadc, para que reg_std bata com as demais margens.
  pop_regiao <- carregar_margens(margens_pnadc)$tabela %>%
    group_by(reg_std = as.character(reg_std)) %>%
    summarise(pop = sum(freq), .groups = "drop")

  message(sprintf("Baixando PNADc %d, entrevista %d (UF e idade) ...",
                  ano_pnadc, visita_pnadc))
  pnadc <- get_pnadc(year = ano_pnadc, interview = visita_pnadc,
                     vars = c("UF", "V2009"), design = FALSE)

  pop_uf <- pnadc %>%
    filter(V2009 >= 16) %>%
    group_by(uf = unname(siglas[as.character(UF)])) %>%
    summarise(peso = sum(V1032), .groups = "drop") %>%
    mutate(reg_std = unname(regiao_de[uf])) %>%
    left_join(pop_regiao, by = "reg_std") %>%
    group_by(reg_std) %>%
    mutate(pop = arredondar_preservando_total(peso / sum(peso) * pop)) %>%
    ungroup() %>%
    select(uf, reg_std, pop)

  stopifnot(setequal(pop_uf$uf, names(regiao_de)), !anyNA(pop_uf$reg_std))

  # 3. MONTAGEM: percentual entre quem compareceu, aplicado a populacao toda

  margem_uf <- tse_uf %>% left_join(pop_uf, by = "uf") %>% distribuir("uf")

  margem_regiao <- tse_uf %>%
    mutate(reg_std = unname(regiao_de[uf])) %>%
    group_by(reg_std, voto) %>%
    summarise(votos = sum(votos), .groups = "drop") %>%
    left_join(pop_regiao, by = "reg_std") %>%
    distribuir("reg_std")

  confere(margem_uf, "uf", pop_uf)
  confere(margem_regiao, "reg_std", pop_regiao)

  # 4. SAIDA

  gravar(margem_uf, "uf", niveis_uf,
         sprintf("tse-2026-turno1-uf-%s.yaml", sufixo))
  gravar(margem_regiao, "reg_std", niveis_regiao,
         sprintf("tse-2026-turno1-regiao-%s.yaml", sufixo))

  cat("\n--- Brasil, % da populacao 16+, base", sufixo, "---\n")
  print(margem_regiao %>% group_by(voto) %>% summarise(freq = sum(freq)) %>%
          mutate(pct = round(100 * freq / sum(freq), 2)) %>%
          arrange(match(voto, niveis_voto)))
}
