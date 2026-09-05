# Gera margens/pnadc-<ano>-visita<n>.yaml: a tabela conjunta genero x idade x
# escolaridade x renda x regiao, da PNADc. Rode quando a fonte mudar.

ano        <- 2024   # ano da PNADc
entrevista <- 5      # numero da visita
sm         <- 1621   # salario minimo das faixas de renda do questionario

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

idade_minima <- 16L  # eleitorado potencial

saida <- file.path("margens", sprintf("pnadc-%d-visita%d.yaml", ano, entrevista))

# 1. PACOTES

suppressPackageStartupMessages({
  library(PNADcIBGE)
  library(tidyverse)
  library(survey)
  library(yaml)
})

# 2. DOWNLOAD

vars <- c(
  "ID_DOMICILIO", "Ano", "Trimestre", "UF",
  "V2007",  # sexo
  "V2009",  # idade
  "VD3004", # escolaridade
  "VD5007"  # renda familiar
)

message(sprintf("Baixando PNADc %d, entrevista %d ...", ano, entrevista))

pnadc <- get_pnadc(year = ano, interview = entrevista, vars = vars,
                   design = FALSE)

# 3. RECODES

# Mesmos recodes aplicados a amostra da onda -- qualquer divergencia aqui
# desalinha silenciosamente a calibracao.

niveis <- list(
  sex_std = c("Homem", "Mulher"),
  age_std = c("16 a 34 anos", "35 a 59 anos", "60 anos ou mais"),
  edu_std = c("Fundamental", "Médio", "Superior"),
  inc_std = c("0-2 SM", "2-5 SM", "5+ SM"),
  reg_std = c("N", "NE", "CO", "SE", "S")
)

pnadc_std <- pnadc %>%
  rename(sexo = V2007, idade = V2009, esc = VD3004, renda_fam = VD5007) %>%
  filter(idade >= idade_minima) %>%
  mutate(
    sex_std = if_else(sexo %in% c("Homem", "Mulher"), as.character(sexo),
                      NA_character_),

    age_std = case_when(
      idade %in% 16:34 ~ "16 a 34 anos",
      idade %in% 35:59 ~ "35 a 59 anos",
      idade >= 60 ~ "60 anos ou mais"
    ),

    edu_std = case_when(
      esc %in% c("Sem instrução e menos de 1 ano de estudo",
                 "Fundamental incompleto ou equivalente",
                 "Fundamental completo ou equivalente",
                 "Médio incompleto ou equivalente") ~ "Fundamental",
      esc %in% c("Médio completo ou equivalente",
                 "Superior incompleto ou equivalente") ~ "Médio",
      esc == "Superior completo" ~ "Superior"
    ),

    inc_std = case_when(
      renda_fam / sm <= 2 ~ "0-2 SM",
      renda_fam / sm <= 5 ~ "2-5 SM",
      renda_fam / sm > 5 ~ "5+ SM"
    ),

    reg_std = case_when(
      UF %in% c("Amazonas", "Pará", "Roraima", "Amapá", "Rondônia", "Acre",
                "Tocantins") ~ "N",
      UF %in% c("Piauí", "Maranhão", "Pernambuco", "Rio Grande do Norte",
                "Paraíba", "Ceará", "Bahia", "Alagoas", "Sergipe") ~ "NE",
      UF %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás",
                "Distrito Federal") ~ "CO",
      UF %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo",
                "Minas Gerais") ~ "SE",
      UF %in% c("Rio Grande do Sul", "Paraná", "Santa Catarina") ~ "S"
    )
  )

# A tabela conjunta so admite casos completos nas cinco variaveis: e o que faz
# todas as margens derivadas somarem exatamente o mesmo total.
missing_pct <- map_dbl(names(niveis), ~ mean(is.na(pnadc_std[[.x]]))) %>%
  set_names(names(niveis))

completos <- pnadc_std %>%
  filter(if_all(all_of(names(niveis)), ~ !is.na(.x))) %>%
  mutate(across(all_of(names(niveis)), ~ factor(.x, levels = niveis[[cur_column()]])))

cat(sprintf("\ncasos completos nas 5 variaveis: %d de %d (perda %.3f%%)\n",
            nrow(completos), nrow(pnadc_std),
            100 * (nrow(pnadc_std) - nrow(completos)) / nrow(pnadc_std)))

# 4. TABELA CONJUNTA

desenho <- pnadc_design(data_pnadc = completos)

conjunta <- svytable(
  as.formula(paste("~", paste(names(niveis), collapse = " + "))),
  design = desenho,
  round = TRUE
) %>%
  as.data.frame() %>%
  rename(freq = Freq) %>%
  mutate(freq = as.integer(freq)) %>%
  arrange(across(all_of(names(niveis))))

n_pop <- sum(conjunta$freq)
zeradas <- sum(conjunta$freq == 0)

# 5. DIAGNOSTICO

cat(sprintf("\ntabela conjunta: %d celulas | %d zeradas | populacao %s\n",
            nrow(conjunta), zeradas, format(n_pop, big.mark = " ")))

if (zeradas > 0) {
  cat("\ncelulas sem populacao (a calibracao falha se a amostra tiver casos ai):\n")
  print(conjunta %>% filter(freq == 0))
}

cat("\n--- marginais derivadas da conjunta ---\n")

for (v in names(niveis)) {
  cat("\n", v, "\n", sep = "")
  print(
    conjunta %>%
      group_by(.data[[v]]) %>%
      summarise(freq = sum(freq), .groups = "drop") %>%
      mutate(pct = sprintf("%.2f%%", 100 * freq / sum(freq)))
  )
}

cat("\n--- missing por variavel (antes do corte de casos completos) ---\n")
print(round(missing_pct, 5))

# 6. SAIDA

conteudo <- list(
  meta = list(
    fonte = sprintf("PNAD Continua %d, entrevista %d (IBGE)", ano, entrevista),
    pacote = sprintf("PNADcIBGE %s",
                     as.character(utils::packageVersion("PNADcIBGE"))),
    universo = sprintf("populacao de %d anos ou mais", idade_minima),
    salario_minimo_referencia = sm,
    escala = paste(
      "contagem populacional (svytable com pesos PNADc). TABELA CONJUNTA:",
      "qualquer cruzamento, inclusive as marginais, e obtido somando as celulas.",
      "So casos completos nas cinco variaveis entram, para que toda margem",
      "derivada some o mesmo total."
    ),
    variaveis = as.list(names(niveis)),
    n_celulas = nrow(conjunta),
    celulas_zeradas = zeradas,
    n_pop = n_pop,
    n_amostra_pnadc = nrow(completos),
    missing_antes_do_corte = as.list(round(missing_pct, 6)),
    gerado_por = "scripts/gerar-margens-pnadc.R",
    gerado_em = format(Sys.Date(), "%Y-%m-%d")
  ),

  niveis = map(niveis, as.list),

  conjunta = pmap(
    conjunta,
    function(...) {
      linha <- list(...)
      c(map(linha[names(niveis)], as.character), list(freq = linha$freq))
    }
  )
)

dir.create("margens", showWarnings = FALSE)
write_yaml(conteudo, saida)

cat("\nescrito: ", saida, "\n", sep = "")
