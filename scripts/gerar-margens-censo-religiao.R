# Gera as margens de religiao x regiao x sexo do Censo 2022, na escala da
# PNADc. O percentual de cada religiao dentro da regiao e do sexo vem do Censo;
# a populacao da celula vem da PNADc, como nas margens do TSE. Rode depois de
# gerar-margens-pnadc.R para cada base.
#
# Insumo: tabela 9537 do SIDRA (pessoas de 10 anos ou mais por religiao, sexo e
# grupos de idade, Brasil e UF), baixada em xlsx para insumos/ibge/.

arq_censo <- "insumos/ibge/tabela9537.xlsx"

# cada base gera a sua margem, com a base no nome do arquivo: misturar bases no
# raking reprova a conferencia de reg_std e sex_std
bases_pnadc <- list(c(ano = 2024, visita = 5), c(ano = 2025, visita = 1))

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

source("R/onda.R")
suppressPackageStartupMessages(library(yaml))

regiao_de <- c(
  "Rondônia" = "N", "Acre" = "N", "Amazonas" = "N", "Roraima" = "N",
  "Pará" = "N", "Amapá" = "N", "Tocantins" = "N",
  "Maranhão" = "NE", "Piauí" = "NE", "Ceará" = "NE",
  "Rio Grande do Norte" = "NE", "Paraíba" = "NE", "Pernambuco" = "NE",
  "Alagoas" = "NE", "Sergipe" = "NE", "Bahia" = "NE",
  "Minas Gerais" = "SE", "Espírito Santo" = "SE", "Rio de Janeiro" = "SE",
  "São Paulo" = "SE",
  "Paraná" = "S", "Santa Catarina" = "S", "Rio Grande do Sul" = "S",
  "Mato Grosso do Sul" = "CO", "Mato Grosso" = "CO", "Goiás" = "CO",
  "Distrito Federal" = "CO"
)
niveis_regiao <- c("N", "NE", "CO", "SE", "S")
niveis_sexo <- c("Homem", "Mulher")

# Mesmo recorte de relig_std. Nao sabe e sem declaracao vao para Outra, como o
# "Nao sei" da pesquisa, que cai no resto da derivada.
mapa_religiao <- c(
  "Católica Apostólica Romana" = "Católica",
  "Evangélicas" = "Evangélica/Protestante",
  "Sem religião" = "Sem Religião/Ateu/Agnóstico",
  "Espírita" = "Outra",
  "Umbanda e Candomblé" = "Outra",
  "Tradições indígenas" = "Outra",
  "Outras religiosidades" = "Outra",
  "Não sabe" = "Outra",
  "Sem declaração" = "Outra"
)
niveis_religiao <- c("Católica", "Evangélica/Protestante",
                     "Sem Religião/Ateu/Agnóstico", "Outra")

# 1. CENSO: o cabecalho tem tres linhas (sexo, idade, religiao) mescladas para a
# direita; cada coluna de dado e uma combinacao das tres

bruto <- readxl::read_excel(arq_censo, sheet = "Tabela", col_names = FALSE,
                            col_types = "text", .name_repair = "minimal")
names(bruto) <- paste0("c", seq_len(ncol(bruto)))

preencher <- function(x) {
  for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]
  x
}
cabecalho <- tibble(
  coluna = names(bruto),
  sexo = preencher(unlist(bruto[5, ])),
  idade = preencher(unlist(bruto[6, ])),
  religiao = unlist(bruto[7, ])
)

censo <- bruto %>%
  rename(uf = c1) %>%
  filter(uf %in% names(regiao_de)) %>%
  pivot_longer(-uf, names_to = "coluna", values_to = "pessoas") %>%
  left_join(cabecalho, by = "coluna") %>%
  filter(sexo %in% c("Homens", "Mulheres"), idade != "Total",
         religiao %in% names(mapa_religiao)) %>%
  # "-" e zero absoluto na legenda do IBGE
  mutate(pessoas = suppressWarnings(as.numeric(if_else(pessoas == "-", "0", pessoas))))

if (anyNA(censo$pessoas)) stop("celula nao numerica na tabela do Censo")

faltando_uf <- setdiff(names(regiao_de), unique(censo$uf))
if (length(faltando_uf) > 0) stop("UF ausente na tabela: ", paste(faltando_uf, collapse = ", "))

# Universo de 16 anos ou mais: sai 10 a 14 e 1/5 de 15 a 19, supondo as idades
# distribuidas por igual dentro da faixa.
fator_idade <- c("10 a 14 anos" = 0, "15 a 19 anos" = 0.8)

proporcao <- censo %>%
  mutate(peso_idade = coalesce(unname(fator_idade[idade]), 1),
         reg_std = unname(regiao_de[uf]),
         sex_std = if_else(sexo == "Homens", "Homem", "Mulher"),
         relig_std = unname(mapa_religiao[religiao])) %>%
  group_by(reg_std, sex_std, relig_std) %>%
  summarise(pessoas = sum(pessoas * peso_idade), .groups = "drop") %>%
  group_by(reg_std, sex_std) %>%
  mutate(pct = pessoas / sum(pessoas)) %>%
  ungroup()

stopifnot(nrow(proporcao) == length(niveis_regiao) * length(niveis_sexo) *
            length(niveis_religiao))

# 2. MARGEM POR BASE DA PNADc

for (base in bases_pnadc) {

  margens_pnadc <- sprintf("margens/pnadc-%d-visita%d.yaml",
                           base[["ano"]], base[["visita"]])
  sufixo <- sprintf("pnadc%dv%d", base[["ano"]], base[["visita"]])

  pop <- carregar_margens(margens_pnadc)$tabela %>%
    group_by(reg_std = as.character(reg_std), sex_std = as.character(sex_std)) %>%
    summarise(pop = sum(freq), .groups = "drop")

  margem <- proporcao %>%
    left_join(pop, by = c("reg_std", "sex_std")) %>%
    group_by(reg_std, sex_std) %>%
    mutate(freq = arredondar_preservando_total(pct * first(pop))) %>%
    ungroup() %>%
    arrange(match(reg_std, niveis_regiao), match(sex_std, niveis_sexo),
            match(relig_std, niveis_religiao))

  conferencia <- margem %>% group_by(reg_std, sex_std) %>%
    summarise(f = sum(freq), p = first(pop), .groups = "drop")
  if (any(conferencia$f != conferencia$p)) {
    stop("regiao x sexo nao bate com ", margens_pnadc)
  }

  saida <- sprintf("margens/censo-2022-religiao-%s.yaml", sufixo)
  write_yaml(list(
    meta = list(
      fonte = "IBGE, Censo Demografico 2022, tabela 9537 (SIDRA)",
      arquivo = arq_censo,
      nota_ibge = "Resultados preliminares da amostra, estimados a partir de areas de ponderacao preliminares.",
      definicao = paste(
        "relig_std: Catolica Apostolica Romana -> Catolica; Evangelicas ->",
        "Evangelica/Protestante; Sem religiao -> Sem Religiao/Ateu/Agnostico;",
        "Espirita, Umbanda e Candomble, Tradicoes indigenas, Outras",
        "religiosidades, Nao sabe e Sem declaracao -> Outra."),
      universo = paste(
        "16 anos ou mais. A tabela comeca em 10 anos: sai 10 a 14 e 1/5 de 15",
        "a 19, supondo as idades distribuidas por igual dentro da faixa."),
      derivado_de = margens_pnadc,
      escala = paste0(
        "contagem POPULACIONAL. O percentual de cada religiao dentro de regiao ",
        "x sexo no Censo e aplicado a populacao da mesma celula em ",
        margens_pnadc, ", entao reg_std e sex_std tem a distribuicao da PNADc. ",
        "`pct` fica em cada celula para auditoria."),
      variaveis = list("reg_std", "sex_std", "relig_std"),
      n_celulas = nrow(margem),
      n_pop = as.integer(sum(margem$freq)),
      gerado_por = "scripts/gerar-margens-censo-religiao.R",
      gerado_em = format(Sys.Date(), "%Y-%m-%d")
    ),
    niveis = list(reg_std = as.list(niveis_regiao),
                  sex_std = as.list(niveis_sexo),
                  relig_std = as.list(niveis_religiao)),
    conjunta = purrr::pmap(margem, function(reg_std, sex_std, relig_std, pct,
                                            freq, ...) {
      list(reg_std = reg_std, sex_std = sex_std, relig_std = relig_std,
           freq = as.integer(freq), pct = round(pct, 6))
    })
  ), saida)
  cat("escrito:", saida, "(", nrow(margem), "celulas )\n")
}

cat("\n--- Brasil, % da populacao 16+ (Censo 2022) ---\n")
print(proporcao %>% group_by(relig_std) %>% summarise(p = sum(pessoas)) %>%
        mutate(pct = round(100 * p / sum(p), 2)) %>%
        arrange(match(relig_std, niveis_religiao)) %>% select(-p))
