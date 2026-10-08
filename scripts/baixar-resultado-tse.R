# Baixa o resultado de presidente da divulgacao do TSE (Brasil, UFs e exterior)
# para insumos/tse/ e converte em CSV por UF. Rode depois da totalizacao; antes
# disso o arquivo muda a cada boletim.
#
# Os dados abertos (votacao_candidato_munzona_<ano>) so saem depois da
# totalizacao final; ate la a fonte e a divulgacao em resultados.tse.jus.br.

ciclo <- "ele2026"
turno <- 1

# ==============================================================================

while (!file.exists("R/onda.R") && dirname(getwd()) != getwd()) setwd("..")
if (!file.exists("R/onda.R")) {
  stop("abra o pesquisa-palver.Rproj antes de rodar, ou ajuste o diretorio ",
       "de trabalho para dentro do repositorio.", call. = FALSE)
}

suppressPackageStartupMessages(library(tidyverse))

base <- "https://resultados.tse.jus.br/oficial"
ufs <- c("ac", "al", "am", "ap", "ba", "ce", "df", "es", "go", "ma", "mg", "ms",
         "mt", "pa", "pb", "pe", "pi", "pr", "rj", "rn", "ro", "rr", "rs", "sc",
         "se", "sp", "to", "zz")

ler_json <- function(caminho) jsonlite::fromJSON(caminho, simplifyVector = FALSE)
numero <- function(x) as.numeric(sub(",", ".", x, fixed = TRUE))

# 1. CODIGO DA ELEICAO: muda a cada ciclo e turno, entao vem do ele-c.json

config <- ler_json(paste0(base, "/comum/config/ele-c.json"))
pleito <- purrr::detect(config$pl, ~ .x$c == ciclo)
if (is.null(pleito)) stop("ciclo ausente no ele-c.json: ", ciclo)

eleicao <- purrr::detect(pleito$e, function(e) {
  cargos <- unlist(purrr::map(e$abr, ~ purrr::map_chr(.x$cp, "ds")))
  e$t == as.character(turno) && "Presidente" %in% cargos
})
if (is.null(eleicao)) stop("sem eleicao de presidente no ", turno, "o turno de ", ciclo)

cod <- sprintf("%06d", as.integer(eleicao$cd))
saida <- file.path("insumos", "tse",
                   sprintf("presidente_%s_t%d", sub("ele", "", ciclo), turno))
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("%s (codigo %s)\n", eleicao$nm, eleicao$cd))

# 2. DOWNLOAD: o JSON fica como o TSE publicou; a conversao vem depois

# em 2026 o resultado esta em dados/<abr>/...-u.json; o -r.json de
# dados-simplificados dos ciclos anteriores nao existe
arquivos <- purrr::map_chr(c("br", ufs), function(abr) {
  destino <- file.path(saida, sprintf("%s-c0001-e%s-u.json", abr, cod))
  url <- sprintf("%s/%s/%s/dados/%s/%s-c0001-e%s-u.json",
                 base, ciclo, eleicao$cd, abr, abr, cod)
  utils::download.file(url, destino, mode = "wb", quiet = TRUE)
  Sys.sleep(0.2)
  destino
})

# 3. CONVERSAO

resumo <- function(arq) {
  d <- ler_json(arq)
  cand <- purrr::map_dfr(d$carg[[1]]$agr, function(agr) {
    purrr::map_dfr(agr$par, function(par) {
      purrr::map_dfr(par$cand, ~ tibble(numero = .x$n, candidato = .x$nmu,
                                        partido = par$sg,
                                        votos = numero(.x$vap)))
    })
  })
  tibble(abrangencia = toupper(d$cdabr), gerado = paste(d$dg, d$hg),
         totalizacao_final = d$tf, secoes_totalizadas_pct = numero(d$s$pst),
         aptos = numero(d$e$te), comparecimento = numero(d$e$c),
         abstencao = numero(d$e$a), brancos = numero(d$v$vb),
         nulos = numero(d$v$tvn), validos = numero(d$v$vv), cand = list(cand))
}

tudo <- purrr::map_dfr(arquivos, resumo)

totais <- tudo %>% select(-cand)
votos <- tudo %>% select(abrangencia, cand) %>% unnest(cand) %>%
  arrange(abrangencia, desc(votos))

# 4. CONFERENCIA

if (any(totais$secoes_totalizadas_pct < 100)) {
  stop("totalizacao incompleta: ",
       paste(totais$abrangencia[totais$secoes_totalizadas_pct < 100], collapse = ", "))
}

colunas <- c("aptos", "comparecimento", "abstencao", "brancos", "nulos", "validos")
br <- totais %>% filter(abrangencia == "BR")
soma <- totais %>% filter(abrangencia != "BR") %>% summarise(across(all_of(colunas), sum))
if (any(unlist(soma) != unlist(br[colunas]))) {
  stop("UFs mais exterior nao somam o total do Brasil")
}

soma_cand <- votos %>% filter(abrangencia != "BR") %>% group_by(numero) %>%
  summarise(votos = sum(votos), .groups = "drop")
if (!isTRUE(all.equal(soma_cand$votos[order(soma_cand$numero)],
                      with(filter(votos, abrangencia == "BR"), votos[order(numero)])))) {
  stop("votos por candidato nas UFs nao somam o do Brasil")
}

if (any(totais$totalizacao_final != "s")) {
  message("aviso: o TSE ainda nao marcou a totalizacao como final (tf = n)")
}

readr::write_csv(totais, file.path(saida, "totais_por_uf.csv"))
readr::write_csv(votos, file.path(saida, "votos_por_uf.csv"))

cat(sprintf("\nBrasil, gerado em %s: aptos %s | comparecimento %.2f%% | abstencao %.2f%%\n",
            br$gerado, format(br$aptos, big.mark = ".", decimal.mark = ","),
            100 * br$comparecimento / br$aptos, 100 * br$abstencao / br$aptos))
print(votos %>% filter(abrangencia == "BR") %>%
        mutate(pct_validos = round(100 * votos / br$validos, 2)) %>%
        select(candidato, partido, votos, pct_validos), n = Inf)
cat("\nescrito em ", saida, "/: ", length(arquivos), " JSON, totais_por_uf.csv, ",
    "votos_por_uf.csv\n", sep = "")
