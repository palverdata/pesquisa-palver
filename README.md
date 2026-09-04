<img src="assets/logo-palver.png" alt="Palver" width="280">

# Pesquisa Palver

Este repositório apresenta os códigos de calibração das pesquisas de opinião da Palver por *raking* (IPF). Cada onda de campo é uma pasta em `ondas/` com três arquivos declarativos; o código em `R/` é o mesmo para todas.

> **Onda nova = pasta nova + três YAML, zero código novo.**

## Divulgações

Relatórios completos e press releases de todas as ondas ficam em
**[palver.com.br/survey](https://www.palver.com.br/survey)**. Este repositório
guarda apenas o motor de calibração e a especificação de cada onda.

| onda | divulgação | registro | campo | relatório | press release |
| ---- | ---------- | -------- | ----- | --------- | ------------- |
| 1 | 10/08/2026 | BR-06596/2026 | 03 a 09/08/2026 | [PDF, 93 páginas](https://www.palver.com.br/api/surveys/voting-intention-2026-august/report) | [PDF, 2 páginas](https://www.palver.com.br/api/surveys/voting-intention-2026-august/press-release) |

Cada onda tem uma tag git — [`v2026-08-10`](../../releases/tag/v2026-08-10) —
que congela o motor, as margens e a configuração usados para produzir aqueles
números.

## Passo a passo: rodar uma onda

1. Abra **`pesquisa-palver.Rproj`** no RStudio.
2. Instale os pacotes, se for a primeira vez:

   ```r
   install.packages(c("tidyverse", "survey", "yaml", "writexl"))
   ```
3. Coloque o export dos dados brutos — um único `.xlsx` — em
   `ondas/<onda>/dados/`. O nome não importa; o motor acha o único `.xlsx` da
   pasta. Esse arquivo **não** vai para o git.
4. Abra [scripts/rodar-onda.R](scripts/rodar-onda.R), escreva o nome da onda e
   clique em **Source** (Ctrl+Shift+S):

   ```r
   onda <- "2026-08-10"
   ```
5. Leia `ondas/<onda>/output/ambiente.txt`. A última linha diz se a onda atende
   os critérios declarados no `config.yaml` (desvio máximo contra as cotas e
   margem de erro). Se não atender, a execução emite aviso.

Saem cinco arquivos em `ondas/<onda>/output/`, que **não** vão para o git — são
gerados a partir do que está versionado:

| arquivo                       | conteúdo                                                  |
| ----------------------------- | --------------------------------------------------------- |
| `<id>_pesquisa_<AAAA>_<MM>_<DD>.json` | os cruzamentos prontos: a fonte única dos gráficos e da plataforma |
| `<prefixo>_localidades.xlsx` | entrevistas por município                                |
| `<prefixo>_microdados.xlsx`  | uma linha por respondente, com o peso calibrado           |
| `diagnostico-margens.csv`   | margem ponderada contra a cota, célula por célula        |
| `ambiente.txt`              | versões, margens usadas e o veredito da onda              |

O JSON é o único que carrega estimativa, com precisão cheia.

E `resultado` fica no ambiente do R para inspeção: `resultado$crosstabs`,
`resultado$sample`, `resultado$base`, `resultado$fit$design`.

### Os microdados ponderados

`<prefixo>_microdados.xlsx` tem duas abas: `Microdados`, uma linha por
respondente calibrado com todas as colunas declaradas do arquivo bruto mais
`peso` (soma = população dos alvos) e `peso_norm` (peso ÷ média, soma = *n*); e
`Dicionario`, que diz de cada coluna o tipo, o enunciado e os níveis possíveis.

Este é o único arquivo de saída que contém base individual. Ele fica em
`output/`, coberto pelo [.gitignore](.gitignore), e **não** é publicado — ver
[Dados que não entram no git](#dados-que-não-entram-no-git). Para desligar o
export, ponha `outputs.microdados: false` no `config.yaml` da onda.

Atenção ao usá-lo: reponderar com `svydesign(ids = ~1, weights = ~peso)` **não**
reproduz os intervalos de confiança das planilhas. Os ICs publicados vêm de
`survey::calibrate()`, que desconta a variância explicada pelas margens; um
desenho que só recebe o peso trata-o como fixo e devolve erro-padrão maior. Para
reproduzir, refaça a calibração — `margens/*.yaml` e `calibracao.margens` estão
versionados exatamente para isso. Já a margem de erro do `ambiente.txt` é outra
conta ainda: pior caso `p = 0,5` sobre o *n* efetivo de Kish, deliberadamente
conservadora, usada para o registro da pesquisa.

### O JSON da onda

`<id>_pesquisa_<AAAA>_<MM>_<DD>.json` traz os cruzamentos prontos. É a fonte de
dois consumidores: a plataforma de exibição e o repositório que monta os
gráficos do relatório. Nenhum dos dois lê microdado nem calcula nada; os dois
buscam por chave (`pergunta|recorte`, com recorte vazio para o total) e desenham.

`share`, `low` e `high` vêm do **mesmo desenho calibrado** que produz o resto da
onda. Não há segunda fórmula de intervalo, e é por isso que a plataforma e o
relatório não mostram números diferentes do mesmo resultado.

O que entra na tela é declarado em [display.yaml](ondas/2026-08-10/display.yaml).
As chaves:

| chave | onde | o que faz |
| ----- | ---- | --------- |
| `secoes` | topo | as seções, e dentro de cada uma as questões, na ordem de exibição |
| `recortes` | topo | os recortes que o menu de cruzamento oferece |
| `amostra` | topo | as variáveis do resumo que abre a divulgação; sai como `sample` |
| `cores` | topo | rótulo → hex; sai como `colors` |
| `rotulo` | questão, recorte | o nome curto na tela |
| `harmonizar` | questão, recorte | agrupa níveis antes de estimar (`mapa`, e `resto` opcional) |
| `ordenar` | questão | `decrescente` ordena por pontuação; `declarado` é o default |
| `fixar_no_fim` | questão | respostas que saem da ordenação e vão para o fim |
| `respostas` | questão | ordem explícita; não convive com `ordenar` |
| `grupos` | recorte | ordem explícita das categorias |
| `base` | questão | restringe a quem respondeu certo valor noutra variável |
| `excluir` | recorte | tira grupos da tela e da base |

O enunciado não se repete no `display.yaml` — vem do `questionario.yaml`, do
`titulo` quando existe e do `texto` quando não. Onda sem `display.yaml` roda
igual, só não gera o JSON.

Quatro coisas que a tabela não diz:

**Harmonizar é antes de estimar, não depois.** `share` e `n` somam, mas intervalo
não soma: o IC de uma categoria agrupada não é a soma dos ICs das partes. O motor
recodifica a variável e refaz a estimativa.

**Cada variável harmonizada recebe uma coluna de trabalho**, com prefixo distinto
para questão e para recorte. A mesma variável pode entrar como as duas coisas,
com harmonizações diferentes. A coluna original nunca é tocada, porque ela pode
ser margem de calibração.

**O bloco `amostra` é conferido contra `calibracao.margens`.** O motor interrompe
a onda se a lista declarada divergir do conjunto de margens, nos dois sentidos.
Cada célula tem `share` e `n`, sem intervalo: margem de calibração tem intervalo
de largura zero por construção.

**O que o motor recusa.** Hex fora de `#RRGGBB`; `ordenar` junto de `respostas`;
ordem declarada que omita valor observado; `harmonizar` citando nível que a
variável não tem; grupo ou resposta inexistente em `excluir` e `fixar_no_fim`.
Cor sem rótulo correspondente só emite aviso.

Publicar é copiar o arquivo para o repositório da plataforma **depois** da
divulgação: ele tem resultado com data de divulgação.
## Passo a passo: criar uma onda nova

1. Crie a pasta com o nome sendo a **data de divulgação** (`AAAA-MM-DD`), e
   copie os três YAML da onda anterior como ponto de partida:

   ```r
   dir.create("ondas/2026-09-14/dados", recursive = TRUE)
   file.copy(c("ondas/2026-08-10/config.yaml",
               "ondas/2026-08-10/questionario.yaml",
               "ondas/2026-08-10/display.yaml"), "ondas/2026-09-14/")
   ```
2. No `config.yaml`, atualize `onda` (`nome`, `registro`, `sequencia`,
   `data_divulgacao`), `campo` e `outputs.prefixo`. `registro`,
   `sequencia` e `data_divulgacao` são **obrigatórios**, e `data_divulgacao`
   tem de ser igual ao nome da pasta.
3. Refaça o `questionario.yaml` a partir do arquivo bruto desta onda. A regra é
   uma só e não tem exceção:

   > Cada `texto` é o cabeçalho da coluna no `.xlsx`, caractere por caractere.
   > Cada nível em `niveis` é o valor da célula, caractere por caractere.
   >

   É por esse texto que o motor descobre qual coluna é qual variável. Se houver
   divergência, o problema é do `questionario.yaml`, nunca do dado.
4. Rode como acima. O motor **interrompe** diante de item declarado que não
   existe no arquivo, resposta fora dos níveis, célula de margem sem
   respondente, ou margens com totais inconsistentes. Coluna do arquivo que não
   está declarada é apenas ignorada, com aviso no console.

## Passo a passo: gerar as margens

As margens em `margens/` são compartilhadas por todas as ondas. Só precisam ser regeradas quando a fonte muda (nova PNADc, nova eleição de referência).

**PNADc** — [scripts/gerar-margens-pnadc.R](scripts/gerar-margens-pnadc.R) baixa
sozinho do FTP do IBGE (~172 MB). Ajuste `ano`, `entrevista` e `sm` no topo e
clique em Source. Precisa de `install.packages("PNADcIBGE")`.

**TSE** — baixe os dois arquivos do portal de dados abertos, descompacte e ponha
os `.csv` em `insumos/tse/`:

- [votacao_candidato_munzona_2022.zip](https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2022.zip)
  → `votacao_candidato_munzona_2022_BRASIL.csv` (~4 GB descompactado, lido em blocos)
- [detalhe_votacao_munzona_2022.zip](https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/detalhe_votacao_munzona_2022.zip)
  → `detalhe_votacao_munzona_2022_BR.csv`

Origem: [dadosabertos.tse.jus.br](https://dadosabertos.tse.jus.br/dataset/resultados-2022)
(Resultados, 2022). Depois rode
[scripts/gerar-margens-tse.R](scripts/gerar-margens-tse.R).

**Ordem importa: TSE depois da PNADc.** A população de cada célula do TSE é a
população regional da PNADc distribuída pelo percentual de voto do TSE dentro
daquela região. É isso que faz `reg_std` ter a mesma distribuição nas duas
fontes, permitindo usar região nas duas margens sem conflito.

## Estrutura

```text
pesquisa-palver/
├── R/                          # o motor, igual para todas as ondas
│   ├── onda.R                  #   fluxo; único source() dos scripts
│   ├── calibracao.R            #   xlsx -> base -> alvos -> raking -> aparo
│   └── resultados.R            #   estimativas, o JSON, microdados, ambiente
├── scripts/                    # abra no RStudio, preencha o topo e Source
│   ├── rodar-onda.R
│   ├── gerar-margens-pnadc.R
│   └── gerar-margens-tse.R
├── margens/                    # alvos populacionais, compartilhados
│   ├── pnadc-2024-visita5.yaml
│   └── tse-2022-turno2.yaml
├── insumos/tse/                # microdados do TSE (fora do git)
└── ondas/2026-08-10/           # pasta = data de divulgação
    ├── config.yaml             #   margens, calibração, aparo, saída
    ├── questionario.yaml       #   enunciados, níveis, derivadas
    ├── display.yaml            #   seções, rótulos e ordens da divulgação
    ├── dados/                  #   o .xlsx bruto (fora do git)
    ├── paradata/               #   mesmo export com fingerprint e ip_hash (fora do git)
    └── output/                 #   resultados, microdados com peso (fora do git)
```

`paradata/` guarda o export enriquecido da **mesma** onda: os 5.256 completos são
exatamente os de `dados/`, mais os incompletos e as colunas de `fingerprint_browser`,
`ip_hash`, `user_agent` e alvo de anúncio. Fica fora de `dados/` de propósito — o
motor usa o único `.xlsx` daquela pasta, e o arquivo que produziu os números
publicados não deve mudar.

Cada `margens/*.yaml` guarda a **tabela conjunta** das suas variáveis: qualquer
cruzamento pedido em `calibracao.margens` é obtido somando essa tabela, sem
regerar nada.

## Dependências

O código é carregado por `source()`; o repositório não é instalado como pacote.
O [DESCRIPTION](DESCRIPTION) é o manifesto. Versões usadas nos resultados
publicados: R 4.5.2, survey 4.5, dplyr 1.2.0, tidyr 1.3.2, purrr 1.2.1,
readxl 1.4.5, readr 2.2.0, stringr 1.6.0, tibble 3.3.1, yaml 2.3.12,
writexl 1.5.4, jsonlite 2.0.0, tidyverse 2.0.0, PNADcIBGE 0.7.5 (só para as
margens).

Cada execução grava as versões exatas em `output/ambiente.txt`, amarrando todo
número publicado ao ambiente que o produziu.

## Dados que não entram no git

Nenhuma base individual — com ou sem peso, identificada ou não. Barreiras no
[.gitignore](.gitignore): `ondas/*/dados/*`, `ondas/*/output/*`,
`ondas/*/paradata/*`, `insumos/**` e `*.sav` `*.dta` `*.rds` em qualquer lugar.

O motor **gera** microdados com peso em `ondas/*/output/` — é o
`<prefixo>_microdados.xlsx` descrito acima. O arquivo nasce dentro do
`.gitignore`, e a regra segue a mesma: nenhuma base individual sai daqui.

Os resultados em `ondas/*/output/` ficam fora do git também por outro motivo: são
gerados pelo motor a partir do que está versionado. O que se versiona é a
**especificação** — os três YAML da onda e as margens derivadas — de onde
qualquer resultado pode ser reproduzido.

### Tag por onda

Cada onda divulgada recebe uma tag, congelando motor, margens e config usados:

```sh
git tag -a v2026-08-10 -m "BR-06596/2026 -- divulgacao 10/08/2026"
```

## Como citar

Ver [CITATION.cff](CITATION.cff). O GitHub o lê e oferece o texto pronto em APA
e BibTeX no botão *Cite this repository*, no topo da página do repositório.

## Licença

MIT — ver [LICENSE](LICENSE).

## Disclaimer de Inteligência Artificial

Inteligência artificial foi utilizada para revisar códigos, correção ortográfica e redação de documentação neste repositório. Toda a estrutura de códigos e resultados foi amplamente revisada e replicada pela equipe da Palver.
