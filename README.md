<img src="assets/logo-palver.png" alt="Palver" width="280">

# Pesquisa Palver

Este repositório apresenta os códigos de calibração das pesquisas de opinião da Palver por *raking* (IPF). Cada onda de campo é uma pasta em `ondas/` com dois arquivos declarativos; o código em `R/` é o mesmo para todas.

> **Onda nova = pasta nova + dois YAML, zero código novo.**

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

Saem quatro arquivos em `ondas/<onda>/output/`, que **não** vão para o git — são
gerados a partir do que está versionado:

| arquivo                     | conteúdo                                                |
| --------------------------- | -------------------------------------------------------- |
| `<prefixo>.xlsx`          | abas`Stratification`, `Results` e `ResultsStrat`   |
| `<prefixo>_N.xlsx`        | as mesmas abas com o N não ponderado de cada estimativa |
| `diagnostico-margens.csv` | margem ponderada contra a cota, célula por célula      |
| `ambiente.txt`            | versões, margens usadas e o veredito da onda            |

E `resultado` fica no ambiente do R para inspeção: `resultado$results`,
`resultado$base`, `resultado$fit$design`.

## Passo a passo: criar uma onda nova

1. Crie a pasta com o nome sendo a **data de divulgação** (`AAAA-MM-DD`), e
   copie os dois YAML da onda anterior como ponto de partida:

   ```r
   dir.create("ondas/2026-09-14/dados", recursive = TRUE)
   file.copy(c("ondas/2026-08-10/config.yaml",
               "ondas/2026-08-10/questionario.yaml"), "ondas/2026-09-14/")
   ```
2. No `config.yaml`, atualize `onda` (`nome`, `registro`, `data_divulgacao`),
   `campo` e `outputs.prefixo`. `registro` e `data_divulgacao` são
   **obrigatórios**, e `data_divulgacao` tem de ser igual ao nome da pasta.
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

As margens em `margens/` são compartilhadas por todas as ondas.  Só precisam ser regeradas quando a fonte muda (nova PNADc, nova eleição de referência).

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
│   └── resultados.R            #   estimativas, 3 abas, Excel, ambiente.txt
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
    ├── dados/                  #   o .xlsx bruto (fora do git)
    └── output/                 #   resultados e diagnósticos (fora do git)
```

Cada `margens/*.yaml` guarda a **tabela conjunta** das suas variáveis: qualquer
cruzamento pedido em `calibracao.margens` é obtido somando essa tabela, sem
regerar nada.

## Dependências

O código é carregado por `source()`; o repositório não é instalado como pacote.
O [DESCRIPTION](DESCRIPTION) é o manifesto. Versões usadas nos resultados
publicados: R 4.5.2, survey 4.5, dplyr 1.2.0, tidyr 1.3.2, purrr 1.2.1,
readxl 1.4.5, readr 2.2.0, stringr 1.6.0, tibble 3.3.1, yaml 2.3.12,
writexl 1.5.4, tidyverse 2.0.0, PNADcIBGE 0.7.5 (só para as margens).

Cada execução grava as versões exatas em `output/ambiente.txt`, amarrando todo
número publicado ao ambiente que o produziu.

## Dados que não entram no git

Nenhuma base individual — com ou sem peso, identificada ou não. Barreiras no
[.gitignore](.gitignore): `ondas/*/dados/*`, `insumos/**` e `*.sav` `*.dta`
`*.rds` em qualquer lugar.

Os resultados em `ondas/*/output/` também ficam fora do git: são gerados pelo
motor a partir do que está versionado. O que se versiona é a **especificação** —
os dois YAML da onda e as margens derivadas — de onde qualquer resultado pode ser
reproduzido.

Tag por onda

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

Inteligência artificial foi utilizada para revisar códigos, correção ortográfica e redação de documentação neste repositório. Toda a a estrutura de códigos e resultados foi amplamente revisada e replicada pela equipe da Palver.
