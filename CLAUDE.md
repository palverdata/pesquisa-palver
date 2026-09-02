# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R engine that calibrates Palver's political opinion polls by raking (iterative
proportional fitting / IPF) against population targets from PNADc and TSE. One engine
in `R/`, many waves in `ondas/`, each wave defined entirely by two YAML files — no new
code per wave. Not an installed R package: code is loaded via `source()`, and
`DESCRIPTION` is only a dependency manifest, not a build target.

## Running a wave

```r
# from R/RStudio, at the repo root (where DESCRIPTION lives):
onda <- "2026-08-10"
source("R/onda.R")
resultado <- rodar_onda(onda)
```

Or open [scripts/rodar-onda.R](scripts/rodar-onda.R), set the `onda` variable at the
top, and Source it — it auto-detects the repo root by walking up directories.
`resultado` stays in the environment for inspection: `resultado$results`,
`resultado$base`, `resultado$fit$design`.

Required packages: `tidyverse`, `survey`, `yaml`, `writexl` (see `DESCRIPTION` for the
full/versioned list; `PNADcIBGE` only needed for regenerating margins).

There is no test suite, linter, or CI config in this repo — correctness is verified by
the engine's own runtime checks (below) and by reading `output/ambiente.txt` after a run.

## Architecture

Everything a wave needs is derived from its folder name (`ondas/<AAAA-MM-DD>/`):

```
R/
├── onda.R          # orchestration: carregar_config() -> rodar_onda()
├── calibracao.R    # questionnaire+data loading, margins, raking, trimming, diagnostics
└── resultados.R    # calibrated design -> estimates -> the 5 output files
```

`rodar_onda()` in [R/onda.R](R/onda.R) is the pipeline, straight-line:
1. `carregar_config()` — reads `config.yaml`, validates the `onda` block, resolves the
   single `.xlsx` in `dados/`.
2. `carregar_questionario()` — reads `questionario.yaml`, expands harmonization blocks.
3. `ler_bruto()` + `montar_respondentes()` — raw xlsx → typed/factored respondent base;
   applies `derivadas` (derived variables).
4. `cortar_amostra()` — optional chronological cut to a registered sample size.
5. `carregar_margens()` + `montar_alvos()` — loads shared population targets from
   `margens/`, builds the joint tables the calibration formulas need.
6. `rake_weights()` — `survey::calibrate()` (raking) against those targets.
7. `trim_weights()` — optional weight trimming.
8. `exportar_onda()` in [R/resultados.R](R/resultados.R) — estimates every question
   (overall and by stratification variable), writes the output files, and grades the
   wave against its own declared criteria in `ambiente.txt`.

### The two YAMLs per wave

- **`config.yaml`** — parameters: `onda` (name/registro/data_divulgacao, must equal the
  folder name), `campo` dates, `amostra` cut, which `margens/*.yaml` files to use,
  `calibracao.margens` (list of variable groups to rake on, e.g. `[sex_std, age_std]`),
  `trimming`, `diagnosticos` (tolerances, confidence level, max margin of error),
  `outputs.prefixo`.
- **`questionario.yaml`** — the instrument: `meta`, `colunas` (fixed metadata columns),
  `matrizes` (grid/matrix questions), `questoes` / `questoes_nao_exportadas`,
  `derivadas` (variables computed from others — types: `mapa`, `mapa_com_resto`,
  `agrupamento`, `voto_pregresso`, `limiar`), `harmonizacao` (collapses candidate-level
  variables into cross-wave-comparable `_h` variants), `estratificacao` (crosstab
  breakdowns), `exportar_ordem`.

**The one rule that matters**: every `texto` in `questoes` must reproduce the raw
`.xlsx` column header character-for-character, and every `niveis` entry must reproduce
the cell value character-for-character. The engine matches by exact text, not fuzzy
matching — any mismatch is a bug in `questionario.yaml`, never in the data. A column in
the file that isn't declared is silently ignored (with a console message); a declared
item missing from the file is a hard error.

### Margins (`margens/`)

Shared population targets, independent of any single wave. Each `margens/*.yaml`
stores the **joint table** for its variables (`conjunta`) — any marginal or crossing
requested by `calibracao.margens` is obtained by summing that table, never
regenerated. Regenerate only when the source changes:
- [scripts/gerar-margens-pnadc.R](scripts/gerar-margens-pnadc.R) — downloads from
  IBGE's FTP.
- [scripts/gerar-margens-tse.R](scripts/gerar-margens-tse.R) — reads local CSVs from
  `insumos/tse/` (downloaded manually from TSE's open data portal).

**Order matters: regenerate TSE after PNADc.** The TSE margin's regional population
comes from PNADc's regional population distributed by TSE's vote share within that
region — this is what keeps `reg_std` consistent across both sources so a wave can mix
variables from both without `conferir_marginais()` rejecting them for disagreeing.

### Validation is load-bearing, not optional

The engine is designed to fail loudly rather than silently produce a wrong number.
Key checks to be aware of when editing `R/calibracao.R`:
- `conferir_marginais()` — rejects margins that imply different population
  distributions for the same variable (e.g. mixing PNADc "resident population" and
  TSE "turnout" for the same field without deliberately choosing one source).
- `conferir_celulas()` — rejects a calibration cell that has population but zero
  respondents (cross too fine for this wave's n) or respondents but zero population.
- `validar_alvos()` — rejects margins whose totals don't agree with each other.
- `ambiente.txt` — every run's verdict ("ATENDE"/"NAO ATENDE") against the wave's own
  declared `diagnosticos.tolerancia_pp` and `moe_maxima_pp`.

## Data policy — never commit microdata

No individual-level survey data, weighted or not, identified or not, goes into git.
Enforced by `.gitignore`: `ondas/*/dados/*`, `insumos/**`, and `*.sav`/`*.dta`/`*.rds`
anywhere. `ondas/*/output/*` is also gitignored — outputs are always reproducible from
what *is* versioned (the engine + the two YAMLs + `margens/`). When adding new
generated-output types, gitignore them the same way rather than committing them.

## Creating a new wave

Copy the previous wave's two YAMLs as a starting point, update `onda.nome`,
`onda.registro`, `onda.data_divulgacao` (must equal the new folder name), `campo`, and
`outputs.prefixo`, then rewrite `questionario.yaml` against the new raw `.xlsx` file
per the character-for-character rule above. Full walkthrough is in
[README.md](README.md).

Each released wave gets a git tag freezing engine + margins + config together:
`git tag -a v<data_divulgacao> -m "<registro> -- divulgacao <data>"`.
