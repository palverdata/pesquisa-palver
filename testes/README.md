# Testes de recrutamento

Experimentos metodológicos sobre **como a amostra é recrutada**, não ondas de
divulgação. Não recebem registro, não são publicados como pesquisa e não usam o
fluxo de `ondas/`.

Cada subpasta é uma rodada, nomeada pela data de encerramento do campo.

```text
2026-08-16/
├── dados/     microdados dos formulários (fora do git)
└── output/    agregados e diagnósticos (fora do git)
```

## Rodada 2026-08-16: criativo × rede

Desenho 2×2 — dois criativos ("Eleições" e "Redes") veiculados em duas
plataformas. Campo de 14 a 16 de agosto de 2026.

| arquivo | `survey_id` | completos |
| ------- | ----------- | --------- |
| `Exp Instagram Eleições bb8474a1.xlsx` | `bb8474a1` | 1.666 |
| `Exp Facebook Eleições 042855c9.xlsx`  | `042855c9` | 1.067 |
| `Exp Instagram Redes d395c025.xlsx`    | `d395c025` | 225 |
| `Exp Facebook Redes 012f1b84.xlsx`     | `012f1b84` | 169 |

O instrumento é o mesmo nos quatro, e **difere do da onda**: a idade vem
numérica (a onda usa faixas), há filtro de título de eleitor regularizado em
2022, pergunta de filiação partidária e a pergunta "neste exato momento você
está na mesma cidade que declarou?". Também traz `fingerprint_browser`,
`ip_hash` e `user_agent`, que a onda só tem no `paradata/`.

A harmonização para as variáveis de margem (`age_std`, `sex_std`, `edu_std`,
`inc_std`, `reg_std`, `vote_std`) reaproveita os mapas do `questionario.yaml` da
onda; só a idade precisa de tratamento próprio, cortada em 34 e 59.

## Três limitações medidas nesta rodada

**A rede do nome é o alvo, não a entrega.** `Exp Instagram Eleições` tem 306
casos entregues no Facebook, de 2.699. Para comparar plataformas, use a coluna
`rede_social`, não o nome do arquivo.

**Há respondentes repetidos entre formulários.** O gate por
`fingerprint_browser` barra reentrada no mesmo formulário, mas não entre
formulários diferentes. Pela matriz de co-ocorrência, 50 dos 225 completos de
`d395c025` (22%) também estão em `bb8474a1` — que é justamente a comparação
entre criativos no Instagram. O `ip_hash` concorda nesse par (51), e superestima
quando o par envolve a onda, por CGNAT e domicílio compartilhado.

**As células pequenas não sustentam leitura.** Com 169 e 225 completos, após
calibração sobram 25 a 36 entrevistas efetivas e a margem de erro passa de
±16 pp. Só a célula grande e os agregados dão números utilizáveis.

## Relação com `ondas/`

Os quatro formulários compartilham 46 respondentes com a onda de 10/08, que foi
a campo antes (03 a 07/08). A onda publicada não tem duplicata interna: 5.210
dos seus 5.256 completos são exclusivos dela.
