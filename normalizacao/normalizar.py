"""Normaliza as colunas de resposta aberta de uma onda.

    python normalizacao/normalizar.py <onda> <entrada.xlsx> <saida.xlsx>
"""

from __future__ import annotations

import csv
import json
import sys
from collections import Counter
from pathlib import Path
from typing import Callable

import pandas as pd
import yaml


def carregar_prompt(caminho: Path) -> dict:
    return json.loads(caminho.read_text(encoding="utf-8"))


def montar_prompt(prompt: dict, texto: str) -> str:
    partes = [prompt["instrucao"], ""]
    partes.append("Regras:")
    partes += [f"{i}. {r}" for i, r in enumerate(prompt["regras"], 1)]
    partes.append("")
    partes.append("Exemplos:")
    for e in prompt["exemplos"]:
        partes += [f"Entrada: {e['entrada']}", f"Saída: {e['saida']}", ""]
    # texto numa linha so: quebra de linha na entrada viraria fim da resposta
    partes += [f"Entrada: {' '.join(texto.split())}", "Saída:"]
    return "\n".join(partes)


def classificar_com(modelo: Path, temperatura: float) -> Callable[[dict, str], str]:
    from llama_cpp import Llama

    if not modelo.exists():
        sys.exit(f"modelo nao encontrado: {modelo}")
    llama = Llama(model_path=str(modelo), n_ctx=4096, n_gpu_layers=-1, verbose=False)

    def classificar(prompt: dict, texto: str) -> str:
        saida = llama.create_completion(
            montar_prompt(prompt, texto),
            max_tokens=48,
            temperature=temperatura,
            stop=["\n"],
        )
        return saida["choices"][0]["text"].strip()

    return classificar


def escrever_mapping(mapa: dict[str, str], freq: Counter, caminho: Path) -> None:
    caminho.parent.mkdir(parents=True, exist_ok=True)
    with caminho.open("w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["bruto", "freq", "rotulo"])
        for texto, n in freq.most_common():
            w.writerow([texto, n, mapa[texto]])


def normalizar(
    df: pd.DataFrame,
    colunas: list[dict],
    classificar: Callable[[dict, str], str],
    pasta_mapping: Path,
) -> pd.DataFrame:
    df = df.copy()
    for c in colunas:
        coluna, nova, prompt, vazio = c["coluna"], c["nova"], c["prompt"], c.get("vazio")
        if coluna not in df.columns:
            raise KeyError(f"coluna ausente na planilha: {coluna!r}")

        freq = Counter(v.strip() for v in df[coluna] if isinstance(v, str) and v.strip())

        mapa: dict[str, str] = {}
        for texto in freq:
            rotulo = classificar(prompt, texto)
            if not rotulo:
                raise RuntimeError(f"{nova}: o modelo nao devolveu rotulo para {texto!r}")
            mapa[texto] = rotulo

        escrever_mapping(mapa, freq, pasta_mapping / f"{nova}.csv")
        df[coluna] = [mapa[v.strip()] if isinstance(v, str) and v.strip() else vazio for v in df[coluna]]
        df = df.rename(columns={coluna: nova})
        print(f"{nova}: {len(freq)} grafias -> {len(set(mapa.values()))} rotulos")
    return df


def main(argv: list[str]) -> int:
    sys.stdout.reconfigure(encoding="utf-8", errors="replace")
    if len(argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    onda, entrada, saida = argv[0], Path(argv[1]), Path(argv[2])
    if entrada.resolve() == saida.resolve():
        sys.exit("saida igual a entrada: nao sobrescrevo o bruto")

    pasta = Path(__file__).resolve().parent.parent / "ondas" / onda / "norm"
    cfg = yaml.safe_load((pasta / "normalizacao.yaml").read_text(encoding="utf-8"))
    colunas = [
        {"coluna": c["coluna"], "nova": c["nova"], "vazio": c.get("vazio"),
         "prompt": carregar_prompt(pasta / "prompts" / c["prompt"])}
        for c in cfg["colunas"]
    ]

    df = pd.read_excel(entrada, dtype=str, keep_default_na=False, na_values=[""])
    classificar = classificar_com(Path(cfg["modelo"]).expanduser(), float(cfg["temperatura"]))
    df = normalizar(df, colunas, classificar, pasta / "mapping")

    saida.parent.mkdir(parents=True, exist_ok=True)
    df.to_excel(saida, index=False)
    print(f"escrito: {saida}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
