from pathlib import Path

import pandas as pd
import pytest

from normalizar import carregar_prompt, montar_prompt, normalizar

PROMPT: dict = {}
COLUNAS = [{"coluna": "voto", "nova": "voto_norm", "prompt": PROMPT}]
MAPA = {"Lula": "Lula", "LULA": "Lula", "Lula 13": "Lula", "22": "Indeciso / Não Respondeu"}
LINHAS = ["Lula", "Lula", "LULA", "Lula 13", "22", None, "", "  "]


def gravado(chamadas):
    def classificar(prompt, texto):
        chamadas.append(texto)
        return MAPA.get(texto, "")
    return classificar


def fixo(prompt, texto):
    return MAPA.get(texto, "")


def test_texto_repetido_gera_uma_chamada(tmp_path):
    chamadas = []
    normalizar(pd.DataFrame({"voto": LINHAS}), COLUNAS, gravado(chamadas), tmp_path)
    assert sorted(chamadas) == ["22", "LULA", "Lula", "Lula 13"]


def test_mapping_aplicado_e_coluna_renomeada_no_lugar(tmp_path):
    df = pd.DataFrame({"id": range(8), "voto": LINHAS, "uf": ["SP"] * 8})
    fora = normalizar(df, COLUNAS, fixo, tmp_path)
    assert list(fora.columns) == ["id", "voto_norm", "uf"]
    assert list(fora["voto_norm"][:5]) == ["Lula"] * 4 + ["Indeciso / Não Respondeu"]


def test_celula_vazia_continua_vazia(tmp_path):
    fora = normalizar(pd.DataFrame({"voto": LINHAS}), COLUNAS, fixo, tmp_path)
    assert fora["voto_norm"][5:].isna().all()


def test_vazio_declarado_preenche_a_celula_vazia(tmp_path):
    cols = [dict(COLUNAS[0], vazio="Indeciso / Não Respondeu")]
    fora = normalizar(pd.DataFrame({"voto": LINHAS}), cols, fixo, tmp_path)
    assert list(fora["voto_norm"][5:]) == ["Indeciso / Não Respondeu"] * 3


def test_rotulo_vazio_para_a_rodada(tmp_path):
    with pytest.raises(RuntimeError, match="Zebra"):
        normalizar(pd.DataFrame({"voto": ["Zebra"]}), COLUNAS, fixo, tmp_path)


def test_mapping_csv_ordenado_por_frequencia(tmp_path):
    normalizar(pd.DataFrame({"voto": LINHAS}), COLUNAS, fixo, tmp_path)
    linhas = (tmp_path / "voto_norm.csv").read_text(encoding="utf-8").splitlines()
    assert linhas[0] == "bruto,freq,rotulo"
    assert linhas[1] == "Lula,2,Lula"


def test_os_prompts_reais_carregam_e_montam():
    pasta = Path(__file__).resolve().parents[2] / "ondas" / "2026-08-10" / "norm" / "prompts"
    for nome in ("voto_presidente", "numero_voto", "problema_brasil"):
        p = carregar_prompt(pasta / f"{nome}.json")
        m = montar_prompt(p, "texto de teste")
        assert p["exemplos"]
        assert m.endswith("Entrada: texto de teste\nSaída:")
