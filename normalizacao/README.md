# Normalização das colunas abertas

Roda um prompt sobre cada texto distinto de uma coluna de resposta aberta, grava
o mapping e substitui a coluna pelo rótulo. Modelo local, na GPU, via llama.cpp.

## Requisitos

- Linux ou WSL2 com GPU NVIDIA. O driver do Windows já expõe a GPU no WSL2:
  `nvidia-smi` funciona dentro da distro.
- CUDA Toolkit, `cmake` e um compilador C++. O `llama-cpp-python` compila da
  fonte, porque a roda publicada não tem CUDA.
- [uv](https://docs.astral.sh/uv/). Ele fornece o Python (≥ 3.11).
- O peso GGUF do modelo, no caminho de `modelo:` do YAML da onda. `~` é o home da
  distro, não o do Windows; guarde o peso no sistema de arquivos dela, não em
  `/mnt/`.

O repositório é usado dos dois lados: a normalização roda no WSL; o motor em R
roda no Windows ou no WSL.

## Por onda

`ondas/<onda>/norm/normalizacao.yaml` declara o modelo, a temperatura e as colunas:

```yaml
modelo: ~/modelos/Qwen_Qwen3-4B-Instruct-2507-Q4_K_M.gguf
temperatura: 0
colunas:
  - coluna: "Se a eleição para presidente fosse acontecer hoje, em quem você votaria?"
    nova: voto_presidente_norm
    prompt: voto_presidente.json
```

Cada prompt é um JSON em `prompts/` com `instrucao`, `regras` e `exemplos`
(`entrada` → `saida`). Fora dos rótulos fixos que a `instrucao` lista, a saída é
livre: nome de quem não disputa a eleição é resposta válida.

Célula vazia continua vazia — vazio quer dizer que a pergunta não foi feita. A
exceção é `vazio: "<rótulo>"` na coluna, para o instrumento que define o branco
como resposta.

O YAML e os prompts versionam. `mapping/` não: é resposta individual.

## Rodar

Tudo a partir da raiz do clone:

```sh
uv sync --project normalizacao                                           # base; os testes rodam sem modelo
CMAKE_ARGS="-DGGML_CUDA=on" uv sync --project normalizacao --extra cuda  # o modelo
uv run --project normalizacao pytest -q normalizacao

# o peso, uma vez
mkdir -p ~/modelos
curl -L -C - -o ~/modelos/Qwen_Qwen3-4B-Instruct-2507-Q4_K_M.gguf \
  https://huggingface.co/bartowski/Qwen_Qwen3-4B-Instruct-2507-GGUF/resolve/main/Qwen_Qwen3-4B-Instruct-2507-Q4_K_M.gguf

uv run --project normalizacao python normalizacao/normalizar.py 2026-08-10 \
  "/mnt/c/Users/<usuário>/Downloads/<export>.xlsx" ondas/2026-08-10/dados/onda_1.xlsx
```

A entrada é o export bruto, onde foi baixado. No WSL a pasta Downloads do Windows
é `/mnt/c/Users/<usuário>/Downloads/`; nome com espaço vai entre aspas. O export
— texto livre, `ip_hash`, fingerprints — não entra no repositório. A saída vai
para o caminho dado; o motor exige um único `.xlsx` em `ondas/<onda>/dados/`.

Depois de rodar:

1. Revise `ondas/<onda>/norm/mapping/<nova>.csv`: uma linha por texto distinto,
   com frequência e rótulo, do mais frequente ao menos. Rótulo vazio interrompe a
   rodada com o texto que o causou; não há fallback.
2. Declare em `ondas/<onda>/questionario.yaml` → `niveis` todo rótulo que ainda
   não está lá. O motor recusa resposta fora dos níveis declarados. O que aparece
   na tela — e o que vira `Outros` — é decisão do `display.yaml`.
3. Da raiz do repositório: `Rscript scripts/rodar-onda.R`.
