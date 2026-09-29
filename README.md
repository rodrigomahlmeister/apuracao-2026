# Apuração com projeção — Presidente 2026

Na noite da eleição, um único comando coleta os arquivos de divulgação do TSE, projeta o resultado final com
faixa de 90% e grava o que a página exibe.

```
Rscript R/apuracao.R oficial 1          # 1º turno; roda até Ctrl+C
Rscript R/apuracao.R oficial 1 23:59    # para às 23:59 (Brasília)
Rscript R/apuracao.R oficial 2          # 2º turno (código da eleição vem do ele-c.json)
```

Saídas: `saida/atual/` (`projecao.json`, `serie.json`, `grafico.png`) e o histórico completo em
`saida/<ambiente>_t<turno>_<data_hora>/` (`log.txt`, `serie.csv`, arquivos brutos de cada ciclo).

## Código da noite (`R/`)
| arquivo | função |
|---|---|
| `apuracao.R` | laço principal: coleta → estado → projeção → publicação |
| `ingestao.R` | arquivos do TSE (configuração, andamento, seções, resultados), ciclo incremental, detecção de reinício |
| `http.R` | requisições em paralelo com limite de taxa, ETag e pausa total em caso de bloqueio |
| `modelo.R` | o método de projeção |
| `grafico.R` | gráfico da apuração |
| `pesquisas.R` | (opcional) gera `config/prior.yaml` a partir de `config/pesquisas.csv` |

## Método
Base = resultado de 2022 levado aos locais de votação de 2026 (pareamento de locais). O arquivo de seções do
TSE informa quais seções de cada município já foram totalizadas, reconciliado proporcionalmente com o resultado
municipal. O swing desde 2022 é estimado na parte apurada de cada município (regressão no perfil da base,
Brasil → região → UF com encolhimento); a parte pendente recebe base + swing. Faixa de 90% por bootstrap,
calibrada no replay de 2022. A projeção só é exibida a partir de 2% do eleitorado apurado.
Detalhes e validação: `docs/notas_tecnicas.md`.

## Preparação (uma vez; já feita)
`preparo/00_extrair.R` → `01_base_hist.R` → `02_rodar_pareamento.R` → `03_base_2026.R`
(dados brutos do TSE em `dados_tse/`, bases em `dados/base/`).

## Testes
`Rscript -e 'testthat::test_dir("tests/testthat")'`

`estudos/` guarda os replays e comparações que levaram à escolha do método; não é usado na noite.
