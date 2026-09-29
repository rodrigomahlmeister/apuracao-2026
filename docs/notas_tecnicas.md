# Notas técnicas: apuração com projeção, Presidente 2026

## Fase 0: inventário de dados (27/09/2026)

### Acesso
- `cdn.tse.jus.br` e `dadosabertos.tse.jus.br` (Portal de Dados Abertos) → **HTTP 403 (Akamai)** a partir de IP dos EUA (Raleigh/NC). Geobloqueio; User-Agent e Referer não mudam nada. Downloads do portal precisam de VPN com IP brasileiro ou de baixa manual.
- `resultados.tse.jus.br` e `resultados-sim.tse.jus.br` (divulgação) **acessíveis** do mesmo IP.

### Arquivos obtidos

| Arquivo | Origem | Tamanho | Conteúdo |
|---|---|---|---|
| `votacao_secao_2022_BR.zip` | manual | 271 MB zip / 1,59 GB CSV | só presidente (`CD_CARGO` = 1), 1º e 2º turnos, por seção |
| `votacao_secao_2018_BR.zip` | manual (VPN) | 251 MB / 1,83 GB | idem 2018 |
| `detalhe_votacao_secao_2018.zip` | manual (VPN) | 256 MB | um CSV por UF + `_BR` (presidente) + `_BRASIL` (todos os cargos) |
| `bweb_{1t,2t}_<UF>_<carimbo>.zip` (56) | manual (VPN) | 1,5 GB (SP 1T: 9,4 GB descompactado) | boletins de urna 2022, 27 UFs + ZZ; carimbos `051020221321` (1T) e `311020221535` (2T) |
| `eleitorado_local_votacao_{2018,2022,2026}.zip` | manual (VPN) | 84 / 24 / 88 MB | uma linha por seção; 2026 traz um CSV por UF + `_BRASIL` |
| `dados_tse/leiame_votacao_secao_2022.pdf` | dentro do zip | | dicionário |
| `dados_tse/amostras_simulado/*.json` | simulado 2026 | | `ele-c`, `-u` BR, `-ab` BR, `cm`, `cs` (AC) |

`votacao_secao_2022_BR.csv` (Latin-1, `;`), colunas:
`DT_GERACAO, HH_GERACAO, ANO_ELEICAO, CD_TIPO_ELEICAO, NM_TIPO_ELEICAO, NR_TURNO, CD_ELEICAO, DS_ELEICAO, DT_ELEICAO, TP_ABRANGENCIA, SG_UF, SG_UE, NM_UE, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, CD_CARGO, DS_CARGO, NR_VOTAVEL, NM_VOTAVEL, QT_VOTOS, NR_LOCAL_VOTACAO, SQ_CANDIDATO, NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO`

- Brancos = `NR_VOTAVEL` 95, nulos = 96. **Não há aptos nem abstenções.** Comparecimento = soma de `QT_VOTOS` da seção, que confere com o oficial (1T 123.682.372; 2T 124.252.796).
- Totais conferem com o oficial: 1T Lula 57.259.504, Bolsonaro 51.072.345, brancos 1.964.779, nulos 3.487.874.
- 472.028 seções, 92.523 locais (chave `SG_UF+CD_MUNICIPIO+NR_ZONA+NR_LOCAL_VOTACAO`), 5.710 "municípios" (inclui cidades do exterior, `SG_UF = ZZ`, 162 locais).
- Nenhuma seção muda de local entre os turnos. 329 chaves de local têm mais de um `NM_LOCAL_VOTACAO` (variação de grafia).
- **Sem CEP e sem coordenadas**: só nome e endereço em texto. O pareamento precisa do eleitorado por local de votação.

### Divulgação 2026 (simulado), confirmado nos arquivos
- `ele-c.json` (simulado, dg 14/09/2026): pleito `17801`, eleição federal 1T `21270`, `cdt2` = `21271`; estadual `21272`/`21273`. Templates de diretório no campo `arq` (`tp`: `ft, cm, e, cs, t, ab, u, aux`).
- `ele-c.json` **oficial** ainda aponta para `ele2024` (dg 12/05/2026): **os códigos 2026 oficiais (6257/3220 no prompt) ainda não estão publicados**. Reconfirmar no dia.
- `br-c0001-e021270-u.json`: `carg → agr → par → cand` (e `carg$fed`); blocos `s` (ts, st, pstn…), `e` (te, est, pestn, c, pcn, a, pan…), `v` (tv, vv, vb, tvn, van, vansj…). Campos são texto; os sufixados `n` usam ponto decimal.
- `br-e021270-ab.json`: `abr` com 29 linhas (BR, 27 UFs, ZZ), cada uma com `s` e `e`, `munf`/`munpt`/`munnr` (municípios finalizados / parcialmente totalizados / não recebidos), e `dt`/`ht`.
- `config/mun-e021270-cm.json` (nome no padrão de 2022; respondeu 200): UFs → `mu` (`cd` código TSE, `cdi` código IBGE, `nm`, `c` = capital `s|n`, `z` zonas). 5.755 municípios (5.728 `n` + 27 `s`), 28 abrangências (27 UFs + `zz`).
- `arquivo-urna/17801/config/<uf>/<uf>-p017801-cs.json` (padrão de 2022; 200): UF → `mu` → `zon` → `sec` com `ns` (seção), `da`/`ha` (data/hora), `nsa`, `nsp`. AC: 3.008 entradas vs `ts` = 3.006 no `-ab`. **Hipótese**: `da/ha` = horário de disponibilização do BU da seção, portanto **status de totalização por seção**. Confirmar na janela do simulado (28–29/09, 14h–16h): ver se seções pendentes aparecem sem `da/ha` ou somem. `nsa`/`nsp` parecem indicar seções agregadas (a verificar). **O `cs` não tem local de votação**, então o mapa seção → local de 2026 precisa vir do eleitorado por local/seção 2026.

### Extração (`R/00_extrair.R` → `dados/base/`, ~110 MB no total)
- Leitura em streaming com `arrow::open_dataset` para os CSVs grandes. **Cuidado:** `open_dataset` ignora `encoding`, então só vale para colunas sem acento. Colunas de texto (nomes) são lidas com `fread` (arquivos ≤ 400 MB). Descompactação com `zip::unzip`, porque o `unzip` do R falha acima de 4 GB.
- Saídas: `votos_secao_<ano>` (formato longo, por `nr_votavel`), `secoes_<ano>` (aptos, comparecimento, abstenções, horário do BU), `eleitorado_secao_<ano>`, `locais_<ano>` (nome, CEP, lat/lon, eleitores), `municipios_<ano>`.
- Blocos de candidatos em `config/blocos.yaml`. **O número do PL em 2026 está a confirmar.**

### Boletins de urna 2022 (`bweb`): confirmado
- Colunas-chave: `DT_BU_RECEBIDO` (horário de recebimento do BU, fuso de Brasília), `QT_APTOS`, `QT_COMPARECIMENTO`, `QT_ABSTENCOES`, `NR_LOCAL_VOTACAO`, `CD_TIPO_URNA`, `DS_AGREGADAS`, `CD_CARGO_PERGUNTA` (1 = Presidente).
- 472.028 seções por turno, **batendo 1:1 com `votacao_secao_2022`** (mesmas chaves; comparecimento idêntico em todas). Aptos 156.453.354 (o eleitorado oficial é 156.454.011; diferença de 657). Todas as seções têm horário.
- 1T: primeiro BU 02/10 02:09 (exterior, Ásia/Oceania), mediana 19:34, último 03/10 19:28. 2T: mediana 30/10 18:26.
- Dispensa o `detalhe_votacao_secao_2022`.

### Detalhe por seção 2018: confirmado
- `detalhe_votacao_secao_2018_BR.csv` (presidente): `QT_APTOS`, `QT_COMPARECIMENTO`, `QT_ABSTENCOES`, `DT_RECEBIMENTO_BU_HOR_TSE`, `DT_PRIM_TOT_PARCIAL_HOR_TSE`, `NR_LOCAL_VOTACAO`.
- 454.490 seções por turno; aptos 147.306.295; comparecimento 1T 117.364.654. **Há horários extremos** (1T até 23/10/2018; 2T até 25/02/2019): são seções re-totalizadas ou sub judice, a tratar no replay.

### Eleitorado por local de votação: confirmado
- Uma linha por seção (só 1º turno): `NR_SECAO`, `CD_TIPO_SECAO_AGREGADA`, `NR_SECAO_PRINCIPAL`, `NR_LOCAL_VOTACAO`, `NM_LOCAL_VOTACAO`, `NR_CEP`, `NR_LATITUDE`, `NR_LONGITUDE`, `QT_ELEITOR_SECAO`. Em 2026 as coordenadas usam vírgula decimal.
- Por isso o arquivo de 2026 **já dá o mapa seção → local**; não é preciso o perfil por seção.

| ano | seções | agregadas | eleitores | locais | % eleitorado c/ coordenada | % c/ CEP |
|---|---|---|---|---|---|---|
| 2018 | 481.863 | 27.366 | 147.306.275 | 95.590 | 81,7 | 99,0 |
| 2022 | 496.856 | 24.781 | 156.454.011 | 92.386 | 94,3 | 99,4 |
| 2026 | 517.179 | 17.931 | 158.745.463 | 95.600 | 98,5 | 99,6 |

Coordenada válida = dentro do retângulo do Brasil; exterior (ZZ) sempre sem coordenada. Cobertura fraca: **2018 MG 1,1%, ES 3,3%, SE 63%, BA 69%**; 2022 BA 71%, ES 70%, SE 73%, AP 89%; 2026 AP 88%, SE 92%, RR 94%. No teste 2018 → 2022, MG e ES quase só pareiam por chave ou CEP.

## Fase 3 (preliminar): cliente mínimo, testado no simulado estático (28/09, madrugada)

### Arquivos e padrões de nome (todos com 200 no simulado)
Diretórios vêm do `ele-c.json` (`arq`), com `<base>/<ambiente>` = raiz do ambiente em `config/params.yaml`.

| tp | arquivo | conteúdo |
|---|---|---|
| cm | `<ele>/config/mun-e021270-cm.json` | municípios (código TSE, IBGE, capital, zonas) |
| ab | `<ele>/dados/<uf>/<uf>-e021270-ab.json` | andamento: BR → por UF; **UF → por município** (`s`, `e`) |
| u | `<ele>/dados/<uf>/<uf>[<mun>]-c0001-e021270-u.json` | resultado BR, UF ou município |
| cs | `arquivo-urna/17801/config/<uf>/<uf>-p017801-cs.json` | todas as seções com `da`/`ha`; agregadas têm `nsp` (principal) e a principal lista `nsa` |
| aux | `arquivo-urna/17801/dados/<uf>/<mun>/<zona>/<secao>/p017801-<uf>-m<mun>-z<zona>-s<secao>-aux.json` | **`st` = status da seção** ("Totalizada"), `hashes[].arq` = arquivos do BU (vazio no estado estático) |

- Servidor responde **ETag/Last-Modified** (`If-None-Match` → 304) e **gzip** (~10×). Cabeçalho `X-RateLimit-Limit: 2000;w=1`, mas usamos o limite do manual (100/s), com teto de 20/s.
- No estado estático (100% apurado), a soma de seções do `cs` (sem as agregadas) = `ts` do `ab` em todos os 5.755 municípios (528.951).

### Custo de um ciclo completo (20 req/s, 10 simultâneas)
- 6.141 requisições: 29 `ab` + 29 `u` BR/UF + 5.755 `u` municipais + 28 `cs` + 300 `aux` (amostra SP e RJ).
- Rede ~330 s (`u` municipais = 305 s, no limite de 20/s); parse ~40 s depois de otimizado (era 3,5 min). **Ciclo ≈ 6 min.**
- Com ETag, arquivos que não mudaram voltam como 304 e não são reprocessados.
- Timeout ocasional (1 em 5.755): `req_retry(retry_on_failure = TRUE)`.

### Execução na janela
- `R/run_simulado.R` (ciclos seguidos até 16:10 de Brasília), chamado por `R/run_simulado.cmd`, agendado no Agendador de Tarefas do Windows ("TSE_simulado_2026-09-28", 12:58 horário local = 13:58 de Brasília).
- Saída: `dados/simulado/<data>_<HHMM>/ciclo_<HHMMSS>/{bruto,ab,u_tot,u_cand,cs,aux,stats}.parquet`, BU em `bu/`.
- `dados/simulado/2026-09-28/` contém dois ciclos de teste pré-janela (estado estático).
- Análise: `Rscript R/analise_simulado.R <pasta>`.

## Fase 1: bases históricas (`R/01_base_hist.R`)
- `hist_local_<ano>.parquet` (2018: 182.198 linhas; 2022: 185.046, somando os dois turnos) e `hist_mun_<ano>.parquet`: turno, uf, mun, zona, local, secoes, aptos, comparecimento, brancos, nulos, validos, PT, PL, OUTROS, exterior. Blocos vêm de `config/blocos.yaml`.
- Aptos e local por seção vêm do BU (2022) e do detalhe (2018); votos, de `votacao_secao`. Em 2022 o casamento é 1:1. Em 2018 sobram 82 seções só no detalhe, todas com comparecimento 0 (não instaladas).
- Conferência com o oficial (% dos válidos): 2018 1T PT 29,28 / PL 46,03; 2T 44,87 / 55,13. 2022 1T 48,43 / 43,20; 2T 50,90 / 49,10.
- `municipios_ibge.parquet`: código TSE → IBGE (do `cm.json`) + região, região imediata (510) e intermediária (API do IBGE). Exterior (184) sem região. **Boa Esperança do Norte (MT)** foi criado depois de 2022 e não está na API: recebe a região de Sorriso e **não tem base em 2022** (tratar no pareamento).
- `renv`: adiado para depois da janela de 29/09, para não mexer na biblioteca usada pela execução agendada. Biblioteca do renv fora do OneDrive (`RENV_PATHS_LIBRARY`).

## Fase 2: pareamento de locais (`R/02_pareamento.R`, `R/02_rodar_pareamento.R`, ~12 min)
- **Chave do local nos anos-base = número do dia da eleição.** No eleitorado de 2022 (gerado em 2024), `NR_LOCAL_VOTACAO_ORIGINAL` bate com o BU em 100% das seções e `NR_LOCAL_VOTACAO` em 98,6% (houve renumeração depois da eleição). Em 2018 o original bate em 96,7%. Os locais-base são definidos pelo BU/detalhe; CEP e coordenadas entram pela seção, preferindo linhas com original = local do BU. Em 2026 (arquivo pré-eleição) usa-se o número atual.
- **CEP genérico**: 71–74% dos locais têm CEP terminado em 000 (genérico do município). No nível 2, CEP só conta se for específico (não termina em 000 e é usado por no máximo 2 locais do município).
- Saída: pesos (t_id, b_id, v), com `sum(v * aptos_b) = 1` por componente; a base de cada local-alvo é formada por pseudo-contagens (`base_projetada()`).

### 2026 ← 2022: % do eleitorado 2026 por nível
| nível | total | 101 cidades grandes |
|---|---|---|
| 1 chave exata | 93,31 | 92,89 |
| 2 mesmo prédio | 2,18 | 3,53 |
| 3 vizinhos | 3,42 | 3,26 |
| 3c prefixo de CEP | 0,49 | 0,20 |
| 4 zona | 0,58 | 0,12 |
| 5 / 5n / sem base | 0,02 | 0,00 |

Semi-novos (tamanho mudou mais de 50%): 3.555 locais, 3,25% do eleitorado. 119 municípios têm mais de 10% do eleitorado nos níveis 4–5 (1,6 milhão de eleitores): Alagoinhas (81%), Simões Filho (100%), Paço do Lumiar (53%), Camaçari (12%), Ilhéus (16%), Lisboa/Porto/Orlando/Edimburgo (exterior, sem coordenadas, renumerados) e Boa Esperança do Norte (município novo, vizinhos de 2022 num raio de 30 km). Lista em `docs/fase2_municipios_nivel45.csv`; por cidade grande em `docs/fase2_niveis_cidades_grandes.csv`.

### Validação 2022 ← 2018 (erro absoluto médio na % dos válidos, p.p., ponderado por válidos)
Previsão = base pareada + swing do município (1T: razões log com OUTROS de referência; 2T: logit PT × PL). "ref" = usar só o resultado do município, sem informação do local.

| nível | % eleit. | 1T PT | ref | 2T PT | ref |
|---|---|---|---|---|---|
| 1 | 97,1 | 3,91 | 5,08 | 2,07 | 4,79 |
| 2 | 0,6 | 5,68 | 5,13 | 3,91 | 4,84 |
| 3 | 1,1 | 5,58 | 5,52 | 4,09 | 5,28 |
| 3c | 0,3 | 7,50 | 5,22 | 5,62 | 4,89 |
| 4 | 0,7 | 5,22 | 5,19 | 4,71 | 4,85 |
| 5 | 0,05 | 5,05 | 5,04 | 4,76 | 4,75 |

Sem MG, ES, BA e SE o padrão é o mesmo (nível 1: 3,99 / 2,10). Semi-novos no nível 1: 4,29 (ref 5,62). Só nas 101 cidades grandes, nível 1: 1T 5,03 (ref 5,37); 2T 2,23 (ref 4,94).
- **3c é pior que usar o município**: vai ser eliminado (sem coordenadas, desce direto para zona/município).
- Nível 2 e nível 3 ganham pouco ou nada no 1T; ganham ~1 p.p. no 2T.
- Parametrização alternativa no 1T (logit PT × PL + logit OUTROS × resto): nível 1 = 3,73 contra 3,91. Ganho pequeno; incluir como variante no replay.

### Revisão da Fase 2 (28/09, madrugada)
Mudanças: (i) 3c eliminado; (ii) nível 2 = distância < 100 m quando os dois anos têm coordenadas, senão CEP igual, único àquele local nos dois anos e não terminado em 000; (iii) **imputação de coordenadas da base**: local-base sem lat/lon herda do mesmo local (mesma chave e mesmo CEP) nos arquivos de eleitorado de 2024/2026 (2018: 2022/2024/2026). Cobertura da base 2022: 94,3% → 98,3%; base 2018: 81,7% → 97,8%; (iv) semi-novos com w = 0,75 no próprio histórico.

**Ponte por 2024 (pedido 6): não compensa.** Nos 185 municípios com > 10% nos níveis 4–5 (fora o exterior), só 4 locais melhoram de nível (os "166" de uma primeira rodada vinham de um erro de numeração, já corrigido e coberto por teste). O eleitorado de 2024 tem só 48% de coordenadas nesses municípios. O problema real era falta de coordenadas na base de 2022 (BA: 12%), resolvido pela imputação. Código mantido em `R/02_ponte_2024.R`, fora da produção.

2026 ← 2022 (produção, `pareamento_2026_2022_{pesos,nivel}.parquet`): nível 1 93,31%; 2 2,07%; 3 3,73%; 4 0,85%; 5/5n/sem base 0,04%. Cidades grandes: 4–5 = 0,28%. Municípios com > 10% nos níveis 4–5: 262 → 225 (3,36 → 2,40 milhões de eleitores; inclui o exterior, 312 mil). Nível 2: com a regra nova saíram 174 locais (146 → nível 3, 26 → 4, 2 → 5); com a imputação entraram 60.

Validação 2018 → 2022 (PT, p.p.; mae por válidos / por eleitorado / rmse; ref = só o município):
| nível | % | 1T mae | 1T rmse | ref | 2T mae | 2T rmse | ref |
|---|---|---|---|---|---|---|---|
| 1 | 97,2 | 3,92 / 3,88 | 5,26 | 5,08 | 2,07 / 2,07 | 2,89 | 4,79 |
| 2 | 0,6 | 5,46 | 7,92 | 5,10 | 3,73 | 5,54 | 4,74 |
| 3 | 1,7 | 5,45 | 7,23 | 5,24 | 3,96 | 5,51 | 4,98 |
| 4 | 0,5 | 5,95 | 9,24 | 5,78 | 5,32 | 8,70 | 5,49 |
| 5 | 0,05 | 4,92 | 6,71 | 4,91 | 4,64 | 6,49 | 4,63 |

Semi-novos, w no próprio histórico (mae PT 1T / 2T): 0 → 5,45 / 4,14; 0,25 → 4,62 / 3,15; 0,5 → 4,33 / 2,63; **0,75 → 4,55 / 2,55**; 1 → 5,18 / 2,90. Escolhido 0,75 (melhor no 2T; no 1T, 0,5 é melhor).

**Erro agregado na cidade** (apuração de 25% ou 50% dos locais em ordem ALEATÓRIA; swing estimado nos locais apurados; resto projetado; ref = % bruta dos apurados), mae em p.p.:
| turno | apurado | 101 grandes: cascata / bruto | demais (≥ 10 locais): cascata / bruto |
|---|---|---|---|
| 1T | 25% | 1,08 / 0,60 | 1,39 / 2,02 |
| 1T | 50% | 0,67 / 0,37 | 0,83 / 1,18 |
| 2T | 25% | 0,36 / 0,53 | 0,84 / 1,91 |
| 2T | 50% | 0,23 / 0,35 | 0,47 / 1,11 |

Nas cidades grandes, no 1T, a cascata com swing uniforme perde para a % bruta, mesmo com a variante logit (1,07). Com ordem aleatória a % bruta não tem viés, e o swing uniforme erra porque a variação de 2018 para 2022 dependeu do perfil do local. Motiva o swing contínuo em função de η (GAM) da Fase 4. O teste decisivo é o replay na ordem real de chegada, em que a % bruta tem viés.

### Testes antes da Fase 4 (`R/02_teste_swing_cidade.R`)
**A. Nível 2 como semi-novo** (w × próprio + (1 − w) × vizinhos), erro PT (p.p.), só locais de nível 2:
| w | 1T | 2T |
|---|---|---|
| 0 | 5,47 | 3,95 |
| 0,25 | 5,15 | 3,61 |
| **0,5** | **5,03** | **3,46** |
| 0,75 | 5,20 | 3,54 |
| 1 (antes) | 5,70 | 3,88 |
ref (só município): 1T 5,10; 2T 4,74. **Adotado**: todo nível 2 é semi-novo com w_n2 = 0,5 (os semi-novos por tamanho mantêm w = 0,75).

**B. Estimador do swing dentro das 101 cidades grandes** (2018 → 2022; erro agregado da cidade em p.p. de PT; média entre cidades / p90)
| turno | ordem | apurado | E1 % bruta | E2 swing uniforme | E3 regressão | E4 encolhido κ=20 | E4 κ=200 |
|---|---|---|---|---|---|---|---|
| 1T | aleatória | 25% | 0,83 / 1,81 | 1,00 / 2,02 | 0,28 / 0,63 | 0,28 / 0,63 | 0,29 / 0,64 |
| 1T | aleatória | 50% | 0,47 / 1,02 | 0,62 / 1,20 | 0,16 / 0,34 | 0,15 / 0,33 | 0,16 / 0,32 |
| 1T | PL primeiro | 25% | 6,42 / 10,35 | 4,36 / 7,45 | 1,06 / 2,42 | **0,74 / 1,51** | 0,77 / 1,56 |
| 1T | PL primeiro | 50% | 4,00 / 6,28 | 2,75 / 4,90 | 0,42 / 0,77 | **0,36 / 0,75** | 0,40 / 0,85 |
| 2T | aleatória | 25% | 0,72 / 1,63 | 0,41 / 0,88 | 0,29 / 0,65 | 0,29 / 0,63 | 0,30 / 0,65 |
| 2T | aleatória | 50% | 0,42 / 0,89 | 0,28 / 0,57 | 0,16 / 0,34 | 0,16 / 0,35 | 0,16 / 0,35 |
| 2T | PL primeiro | 25% | 5,99 / 9,51 | 2,18 / 3,77 | 1,19 / 2,68 | **0,80 / 1,48** | 0,77 / 1,56 |
| 2T | PL primeiro | 50% | 3,67 / 5,82 | 1,40 / 2,29 | 0,48 / 1,00 | **0,39 / 0,82** | 0,43 / 0,91 |

E3: `swing ~ eta_PT_base + eta_PL_base` (1T; 2T: logit PT × PL) nos locais apurados, ponderado por eleitorado; intercepto zera o resíduo médio nos apurados. E4: slopes encolhidos para os slopes "dentro das cidades" de todas as grandes, peso n/(n + κ).
**Conclusão**: o swing uniforme estava mal especificado. A regressão no perfil da base corta o erro por 3 a 6 vezes, e o encolhimento (κ ≈ 20) ajuda quando a ordem é enviesada e há poucos locais apurados. Especificação para as cidades grandes na Fase 4: **E4 com κ = 20**. Obs.: os slopes agrupados usam só os locais apurados no próprio cenário (ao vivo: apurados das outras cidades grandes, da mesma noite).

## Ajustes de ingestão para a janela do simulado (28/09)
- **Reinício**: se o nº de seções totalizadas de BR ou de alguma UF cai em relação ao ciclo anterior, abre-se nova rodada (`<execução>/rodada_NN/`), e as marcas de "já baixado", as ETags e o cache de parse são zerados.
- **Bloqueio**: 429 e 403 não são repetidos. Requisições em lotes de 200; ao ver 429/403, nenhuma requisição por 11 min (tentar durante o bloqueio reinicia a contagem de 10 min). Só 5xx e falhas de rede têm retry com backoff.
- **Destinação**: válidos = só candidatos com `dvt` começando por "Válido" (`votos_blocos()`); "Anulado" e "Anulado sub judice" ficam de fora, registrados à parte. No simulado estático, o maior candidato em votos (nº 57) é "Anulado sub judice": somar `vap` de todos erraria os válidos em ~10%.
- **Seções**: `-ab`/`-u` passam a trazer `snt, sni, sna, esnt, esi, esni, esa, esna`. No estado estático há 1 seção não instalada (BA, município 35572, `sni` = 1), contada como totalizada (`st = ts`), então completo = `st >= ts` funciona.
- **Conferência**: `<execução>/conferencia.csv` a cada ciclo (BR, SP, BA, RS: % totalizadas, válidos, 3 primeiros em % dos válidos) + linha "BR: ..." no log, para comparar com o app oficial.
- Código do município sempre com 5 dígitos (checado; vem do `cm.json`).

## Fase 4 (versão mínima): `R/04_modelo.R`, `R/05_replay.R`, `R/06_live.R`, `R/06_grafico.R`
- **Prior**: `config/pesquisas.csv` → `Rscript R/pesquisas_prior.R` → `config/prior.yaml` (média simples em válidos; dp 3 p.p. por bloco). Intercepto nacional do swing ~ N(razões log das pesquisas − razões log da base nacional, dp pelo método delta). 2022: `config/prior_2022_{1t,2t}.yaml` (1T: Datafolha, Quaest, Ipec → 50/37/13; 2T: Datafolha, Quaest → 52/48).
- **Unidade**: município. Parte apurada = votos reais; parte pendente = base × (comparecimento × taxa de válidos) × proporções projetadas.
- **Base da parte pendente**: A = base do município inteiro, reescalada; B = base dos locais das seções pendentes (ao vivo: seções sem data/hora no `cs`).
- **Swing** (razões log): regressão ponderada `swing ~ e1 + e2 + log(eleitorado)` (centrados na média nacional), Brasil → região → UF, cada nível encolhido para o de cima com n/(n + 30); intercepto nacional combinado com o prior por precisão. Exterior = nível Brasil. Só entram municípios com ≥ 90% do eleitorado de nível 1.
  - `unidades = "completos"` (versão mínima): só municípios 100% totalizados.
  - `unidades = "apurado"` (**B_parc**): parte apurada de todo município, com a base exata da parte apurada (base total − base pendente). Só é possível quando se sabe quais seções foram apuradas (`cs`).
- **Comparecimento/válidos**: razão real/base nas unidades de estimação da UF, encolhida para o Brasil com n/(n + 20).
- **Faixa**: bootstrap de Poisson (peso ~ Poisson(1) por município, equivale a reamostrar dentro da UF) + sorteio do intercepto nacional pela posterior; quantis 5–95%; P(2º turno) = fração das réplicas sem bloco > 50%.
- **Desempenho ao vivo**: base por seção em cache (`dados/base/base_live_2026.rds`; apagar se o pareamento mudar); estado do ciclo ~3 s; faixa com 200 réplicas ~20 s.
- Testes: `tests/testthat/test_modelo.R` (swing conhecido recuperado; sem dado segue o prior; estimação pela parte apurada).

## Fase 5 (primeira rodada): replay 2022 na ordem real (`R/05_replay.R`, passos de 5 min, 50 réplicas)
Base 2018 pareada aos locais de 2022; ordem = `DT_BU_RECEBIDO`; início 17:00. Fim da noite (99,9% das seções): 1T 23:55 (472 seções depois, 0,10% do eleitorado); 2T 21:03 (472 seções, 0,12%).
Métodos: bruto; uf (extrapolação por UF); A/B × prior pesquisas/zero (unidades = completos); **B_parc** (B + estimação pela parte apurada de todos os municípios, prior pesquisas).

Erro absoluto médio (p.p.; 1T: média de PT e PL; 2T: PT), por faixa de % do eleitorado apurado:
| faixa | bruto | uf | A_pesq | B_pesq | B_zero | B_parc |
|---|---|---|---|---|---|---|
| 1T (0,5] | 5,47 | 2,44 | 6,05 | 6,05 | 5,06 | **1,13** |
| 1T (5,10] | 5,56 | 1,69 | 1,16 | 1,21 | 1,11 | **0,03** |
| 1T (20,30] | 4,78 | 1,95 | 2,16 | 1,93 | 1,94 | **0,04** |
| 1T (40,50] | 3,74 | 1,46 | 0,45 | 0,17 | 0,17 | **0,03** |
| 1T (70,80] | 2,15 | 0,79 | 0,14 | 0,07 | 0,07 | **0,05** |
| 2T (0,5] | 6,26 | 4,24 | 7,15 | 7,15 | 4,82 | **2,71** |
| 2T (5,10] | 3,78 | 1,36 | 6,25 | 6,27 | 4,86 | **0,11** |
| 2T (20,30] | 2,17 | 0,77 | 0,76 | 0,59 | 0,60 | **0,05** |
| 2T (50,60] | 1,06 | 0,75 | 0,94 | 0,69 | 0,69 | **0,04** |

Marcos (% apurado a partir do qual vale sempre):
| método | 1T erro < 0,5 | 1T < 0,25 | 1T líder certo | 2T < 0,5 | 2T < 0,25 | 2T líder certo |
|---|---|---|---|---|---|---|
| bruto | 97,6 | 99,0 | 67,3 | 87,7 | 95,5 | 68,6 |
| uf | 92,1 | 96,9 | 0,6 | 78,9 | 90,5 | 48,2 |
| A_pesq | 63,6 | 71,1 | 0,2 | 68,6 | 84,1 | 64,0 |
| B_pesq | 59,8 | 59,8 | 0,2 | 64,0 | 73,9 | 39,3 |
| **B_parc** | **0,7** | **4,8** | **0,2** | **0,7** | **1,5** | **0,3** |

- **B > A** (base dos locais pendentes ajuda), mas o salto grande vem de **estimar o swing pela parte apurada com a base exata dessa parte** (B_parc). Isso depende de saber quais seções foram apuradas (`cs`), a confirmar na janela.
- **Prior**: só importa antes de ~5–10% apurado; as pesquisas finais de 2022 superestimaram Lula, e o prior delas piora o início (1T 6,05 × 5,06 com prior zero; 2T 7,15 × 4,82). Depois de 10%, pesquisas = zero.
- **Cobertura da faixa de 90%**: B_parc 100% até ~70% apurado (faixa larga demais no meio?) e cai para 25% e 0% em 80–100% (estreita demais no fim); B_pesq irregular. Calibração pendente: piso de variância que não some quando o apurado → 100% e inflação empírica por faixa.
- Ressalva: no replay a lista de seções apuradas é exata e síncrona com os votos; ao vivo depende do `cs` estar sincronizado com o `-u` do município.
- Gráficos: `figs/replay_2022_t{1,2}_{B_parc,B_pesq,A_pesq,B_zero}.png`; séries: `dados/replay/replay_2022_t{1,2}.parquet`.

## Janela do simulado 28/09 (14h–16h Brasília; única janela, não há outra)
Execução agendada `dados/simulado/2026-09-28_1358/` (55 ciclos, 13:58–16:10 Brasília) + sonda de zonas (`sonda_zona/`).
- **Rodadas**: rodada 1 = estado estático (100%, 24/09). Às 14:36 o simulado foi reiniciado e chegou a 50% (rodada 2). Às 14:40 foi reiniciado de novo (rodada 3), com UFs voltando a zero em momentos diferentes (pi, ms, ac, ap, df primeiro). Na rodada 3 a apuração foi de 0% a 84% entre 15:05 e 16:09. Os reinícios foram detectados e separados automaticamente. Sem bloqueio (nenhum 429/403).
- **(a) Parse**: sem erro em 55 ciclos; timeouts isolados (1 em ~5.755). Durante a apuração o `-u` BR não traz `pstn`: calcular `st/ts` (corrigido na conferência).
- **(b) Status por seção**:
  - **`cs` (EA16)**: seção pendente sem `da`/`ha`, que ganha horário quando o arquivo auxiliar da seção é gerado; zera no reinício. Especificação oficial 2026 (`docs/tse-ea16-...pdf`, 22/05/2026): "da/ha = data/hora do arquivo auxiliar da seção, disponível somente para a seção principal e após a geração do arquivo auxiliar", e vale para `f = "o"` (oficial). (O `cs` oficial de 2024 não tinha esses campos: formato novo em 2026.)
  - Sincronia na rodada 3 (seções com hora no `cs` × totalizadas no `-ab`): iguais na maioria dos ciclos; em 4 ciclos o `cs` ficou atrás por um ciclo (ex.: 82.822 × 105.818) e alcançou no seguinte; uma vez ficou à frente (391.504 × 391.103). O `-u` municipal fica atrás do `-ab` e do `cs` (na rodada 2: 289.332 × 312.457 × 317.820); 86% dos municípios parciais com `cs` = `-u`.
  - **Sonda por zona** (5 capitais, 162 zonas, `cs` baixado logo antes do `-u`): 162/162 zonas com `cs` = `-u` em 5 de 6 passadas (na 1ª, 3 zonas com `cs` ~1 min atrás). A soma das zonas = município sempre.
  - **`aux` (EA18) não serve no simulado**: sempre "Totalizada", sem arquivos, nunca muda (304). No oficial de 2024 o `aux` tinha status e horário real de recebimento (`dr`/`hr`) e lista de arquivos como objetos `{nm, tp}` (corrigir `parse_aux`).
- **Arquivo por zona** (EA20, `<uf><mun>-z<zona>-c0001-e<ele>-u.json`): existe e é atualizado ao vivo. Documentado em `docs/tse-instrucoes-para-download-...pdf`.
- **(c) Custo e tempo**: ciclos incrementais de 386 a 1.329 requisições e 10 a 54 s de rede. **A varredura completa a cada 5 ciclos custa ~6.141 requisições e ~200 s**, porque mesmo com 304 cada município é uma requisição, e isso estoura a meta de 2 min. Nas varreduras, 700 a 2.250 arquivos vieram 200 sem mudança de andamento no `-ab`: o TSE regenera o `-u` (dg/hg/ETag novos) mesmo sem dado novo. → trocar a varredura por **varredura rotativa** (1/5 dos municípios por ciclo).
- **Seções não instaladas / anuladas**: há 1 não instalada (Ibirapuã/BA, 35572, `sni = 1`). No fim da rodada 3 o município aparece com `st = ts = 28` e `pstn = 100`: a não instalada conta como totalizada, e completo = `st >= ts` funciona. `sna` (não apuradas) = 0 em todos os ciclos; as seções anuladas mencionadas no comunicado não apareceram como contador próprio nos arquivos.
- **Destinação**: o maior candidato do simulado é "Anulado sub judice" (nº 57), e há outro "Anulado" (nº 60); `votos_blocos()` os exclui dos válidos corretamente.
- **(d) Boletins de urna**: nenhum. O `aux` do simulado não lista arquivos. Download de BU não testado; fora do plano (o dado por zona substitui).

## Ingestão ajustada após a janela (28/09, noite)
- **Ordem**: `ab` → `cs` → `-u` BR/UF → `-u` municipais → `-u` por zona. O `cs` vem antes dos `-u` porque o `-u` municipal tende a ficar atrás; baixá-lo depois só aumentava a defasagem.
- **Varredura rotativa**: em vez de todos os 5.755 municípios a cada 5 ciclos (~200 s), a cada ciclo baixa os de andamento novo + a fatia `índice %% 5 == ciclo %% 5` (~1.150, quase todos 304). Ciclo de teste: 1.307 requisições em 46 s.
- **Resultado por zona** (EA20) das 101 cidades grandes (`config/cidades_grandes.csv`, capitais + > 200 mil eleitores em 2026): 540 zonas; baixadas quando o município está selecionado no ciclo. Saída `u_zona.parquet` e `u_zona_cand.parquet` (com uf, mun, zona).
- **`parse_aux`** aceita arquivos como texto (simulado) ou como objeto `{nm, tp}` (oficial 2024); teste em `tests/testthat/test_aux.R`.

## Fase 5: replay comparativo de métodos (`R/05_replay_metodos.R`; 5 min; 40 réplicas; atraso de 2 min)
Todos os métodos sobre a mesma apuração (ordem real dos BUs de 2022). Marcos: % apurado a partir do qual o erro fica sempre abaixo do limite (1T: média PT/PL; 2T: PT).
| método | 1T < 0,5 | 1T < 0,25 | 2T < 0,5 | 2T < 0,25 | 2T líder certo | dessinc. (1T/2T) |
|---|---|---|---|---|---|---|
| bruto | 97,6 | 99,0 | 87,7 | 95,5 | 68,6 | – |
| uf | 92,1 | 96,9 | 78,9 | 90,5 | 48,2 | – |
| M_A (mínima, base do município) | 63,6 | 71,1 | 68,6 | 84,1 | 64,0 | – |
| M_B (mínima, base via cs) | 59,8 | 59,8 | 64,0 | 73,9 | 39,3 | – |
| P_mun (sincronia perfeita) | 0,7 | 4,8 | 0,7 | 1,5 | 0,3 | 0 |
| **P_mun_ua** (-u 2 min atrás + reconciliação por horário) | **0,9** | **4,8** | **1,5** | **5,1** | **0,3** | 0 |
| P_mun_ua_sem (sem reconciliação) | 37,8 | 56,2 | 64,0 | 78,9 | 14,2 | 18% / 33% |
| P_mun_ca (cs 2 min atrás) | 37,8 | 56,2 | 64,0 | 73,9 | 14,2 | 19% / 33% |
| P_zona_ua (zonas nas grandes) | 0,7 | 4,0 | 5,1 | 5,1 | 0,3 | 0 |
| P_mun_ua_zero (prior zero) | 1,1 | 4,8 | 3,0 | 5,1 | 3,0 | 0 |

- **O ponto crítico é a sincronia cs × -u**, não o modelo. Sem reconciliação (ou com o cs atrasado), 18–33% dos municípios parciais ficam dessincronizados e o método cai para perto da versão mínima (erro < 0,5 só a partir de 38–64%).
- A reconciliação por horário é exata no replay (o horário do cs = a ordem de entrada no -u): **cenário otimista**. Ao vivo, o `ha` é o horário do arquivo auxiliar; a ordem deve ser próxima, mas não é garantida. E não resolve quando o cs está atrás.
- **Zonas**: não melhoram de forma consistente (1T: melhor em 2–5% e no fim; 2T: pior no início). Com o swing pela parte apurada já usando a base seção a seção, dividir em zonas acrescenta unidades pequenas e ruidosas. Mantido como diagnóstico, fora do método principal.
- **Prior**: só importa abaixo de ~2–5%. Em 2022 o prior zero foi melhor que o das pesquisas no começo (1T 0–2%: 1,23 × 1,89; 2T: 3,12 × 4,05), porque as pesquisas superestimaram Lula.
- **Faixa (P_mun_ua)**: cobertura ~1,0 entre 2% e 70% (larga) e 0–0,4 em 90–100% (estreita no fim). Calibração pendente.
- Gráficos: `figs/replay_metodos_t{1,2}_erro.png` (erro × % apurado, todos os métodos) e `figs/replay_metodos_t{1,2}_{M_B,P_mun_ua,P_zona_ua}.png`.

### Reconciliação cs × -u (replay `recon`, 5 min, 40 réplicas)
| método | 1T < 0,5 | 1T < 0,25 | 2T < 0,5 | 2T < 0,25 |
|---|---|---|---|---|
| sincronia perfeita | 0,7 | 4,8 | 0,7 | 1,5 |
| -u 2 min atrás, por horário | 0,9 | 4,8 | 1,5 | 5,1 |
| -u atrás, ordem embaralhada | 0,9 | 4,8 | 3,0 | 5,1 |
| **-u atrás, proporcional** | **0,9** | **4,8** | **3,0** | **5,1** |
| -u 4 min atrás, proporcional | 0,9 | 4,8 | 3,0 | 5,1 |
| **cs atrás, proporcional** | **0,7** | **15,7** | **5,1** | **5,1** |
| sem reconciliação (qualquer sentido) | 37,8 | 56,2 | 64,0 | 73,9–78,9 |

**Reconciliação proporcional** (sem usar ordem): n_u = seções no -u, n_c = seções com hora no cs, n = seções do município. Se n_c < n_u (cs atrás): seção com hora conta 1 e sem hora conta (n_u − n_c)/(n − n_c); se n_c > n_u (cs à frente): seção com hora conta n_u/n_c. A parte pendente usa a base × (1 − peso). Recupera praticamente todo o ganho nos dois sentidos, sem depender de o horário do cs reproduzir a ordem do -u. **Adotada** em `R/06_live.R`.
- Correção no ao vivo: a contagem de seções do cs vem do cs inteiro (junção pela esquerda). No simulado, parte das seções do cs não existe no arquivo de eleitorado 2026 e a junção interna criava dessincronias falsas (135 de 189). Com a correção: 0, 10 e 0 dessincronizados em 3 ciclos da rodada 3, o que confirma que o cs bate com o -u municipal no simulado.

### Calibração da faixa (`PAR_MOD$faixa_*`)
Faixa bruta do bootstrap (P_ua_prop + P_ca_prop, 1T e 2T): cobertura 76% em 0–2% apurado, ~100% entre 2% e 70% (larga), 79% / 40% / 15% em 70–90 / 90–97 / 97–100% (vai a zero, mas resta erro de 0,01–0,04 p.p.).
Ajuste: réplica = ponto + k(f)·(réplica − ponto) + N(0, piso/1,645), com k = 3 se f < 2% apurado, 0,7 depois, e piso = 0,04 p.p. Calibrado no 1T e avaliado no 2T, cobertura de 0,94; o inverso dá 0,87. Com dois turnos de dados, parâmetros arredondados para o lado conservador. Cobertura por faixa com o ajuste escolhido: 0,95 / 0,89 / 0,81 / 0,83 / 0,99 / 0,92 / 0,94 / 1,00.

### Método principal (congelado para a versão mínima)
Swing estimado pela parte apurada de cada município (`unidades = "apurado"`), base da parte pendente via cs com reconciliação proporcional, hierarquia Brasil → região → UF com prior das pesquisas, faixa calibrada. Unidade = município (as zonas ficam como diagnóstico). Fallback automático: sem cs no ciclo, variante A com `unidades = "completos"`.

## Página e trajetória projetada (28/09, noite)
- Código da noite reorganizado: `R/` (apuracao, ingestao, http, modelo, pagina, pesquisas, replay_2022), `preparo/` (bases), `estudos/` (comparações). README na raiz.
- Página estática em `site/` (HTML + CSS + D3), lê `site/dados/apuracao.json` (escrito por `R/pagina.R` a cada ciclo, com troca atômica). Cartões com projeção final, faixa e apurado; gráfico principal (Lula × PL) e painel de Outros com o mesmo eixo x; tabela; modo claro/escuro; cruz com dica.
- **Trajetória projetada** (`trajetoria()` em `R/modelo.R`): a parte pendente de cada UF chega no ritmo recente dela (fração do eleitorado da UF apurada por minuto nos últimos 15 min; `ritmo_uf()`); municípios pendentes da UF avançam juntos; sem ritmo medido, todas as UFs no mesmo passo. Dá o caminho do % apurado até 100%, que termina na projeção final. A página desenha o caminho pontilhado com a faixa abrindo até a faixa final. O ponto final é robusto; o formato do caminho depende da ordem suposta. Replay 2022 1T, aos 27,5% apurado: virada prevista em ~58% (real: ~67%).
- `R/replay_2022.R`: replay "como se fosse a eleição", montando com dados de 2022 os arquivos do TSE a cada instante e rodando o mesmo código da noite (base 2018 → seções de 2022: `preparo/04_base_replay2022.R`). Com o código de produção: 1T projeção a 2,5% apurado 48,72 / 42,96; a 8,6%, 48,47 / 43,15 (real 48,43 / 43,20).
