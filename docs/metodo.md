# Projeção do resultado de Presidente durante a apuração — método

Rodrigo Mahlmeister · código: [github.com/rodrigomahlmeister/apuracao-2026](https://github.com/rodrigomahlmeister/apuracao-2026)

A projeção combina o que já foi apurado com uma estimativa, seção a seção, do que falta apurar. A estimativa parte do
resultado da eleição anterior nos mesmos locais de votação e o corrige pela mudança observada, até aquele momento, nas
seções já totalizadas. Validação: reprodução da apuração de 2022 na ordem real de chegada dos boletins de urna.

## 1. Dados de entrada (divulgação do TSE)

A cada ciclo (~1 min) são lidos os arquivos públicos da divulgação de resultados, conforme as especificações do TSE para 2026:

| arquivo | conteúdo usado |
|---|---|
| EA11 configuração de eleições | códigos da eleição e do pleito; diretórios dos demais arquivos |
| EA14/EA15 acompanhamento | seções totalizadas, eleitorado e comparecimento por UF e por município |
| EA16 configuração de seções | lista de seções por município e zona; `da`/`ha` (data/hora do arquivo auxiliar) marcam as seções já totalizadas |
| EA20 resultado unificado | votos por candidato em cada município, com destinação do voto |

Só entram nos votos válidos os candidatos com destinação válida (candidaturas anuladas ou *sub judice* ficam de fora).
O resultado municipal só é baixado de novo quando o andamento do município mudou (mais uma fatia rotativa de 1/5 dos
municípios por ciclo); requisições condicionais (ETag) evitam transferir arquivos inalterados.

## 2. Base: a eleição anterior nos locais de votação atuais

Para cada seção de 2026, a base é o resultado de 2022 do mesmo local de votação (comparecimento, válidos e votos por bloco:
candidato do PT, candidato do PL, demais). Os locais de 2026 são pareados aos de 2022 por uma cascata, sem usar o nome do local:

1. mesma chave (município, zona, número do local), confirmada por CEP igual ou distância < 200 m;
2. mesmo prédio sob outra chave: distância < 100 m (ou, sem coordenadas, CEP de 8 dígitos exclusivo daquele local nos dois anos);
3. vizinhos: média dos 5 locais de 2022 mais próximos (raio de 3 km), ponderada por eleitorado / distância;
4. zona eleitoral de 2022;
5. município de 2022.

Locais cujo eleitorado mudou mais de 50% e locais do nível 2 recebem base mista (histórico próprio e vizinhos, pesos
calibrados em 2018 → 2022). Coordenadas ausentes na base são herdadas do mesmo local (mesma chave e CEP) nos cadastros
seguintes. Em 2026, 93% do eleitorado tem base de nível 1 e 0,9% cai nos níveis 4–5.

## 3. Estado de cada município

Para o município *m* no instante *t*: votos apurados por bloco; seções totalizadas segundo o EA16; base da parte apurada
(soma da base dessas seções) e da parte pendente. Quando o EA16 e o resultado municipal não estão sincronizados
(nº de seções com horário *n_c* ≠ nº de seções totalizadas no resultado *n_u*), a reconciliação é proporcional:
se *n_c* < *n_u*, cada seção sem horário conta como apurada com peso (*n_u* − *n_c*)/(*n* − *n_c*); se *n_c* > *n_u*,
cada seção com horário conta com peso *n_u*/*n_c*.

## 4. Modelo do swing

Para cada município com parte apurada, o swing é a diferença entre as razões log do apurado e da base da parte apurada
(1º turno: η₁ = log(PT/Outros), η₂ = log(PL/Outros); 2º turno: η = log(PT/PL)):

  swingₖ(m) = ηₖ(apurado) − ηₖ(base da parte apurada)

O swing é modelado por regressão linear ponderada pelo eleitorado apurado:

  swingₖ = aₖ + bₖ·η₁,base + cₖ·η₂,base + dₖ·log(eleitorado) + ε

com covariáveis centradas na média nacional. Os coeficientes têm estrutura hierárquica: são estimados para o Brasil,
para cada região e para cada UF, e cada nível é encolhido para o nível acima com peso *n* / (*n* + κ) (κ = 30 municípios).
O intercepto nacional é combinado por precisão com um prior: a média das pesquisas finais (em razão log, desvio de
4 p.p. por bloco) ou, sem pesquisas, swing nulo. Municípios com menos de 90% do eleitorado em base de nível 1 são
projetados, mas não entram na estimação. O exterior usa os coeficientes nacionais.

Parte pendente de cada município: base × razão de comparecimento × razão de taxa de válidos (apurado/base na UF,
encolhidas para o Brasil) × proporções projetadas, obtidas aplicando o swing previsto pelo perfil **das seções que
faltam** e voltando da razão log para proporções (softmax). Resultado projetado = apurado + pendente projetado.

## 5. Incerteza

Faixa de 90% por bootstrap: 100 réplicas com pesos de Poisson por município (equivalente a reamostrar municípios dentro
da UF) e sorteio do intercepto nacional pela distribuição posterior. A largura foi calibrada na reprodução de 2022:
as réplicas são reescaladas por k = 3 antes de 2% do eleitorado apurado e k = 0,7 depois, com um piso de 0,04 p.p.
(cobertura de 87–94% na validação cruzada entre os dois turnos). A probabilidade de 2º turno é a fração das réplicas
em que nenhum bloco passa de 50% dos válidos. A projeção só é exibida a partir de 2% do eleitorado apurado.

## 6. Trajetória projetada

A linha pontilhada mostra o caminho esperado do % apurado até 100%: a parte pendente de cada UF chega no ritmo de
apuração observado nela nos últimos 15 minutos (UF atrasada termina depois), e os municípios pendentes da UF avançam
juntos. O ponto final coincide com a projeção; o formato do caminho depende da ordem suposta e é menos certo que ele.

## 7. Projeção por estado e pesquisas estaduais

A projeção de cada UF é o apurado da UF mais a parte pendente projetada dos seus municípios (mesmo modelo da seção 4,
sem estimação adicional). A página compara essa projeção com a média simples das últimas pesquisas estaduais
(`config/pesquisas_uf.csv`, convertidas para votos válidos). A célula recebe a cor do candidato quando ele supera a
pesquisa, com intensidade crescente de 1 a 5 p.p. UFs com menos de 2% do próprio eleitorado apurado aparecem sem cor.
No replay de 2022, com 31% do eleitorado nacional apurado, o erro absoluto médio da projeção por UF foi de 0,6 p.p.
(PT e PL), com 5 UFs acima de 1 p.p. Diferenças de 1–2 p.p. para a pesquisa estão, portanto, dentro do erro da projeção
no começo da noite.

## 8. Validação: reprodução de 2022

A apuração de 2022 foi reproduzida a cada 5 minutos na ordem real de recebimento dos boletins, com a base de 2018 levada
aos locais de 2022 e o arquivo de seções atrasado em relação ao resultado municipal, como observado no simulado de 2026.
Erro absoluto médio (p.p.) a partir do qual a projeção fica sempre abaixo do limite:

| | erro < 0,5 (1º turno) | erro < 0,5 (2º turno) | líder certo (2º turno) |
|---|---|---|---|
| % apurado bruto | 97,6% apurado | 87,7% | 68,6% |
| extrapolação por UF | 92,1% | 78,9% | 48,2% |
| **este método** | **0,9%** | **3,0%** | **0,3%** |

No 1º turno de 2022, com 8,6% do eleitorado apurado, a projeção era 48,47% (PT) e 43,15% (PL), para um resultado final
de 48,43% e 43,20%. Detalhes, testes intermediários e alternativas descartadas: `docs/notas_tecnicas.md`.
