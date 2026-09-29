# Testes dos parsers da divulgação com amostras reais do simulado 2026 (dados_tse/amostras_simulado).
# Rodar da raiz: Rscript -e 'testthat::test_dir("tests/testthat")'

raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old <- setwd(raiz); source("R/ingestao.R"); setwd(old)
am <- file.path(raiz, "dados_tse", "amostras_simulado")
rt <- function(f) paste(readLines(file.path(am, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("ele-c: eleição federal 1T com Presidente e diretórios dos arquivos usados", {
  ec <- parse_elec(rt("ele-c_simulado.json"))
  el <- ec$eleicoes[cargo == "1" & turno == "1" & abr == "br"]
  expect_equal(nrow(el), 1)
  expect_equal(el$eleicao, "21270")
  expect_equal(el$pleito, "17801")
  expect_equal(el$cdt2, "21271")
  expect_true(all(c("u", "ab", "cs", "cm") %in% ec$dirs$tp))
})

test_that("cm: 5.755 municípios com código de 5 dígitos", {
  cm <- parse_cm(rt("mun-e021270-cm.json"))
  expect_equal(nrow(cm), 5755)
  expect_true(all(nchar(cm$mun) == 5))
  expect_equal(cm[uf == "sp" & mun == "71072", nm], "SÃO PAULO")
})

test_that("cs: uma linha por seção com data/hora", {
  cs <- parse_cs(rt("ac-p017801-cs.json"))
  expect_true(nrow(cs) >= 3006)
  expect_false(anyNA(cs$secao))
  expect_true(all(c("ha", "nsp") %in% names(cs)))
})

test_that("ab: BR tem 27 UFs + ZZ, campos numéricos", {
  ab <- parse_ab(rt("br_ab.json"))
  expect_true(all(c("ac", "sp", "zz") %in% ab$cdabr))
  expect_type(ab$est, "double")
  expect_true(all(ab$st <= ab$ts))
})

test_that("u: totais batem com soma dos candidatos", {
  u <- parse_u(rt("br_u.json"))
  expect_equal(nrow(u$tot), 1)
  expect_gt(nrow(u$cand), 1)
  expect_type(u$cand$vap, "double")
  # nominais válidos = soma dos votos dos candidatos com voto válido
  expect_equal(sum(u$cand[dvt == "Válido", vap]), u$tot$vv)
})

test_that("válidos por bloco usam só candidatos com destinação válida", {
  old <- setwd(raiz); source("R/modelo.R"); setwd(old)
  u <- parse_u(rt("br_u.json"))
  expect_true(any(!e_valido(u$cand$dvt)))                         # a amostra tem anulado e anulado sub judice
  vb <- votos_blocos_mun(u$cand, list(PT = 89, PL = 68))
  expect_equal(vb$PT + vb$PL + vb$OU, u$tot$vv)                   # soma dos blocos = válidos do arquivo
  expect_lt(vb$PT + vb$PL + vb$OU, sum(u$cand$vap))               # somar todos os vap superestimaria
})
