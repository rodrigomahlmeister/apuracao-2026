# Testes de regressão do pareamento (Fase 2) e da seleção incremental da ingestão.
raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old <- setwd(raiz); source("preparo/02_pareamento.R"); setwd(old)

loc <- function(uf, mun, zona, local, lat, lon, aptos, cep = NA_integer_)
  data.table(uf = uf, mun = mun, zona = zona, local = local, cep = cep, lat = lat, lon = lon, aptos = aptos)

test_that("suavização é aplicada em contagens, não em taxas por apto (bug 1)", {
  # base por apto: 80% válidos; PT 40%, PL 40%, OUTROS 20% dos válidos; swing zero
  for (aptos in c(50, 1000)) {
    s <- projetar_shares(0.32, 0.32, 0.16, aptos = aptos, sw1 = 0, sw2 = 0, turno = 1)
    expect_equal(s$PT, 0.40, tolerance = 0.01)
    expect_equal(s$PL, 0.40, tolerance = 0.01)
  }
  s2 <- projetar_shares(0.30, 0.50, 0, aptos = 500, sw1 = 0, turno = 2)
  expect_equal(s2$PT, 0.375, tolerance = 0.005)
})

test_that("marcação de semi-novo é gravada e gera componente de vizinhos (bug 2)", {
  B <- rbind(loc("SP", 1L, 1L, 100L, -23.5000, -46.6000, 300),
             loc("SP", 1L, 1L, 101L, -23.5010, -46.6000, 400),
             loc("SP", 1L, 1L, 102L, -23.5020, -46.6000, 500))
  T <- loc("SP", 1L, 1L, 100L, -23.5000, -46.6000, 900)           # mesma chave, triplicou
  r <- parear_mun(prep(T), prep(B)[, b_id := .I])
  expect_equal(r$n$nivel, "1")
  expect_true(r$n$semi)
  expect_true(any(r$v$comp == "s"))
  # sem mudança de tamanho: não é semi-novo
  r2 <- parear_mun(prep(loc("SP", 1L, 1L, 100L, -23.5000, -46.6000, 320)), prep(B)[, b_id := .I])
  expect_false(r2$n$semi)
})

test_that("pesos de município novo ficam alinhados com os demais (bug 3)", {
  base <- rbind(loc("MT", 10L, 1L, 1L, -12.50, -55.70, 1000), loc("MT", 10L, 1L, 2L, -12.51, -55.71, 800))
  alvo <- rbind(loc("MT", 10L, 1L, 1L, -12.50, -55.70, 1000),     # município existente
                loc("MT", 99L, 5L, 7L, -12.60, -55.75, 600))      # município novo (sem base), ~12 km
  par <- parear(alvo, base)
  expect_equal(names(par$pesos)[1:4], c("t_id", "b_id", "v", "comp"))
  expect_true(all(par$pesos$b_id %in% par$base$b_id))
  expect_true(all(is.finite(par$pesos$v) & par$pesos$v > 0))
  chk <- merge(par$pesos, par$base[, .(b_id, aptos)], by = "b_id")[, .(s = sum(v * aptos)), by = t_id]
  expect_equal(chk$s, rep(1, nrow(chk)), tolerance = 1e-9)
  expect_equal(par$nivel[mun == 99L, nivel], "5n")
})

test_that("nível 2 com coordenadas exige distância, não aceita só CEP", {
  B <- loc("SP", 1L, 1L, 100L, -23.5000, -46.6000, 500, cep = 1310100L)
  T <- loc("SP", 1L, 2L, 555L, -23.5100, -46.6000, 500, cep = 1310100L)   # mesmo CEP, ~1,1 km
  r <- parear_mun(prep(T), prep(B)[, b_id := .I])
  expect_false(identical(r$n$nivel, "2"))
})

test_that("seleção incremental baixa só municípios com andamento novo", {
  old <- setwd(raiz); source("R/ingestao.R"); setwd(old)
  atual  <- data.table(mun = c("01", "02", "03"), assin = c("a", "b", "c"))
  ultimo <- data.table(mun = c("01", "02"), assin = c("a", "x"))
  expect_setequal(selecionar_mun(atual, ultimo), c("02", "03"))
  expect_setequal(selecionar_mun(atual, ultimo, completo = TRUE), c("01", "02", "03"))
  expect_setequal(selecionar_mun(atual, ultimo[0]), c("01", "02", "03"))
})

test_that("ponte mantém numeração do ano do meio mesmo com local de eleitorado zero", {
  base <- rbind(loc("BA", 5L, 1L, 10L, -12.10, -38.40, 800), loc("BA", 5L, 1L, 11L, -12.11, -38.41, 700))
  meio <- rbind(loc("BA", 5L, 1L, 0L, NA, NA, 0),                    # local vazio antes dos demais
                loc("BA", 5L, 1L, 10L, -12.10, -38.40, 820), loc("BA", 5L, 1L, 11L, -12.11, -38.41, 690))
  alvo <- loc("BA", 5L, 1L, 11L, -12.11, -38.41, 700)
  pt <- ponte(alvo, meio, base)
  chk <- merge(pt$pesos, pt$base[, .(b_id, aptos)], by = "b_id")[, .(s = sum(v * aptos)), by = t_id]
  expect_equal(chk$s, 1, tolerance = 1e-9)
  expect_equal(pt$base[b_id %in% pt$pesos$b_id, local], 11L)
})

test_that("imputação de coordenadas exige mesma chave e mesmo CEP", {
  base <- rbind(loc("BA", 5L, 1L, 10L, NA, NA, 800, cep = 48000000L), loc("BA", 5L, 1L, 11L, NA, NA, 700, cep = 48000000L))
  fonte <- rbind(loc("BA", 5L, 1L, 10L, -12.1, -38.4, 0, cep = 48000000L),   # mesmo CEP: imputa
                 loc("BA", 5L, 1L, 11L, -12.2, -38.5, 0, cep = 48999000L))   # CEP diferente: não imputa
  r <- imputar_coord(base, list(fonte))
  expect_equal(r[local == 10L, lat], -12.1)
  expect_true(is.na(r[local == 11L, lat]))
  expect_equal(sum(r$coord_imputada), 1L)
})
