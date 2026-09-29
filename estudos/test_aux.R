# parse_aux: formato do simulado 2026 (arq vazio) e do oficial 2024 (arq como objetos {nm, tp})
raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old <- setwd(raiz); source("estudos/03_ingestao.R"); setwd(old)

test_that("aux oficial 2024: arquivos como objetos nm/tp e horário de recebimento", {
  txt <- '{ "dg" : "06/10/2024", "hg" : "18:21:49", "f" : "O", "st" : "Totalizada", "hashes" : [
  { "hash" : "4770", "dr" : "06/10/2024", "hr" : "17:50:48", "st" : "Totalizado", "arq" : [
  { "nm" : "o00452ac0106600040077-bu.dat", "tp" : "bu" }, { "nm" : "o00452ac0106600040077-imgbu.dat", "tp" : "imgbu" } ] } ] }'
  a <- parse_aux(txt)
  expect_equal(a$st, "Totalizada")
  expect_equal(a$arq$arq, c("o00452ac0106600040077-bu.dat", "o00452ac0106600040077-imgbu.dat"))
  expect_equal(a$arq$tp, c("bu", "imgbu"))
  expect_equal(a$arq$hr[1], "17:50:48")
})

test_that("aux do simulado 2026: sem arquivos", {
  a <- parse_aux('{ "dg" : "24/09/2026", "hg" : "16:19:30", "idg" : "1", "f" : "s", "st" : "Totalizada", "hashes" : [ { "arq" : [ ] } ] }')
  expect_equal(a$st, "Totalizada")
  expect_equal(nrow(a$arq), 0)
})
