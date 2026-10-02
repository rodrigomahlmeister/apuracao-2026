# Testes do modelo de projeção (R/modelo.R) com dados sintéticos de swing conhecido.
raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old <- setwd(raiz); source("R/modelo.R"); setwd(old)

sintetico <- function(n = 120, n_compl = 80, delta = c(0.3, -0.2), seed = 1) {
  set.seed(seed)
  aptos <- round(runif(n, 5e3, 5e4))
  sPT <- runif(n, 0.2, 0.6); sPL <- runif(n, 0.2, 0.6) * (1 - sPT); sOU <- 1 - sPT - sPL
  val <- aptos * 0.8 * 0.9
  m <- data.table(uf = rep(c("AA", "BB", "CC"), length.out = n), mun = seq_len(n),
                  regiao = rep(c("R1", "R1", "R2"), length.out = n), aptos = aptos, elegivel = TRUE,
                  bt_comp = aptos * 0.8, bt_val = val, bt_PT = val * sPT, bt_PL = val * sPL, bt_OU = val * sOU)
  # resultado verdadeiro = base + swing uniforme em razão log (mesmo comparecimento e taxa de válidos)
  e1 <- log(sPT / sOU) + delta[1]; e2 <- log(sPL / sOU) + delta[2]; den <- 1 + exp(e1) + exp(e2)
  m[, `:=`(v_PT = val * exp(e1) / den, v_PL = val * exp(e2) / den, v_OU = val / den)]
  comp <- seq_len(n) <= n_compl
  m[, completo := comp]
  m[, `:=`(aptos_obs = fifelse(completo, aptos, 0), obs_comp = fifelse(completo, bt_comp, 0),
           obs_val = fifelse(completo, bt_val, 0), obs_PT = fifelse(completo, v_PT, 0),
           obs_PL = fifelse(completo, v_PL, 0), obs_OU = fifelse(completo, v_OU, 0))]
  m[, `:=`(bp_comp = fifelse(completo, 0, bt_comp), bp_val = fifelse(completo, 0, bt_val),
           bp_PT = fifelse(completo, 0, bt_PT), bp_PL = fifelse(completo, 0, bt_PL), bp_OU = fifelse(completo, 0, bt_OU))]
  m
}

test_that("swing uniforme conhecido é recuperado e a projeção acerta o total", {
  m <- sintetico()
  centro <- centro_nacional(m, 1)
  prior <- list(mu = c(0, 0), sd = c(10, 10))                # prior vago
  est <- estimar(m, 1, prior, centro)
  expect_equal(unname(est$coefs[[1]]["AA", 1]), 0.3, tolerance = 0.02)
  expect_equal(unname(est$coefs[[2]]["CC", 1]), -0.2, tolerance = 0.02)
  pj <- projetar(m, est, centro)
  real <- m[, c(sum(v_PT), sum(v_PL)) / sum(bt_val)]
  expect_equal(unname(pj[c("PT", "PL")]), real, tolerance = 1e-3)
})

test_that("sem nenhum município completo, a projeção segue o prior", {
  m <- sintetico(n_compl = 0)
  centro <- centro_nacional(m, 1)
  est <- estimar(m, 1, list(mu = c(0.3, -0.2), sd = c(0.05, 0.05)), centro)
  pj <- projetar(m, est, centro)
  real <- m[, c(sum(v_PT), sum(v_PL)) / sum(bt_val)]
  expect_equal(unname(pj[c("PT", "PL")]), real, tolerance = 1e-3)
})

test_that("estimação pela parte apurada usa a base da parte apurada (bt - bp)", {
  m <- sintetico(n_compl = 0)
  # metade de cada município apurada: observado = metade do resultado verdadeiro; base pendente = metade
  m[, `:=`(completo = FALSE, aptos_obs = aptos / 2, obs_comp = bt_comp / 2, obs_val = bt_val / 2,
           obs_PT = v_PT / 2, obs_PL = v_PL / 2, obs_OU = v_OU / 2,
           bp_comp = bt_comp / 2, bp_val = bt_val / 2, bp_PT = bt_PT / 2, bp_PL = bt_PL / 2, bp_OU = bt_OU / 2)]
  centro <- centro_nacional(m, 1)
  est <- estimar(m, 1, list(mu = c(0, 0), sd = c(10, 10)), centro)
  pj <- projetar(m, est, centro)
  real <- m[, c(sum(v_PT), sum(v_PL)) / sum(bt_val)]
  expect_equal(unname(pj[c("PT", "PL")]), real, tolerance = 1e-3)
  expect_equal(est$n_unidades, nrow(m))
})

test_that("estado: agregada listada no cs usa a própria base; seção nova recebe a média da zona; escala consistente", {
  bl <- list(PT = 13, PL = 22)
  # município 1, zona 1: seções 1 e 2 (2 é agregada à 1) e 3; o cs lista 1, 2, 3 e a seção nova 4
  bsec <- data.table(uf = "AA", mun = 1L, zona = 1L, secao = c(1L, 2L, 3L), secao_cs = c(1L, 1L, 3L), eleitores = 100,
                     b_comp = 80, b_val = 70, b_PT = c(40, 10, 30), b_PL = c(20, 50, 30), b_OU = c(10, 10, 10))
  base <- list(sec = bsec, mun = data.table(uf = "AA", mun = 1L, regiao = "R1", elegivel = TRUE), turno = 1)
  cs <- data.table(uf = "aa", mun = "00001", zona = "0001", secao = sprintf("%04d", 1:4), nsp = NA_character_,
                   ha = c("x", "x", NA, NA))
  ut <- data.table(tpabr = "mu", cdabr = "00001", ts = 4L, st = 2L, te = 400, est = 200, c = 160)
  uc <- data.table(tpabr = "mu", cdabr = "00001", n = c("13", "22", "99"), vap = c(50, 70, 20), dvt = "Válido")
  m <- estado(list(u_tot = ut, u_cand = uc, cs = cs, rodada = 1L), base, bl)
  # pendentes: seção 3 (base própria) + seção 4 (média da zona = média das seções 1, 2, 3); escala te / soma do cs = 400 / 400
  expect_equal(m$bp_PT, 30 + (40 + 10 + 30) / 3)
  expect_equal(m$bt_PT, 40 + 10 + 30 + 80 / 3)
  expect_equal(m$bt_PT - m$bp_PT, 50)                 # base da parte apurada = seções 1 e 2, cada uma com a própria base
  expect_false(m$dessinc)
})
