# Testes rápidos antes da Fase 4 (2018 -> 2022):
#  A. nível 2 tratado como semi-novo (w x próprio + (1 - w) x vizinhos): deixa de perder para o município no 1T?
#  B. estimadores do swing dentro das 101 cidades grandes, com apuração parcial:
#     E1 % bruta | E2 swing uniforme | E3 regressão swing ~ eta_base (local) | E4 E3 com slopes encolhidos
#     ordens: aleatória (25%, 50%; 5 repetições) e "maior % PL na base primeiro" (centro antes da periferia)
source("preparo/02_pareamento.R")
set.seed(2026)

h18 <- rd("hist_local_2018.parquet"); h22 <- rd("hist_local_2022.parquet")
m18 <- rd("hist_mun_2018.parquet");   m22 <- rd("hist_mun_2022.parquet")
fontes <- list(rd("locais_2022.parquet"), rd("locais_2024.parquet"), rd("locais_2026.parquet"))

# tabela local-a-local de um turno (base por apto + resultado real + swing do município), dado um pareamento
montar <- function(par, tn, w_semi) {
  bp  <- base_projetada(par, h18[turno == tn], w_semi = w_semi)
  act <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], h22[turno == tn], by = c("uf", "mun", "zona", "local"))
  x <- merge(act[, .(t_id, uf, mun, PT, PL, OUTROS, validos, aptos)], bp[, .(t_id, bPT = PT, bPL = PL, bOU = OUTROS)], by = "t_id")
  sw <- merge(m18[turno == tn, .(uf, mun, PTb = PT, PLb = PL, OUb = OUTROS)],
              m22[turno == tn, .(uf, mun, PTa = PT, PLa = PL, OUa = OUTROS, Va = validos)], by = c("uf", "mun"))
  x <- merge(merge(x, sw, by = c("uf", "mun")), par$nivel[, .(t_id, nivel, semi)], by = "t_id")[validos > 0]
  x[, turno := tn]
  s <- if (tn == 2) projetar_shares(x$bPT, x$bPL, x$bOU, x$aptos, lr(x$PTa, x$PLa) - lr(x$PTb, x$PLb), turno = 2)
       else projetar_shares(x$bPT, x$bPL, x$bOU, x$aptos, lr(x$PTa, x$OUa) - lr(x$PTb, x$OUb), lr(x$PLa, x$OUa) - lr(x$PLb, x$OUb))
  x[, `:=`(e_PT = 100 * (s$PT - PT / validos), r_PT = 100 * (PTa / Va - PT / validos))]
}

# ---- A. nível 2 como semi-novo ---------------------------------------------------------------
cat("== A. nível 2 tratado como semi-novo (2018 -> 2022), erro PT em p.p. ==\n")
PAR$n2_semi <- TRUE
p22 <- parear(imputar_coord(locais_base(2022), fontes[2:3]), imputar_coord(locais_base(2018), fontes))
PAR$n2_semi <- FALSE
resA <- rbindlist(lapply(c(0, 0.25, 0.5, 0.75, 1), \(w) rbindlist(lapply(1:2, \(tn) {
  x <- montar(p22, tn, w)[nivel == "2"]
  data.table(turno = tn, w = w, locais = nrow(x), mae = metricas(x$e_PT, x$validos)$mae,
             rmse = metricas(x$e_PT, x$validos)$rmse, ref_mae = metricas(x$r_PT, x$validos)$mae)
}))))
print(dcast(resA, w ~ turno, value.var = c("mae", "ref_mae"))[, lapply(.SD, round, 2)])

# ---- B. estimadores dentro da cidade -----------------------------------------------------------
cat("\n== B. estimadores do swing nas 101 cidades grandes ==\n")
m <- rd("municipios_ibge.parquet"); e <- rd("locais_2026.parquet")[, .(el = sum(eleitores)), by = .(uf, mun)]
g <- merge(e, m, by = c("uf", "mun"))[capital | el > 2e5, .(uf, mun)]
ev <- as.data.table(read_parquet("dados/base/validacao_pareamento_2022_2018.parquet"))[g, on = .(uf, mun), nomatch = 0]

# eta da base e real por local; nomes: 1T -> (1 = PT/OUTROS, 2 = PL/OUTROS); 2T -> (1 = PT/PL)
prep_eta <- function(x) {
  x <- copy(x)
  x[, `:=`(BPT = bPT * aptos, BPL = bPL * aptos, BOU = bOU * aptos)]
  if (x$turno[1] == 1) {
    x[, `:=`(eb1 = lr(BPT, BOU), eb2 = lr(BPL, BOU), s1 = lr(PT, OUTROS) - lr(BPT, BOU), s2 = lr(PL, OUTROS) - lr(BPL, BOU))]
  } else {
    x[, `:=`(eb1 = lr(BPT, BPL), eb2 = 0, s1 = lr(PT, PL) - lr(BPT, BPL), s2 = 0)]
  }
  x[, pl_base := BPL / (BPT + BPL + BOU)]
}

# regressão ponderada de s ~ eb (1T: eb1 + eb2; 2T: eb1); devolve slopes
slopes <- function(s, X, w) {
  ok <- is.finite(s) & rowSums(!is.finite(X)) == 0
  if (sum(ok) < ncol(X) + 3) return(rep(0, ncol(X)))
  b <- tryCatch(coef(lm.wfit(cbind(1, X[ok, , drop = FALSE]), s[ok], w[ok]))[-1], error = \(e) rep(0, ncol(X)))
  b[is.na(b)] <- 0
  b
}

# projeção da cidade dada a seleção obs; devolve erro (p.p.) de PT e PL por estimador
avaliar_cidade <- function(x, tn, beta_pool, kappas) {
  O <- x[obs == TRUE]; R <- x[obs == FALSE]
  real <- c(PT = sum(x$PT), PL = sum(x$PL)) / sum(x$validos)
  cidade <- function(pPT, pPL) 100 * (c(PT = sum(O$PT) + sum(pPT * R$validos), PL = sum(O$PL) + sum(pPL * R$validos)) /
                                       sum(x$validos) - real)
  proj <- function(sw1, sw2) {
    if (tn == 1) projetar_shares(R$bPT, R$bPL, R$bOU, R$aptos, sw1, sw2)
    else projetar_shares(R$bPT, R$bPL, R$bOU, R$aptos, sw1, turno = 2)
  }
  out <- list()
  out$E1_bruto <- cidade(rep(sum(O$PT) / sum(O$validos), nrow(R)), rep(sum(O$PL) / sum(O$validos), nrow(R)))
  if (tn == 1) {
    u1 <- lr(sum(O$PT), sum(O$OUTROS)) - lr(sum(O$BPT), sum(O$BOU)); u2 <- lr(sum(O$PL), sum(O$OUTROS)) - lr(sum(O$BPL), sum(O$BOU))
  } else { u1 <- lr(sum(O$PT), sum(O$PL)) - lr(sum(O$BPT), sum(O$BPL)); u2 <- 0 }
  s <- proj(u1, u2); out$E2_uniforme <- cidade(s$PT, s$PL)
  cols <- if (tn == 1) c("eb1", "eb2") else "eb1"
  XO <- as.matrix(O[, ..cols]); XR <- as.matrix(R[, ..cols])
  pred_sw <- function(sname, b) {                   # intercepto: zera o resíduo médio (ponderado) nos apurados
    a <- weighted.mean(O[[sname]] - drop(XO %*% b), O$aptos)
    a + drop(XR %*% b)
  }
  b1 <- slopes(O$s1, XO, O$aptos); b2 <- if (tn == 1) slopes(O$s2, XO, O$aptos) else 0
  s <- proj(pred_sw("s1", b1), if (tn == 1) pred_sw("s2", b2) else 0); out$E3_regressao <- cidade(s$PT, s$PL)
  for (k in kappas) {
    w <- nrow(O) / (nrow(O) + k)
    c1 <- w * b1 + (1 - w) * beta_pool$b1; c2 <- if (tn == 1) w * b2 + (1 - w) * beta_pool$b2 else 0
    s <- proj(pred_sw("s1", c1), if (tn == 1) pred_sw("s2", c2) else 0)
    out[[paste0("E4_encolhido_k", k)]] <- cidade(s$PT, s$PL)
  }
  rbindlist(lapply(names(out), \(n) data.table(estimador = n, e_PT = out[[n]]["PT"], e_PL = out[[n]]["PL"])))
}

# slopes "de todas as cidades": regressão dentro das cidades (variáveis centradas por cidade) nos apurados
beta_agrupado <- function(X, tn) {
  O <- X[obs == TRUE]
  cols <- if (tn == 1) c("eb1", "eb2") else "eb1"
  O[, (paste0("c_", c(cols, "s1", "s2"))) := lapply(.SD, \(v) v - weighted.mean(v, aptos)), by = .(uf, mun), .SDcols = c(cols, "s1", "s2")]
  XO <- as.matrix(O[, paste0("c_", cols), with = FALSE])
  list(b1 = slopes(O$c_s1, XO, O$aptos), b2 = if (tn == 1) slopes(O$c_s2, XO, O$aptos) else 0)
}

rodar_cenario <- function(X, tn, ordem, f, rep) {
  if (ordem == "aleatoria") X[, obs := seq_len(.N) %in% sample(.N, ceiling(f * .N)), by = .(uf, mun)]
  else X[order(-pl_base), obs := seq_len(.N) <= ceiling(f * .N), by = .(uf, mun)]
  bp <- beta_agrupado(X, tn)
  r <- X[, avaliar_cidade(.SD, tn, bp, kappas = c(20, 200)), by = .(uf, mun)]
  r[, `:=`(turno = tn, ordem = ordem, f = f, rep = rep)]
}

resB <- rbindlist(lapply(1:2, \(tn) {
  X <- prep_eta(ev[turno == tn])
  rbindlist(c(
    lapply(c(0.25, 0.5), \(f) rbindlist(lapply(1:5, \(r) rodar_cenario(X, tn, "aleatoria", f, r)))),
    lapply(c(0.25, 0.5), \(f) rodar_cenario(X, tn, "PL_primeiro", f, 1))))
}))
tab <- resB[, .(mae_PT = mean(abs(e_PT)), p90_PT = quantile(abs(e_PT), 0.9),
                mae_PL = mean(abs(e_PL)), p90_PL = quantile(abs(e_PL), 0.9)), by = .(turno, ordem, f, estimador)]
tab <- tab[, lapply(.SD, \(v) if (is.double(v)) round(v, 2) else v)][order(turno, ordem, f, estimador)]
print(tab, nrows = 200)
fwrite(tab, "docs/fase2_teste_swing_cidade.csv")
fwrite(resA, "docs/fase2_nivel2_semi.csv")
