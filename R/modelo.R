# Projeção do resultado final de Presidente durante a apuração.
#
# Cada município tem uma parte apurada (votos reais) e uma pendente. A base de cada seção é o resultado de 2022
# levado aos locais de votação de 2026 (preparo/03_base_2026.R). O arquivo de seções do TSE diz quais seções já
# foram totalizadas; com isso a base da parte apurada e a da pendente são conhecidas seção a seção.
#
# Swing (mudança em razão log desde 2022; 1º turno: PT/OUTROS e PL/OUTROS; 2º turno: PT/PL), estimado na parte
# apurada de cada município: swing ~ eta da base + log(eleitorado), regressão ponderada; hierarquia
# Brasil -> região -> UF, cada nível encolhido para o de cima com peso n / (n + kappa); intercepto nacional com
# prior (pesquisas, se houver; senão swing zero). Parte pendente = base x comparecimento x taxa de válidos x
# proporções projetadas. Faixa de 90%: bootstrap dos municípios + calibração no replay de 2022.
suppressPackageStartupMessages({ library(data.table); library(arrow); library(yaml) })

PAR_MOD <- list(kappa_reg = 30, kappa_uf = 30, kappa_taxa = 20, min_n = 8, B = 100,
                faixa_f_ini = 0.02, faixa_k_ini = 3, faixa_k = 0.7, faixa_piso = 4e-4,
                prior_dp = 0.04, mostrar_a_partir = 0.02)

sm <- function(x) x + 0.5
lr <- function(a, b) log(sm(a) / sm(b))
e_valido <- function(dvt) !is.na(dvt) & startsWith(dvt, "Válido")    # "Anulado"/"Anulado sub judice" fora

# votos por bloco em cada abrangência (cdabr), só de candidatos com destinação válida
votos_blocos_mun <- function(uc, blocos) {
  pt <- as.character(blocos$PT); pl <- as.character(blocos$PL)
  uc[, .(PT = sum(vap[e_valido(dvt) & n %in% pt]), PL = sum(vap[e_valido(dvt) & n %in% pl]),
         OU = sum(vap[e_valido(dvt) & !n %in% c(pt, pl)])), by = .(mun = cdabr)]
}

# ---- base ---------------------------------------------------------------------------------------------------
carregar_base <- function(turno, secoes = "base_2026_secao", municipios = "municipios_2026_modelo") {
  sec <- as.data.table(read_parquet(sprintf("dados/base/%s_t%d.parquet", secoes, turno)))
  mun <- as.data.table(read_parquet(sprintf("dados/base/%s.parquet", municipios)))
  list(sec = sec, mun = mun, turno = turno)
}

# ---- prior do swing nacional ---------------------------------------------------------------------------------
# Pesquisas (config/prior.yaml, gerado por R/pesquisas.R) ou swing zero. dp em proporção -> razão log (delta).
prior_swing <- function(base, arq = "config/prior.yaml") {
  b <- base$sec[, .(PT = sum(b_PT), PL = sum(b_PL), OU = sum(b_OU))]
  tem <- file.exists(arq)
  m <- if (tem) read_yaml(arq)$media_validos else { v <- sum(unlist(b)); list(PT = b$PT / v, PL = b$PL / v, OUTROS = b$OU / v) }
  dp <- PAR_MOD$prior_dp
  if (base$turno == 1) {
    mu <- c(log(m$PT / m$OUTROS) - log(b$PT / b$OU), log(m$PL / m$OUTROS) - log(b$PL / b$OU))
    sd <- c(sqrt((dp / m$PT)^2 + (dp / m$OUTROS)^2), sqrt((dp / m$PL)^2 + (dp / m$OUTROS)^2))
  } else {
    mu <- log(m$PT / m$PL) - log(b$PT / b$PL); sd <- sqrt((dp / m$PT)^2 + (dp / m$PL)^2)
  }
  list(mu = mu, sd = sd, fonte = if (tem) "pesquisas" else "swing zero")
}

# ---- estado dos municípios num ciclo ---------------------------------------------------------------------------
# Votos por bloco só de candidatos com destinação válida. Seções apuradas segundo o arquivo de seções,
# reconciliadas com o resultado municipal: n_u = seções totalizadas no -u; n_c = seções com hora no cs;
# n = seções do município. Se n_c < n_u, seção com hora conta 1 e sem hora (n_u - n_c)/(n - n_c);
# se n_c > n_u, seção com hora conta n_u/n_c. A parte pendente é a base x (1 - peso).
estado <- function(ciclo, base, blocos) {
  ut <- ciclo$u_tot[tpabr == "mu"]; uc <- ciclo$u_cand[tpabr == "mu"]
  vb <- votos_blocos_mun(uc, blocos)
  m <- merge(ut[, .(mun = as.integer(cdabr), ts, st, te, est, c)], vb[, mun := as.integer(mun)], by = "mun", all.x = TRUE)
  bsec <- base$sec
  bt <- bsec[, .(uf = uf[1], aptos_base = sum(eleitores), bt_comp = sum(b_comp), bt_val = sum(b_val),
                 bt_PT = sum(b_PT), bt_PL = sum(b_PL), bt_OU = sum(b_OU)), by = mun]
  m <- merge(m, bt, by = "mun")
  m[, `:=`(aptos = as.numeric(te), aptos_obs = as.numeric(est), completo = st >= ts, obs_comp = fcoalesce(as.numeric(c), 0),
           obs_PT = fcoalesce(as.numeric(PT), 0), obs_PL = fcoalesce(as.numeric(PL), 0), obs_OU = fcoalesce(as.numeric(OU), 0))]
  m[, obs_val := obs_PT + obs_PL + obs_OU]
  f <- m$aptos / m$aptos_base                                   # base na escala do eleitorado da divulgação
  for (k in c("comp", "val", "PT", "PL", "OU")) set(m, j = paste0("bt_", k), value = m[[paste0("bt_", k)]] * f)

  cs <- ciclo$cs
  if (!is.null(cs) && nrow(cs)) {
    csp <- unique(cs[is.na(nsp), .(uf = toupper(uf), mun = as.integer(mun), zona = as.integer(zona),
                                  secao_cs = as.integer(secao), tem = !is.na(ha))])
    x <- merge(csp, bsec, by = c("uf", "mun", "zona", "secao_cs"), all.x = TRUE)
    bc <- c("b_comp", "b_val", "b_PT", "b_PL", "b_OU")
    x[, (bc) := lapply(.SD, as.numeric), .SDcols = bc]
    x[, (bc) := lapply(.SD, \(v) fcoalesce(v, mean(v, na.rm = TRUE))), by = mun, .SDcols = bc]
    x <- merge(x[!is.na(b_comp)], m[, .(mun, n_u = st)], by = "mun")
    x[, `:=`(n_c = sum(tem), n = .N), by = mun]
    x[, w := fcase(n_c == n_u, as.numeric(tem), n_c < n_u, fifelse(tem, 1, (n_u - n_c) / pmax(n - n_c, 1)),
                   default = fifelse(tem, n_u / n_c, 0))]
    pb <- x[, .(bp_comp = sum(b_comp * (1 - w)), bp_val = sum(b_val * (1 - w)), bp_PT = sum(b_PT * (1 - w)),
                bp_PL = sum(b_PL * (1 - w)), bp_OU = sum(b_OU * (1 - w)), dessinc = n_c[1] != n_u[1]), by = mun]
    m <- merge(m, pb, by = "mun", all.x = TRUE)
    for (k in c("comp", "val", "PT", "PL", "OU")) {
      col <- paste0("bp_", k); set(m, j = col, value = fcoalesce(m[[col]], 0) * m$aptos / m$aptos_base * fifelse(m$completo, 0, 1))
    }
  } else {                                                       # sem arquivo de seções: pendente proporcional
    g <- pmax(m$aptos - m$aptos_obs, 0) / m$aptos
    for (k in c("comp", "val", "PT", "PL", "OU")) set(m, j = paste0("bp_", k), value = m[[paste0("bt_", k)]] * g)
    m[, dessinc := NA]
  }
  merge(m, base$mun, by = c("uf", "mun"), all.x = TRUE)[is.na(elegivel), elegivel := FALSE][is.na(regiao), regiao := "ZZ"]
}

# ---- estimação e projeção ---------------------------------------------------------------------------------------
eta_base <- function(PT, PL, OU, turno) if (turno == 1) cbind(e1 = lr(PT, OU), e2 = lr(PL, OU)) else cbind(e1 = lr(PT, PL))
centro_nacional <- function(m, turno) {
  E <- eta_base(m$bt_PT, m$bt_PL, m$bt_OU, turno)
  c(apply(E, 2, weighted.mean, w = m$aptos), le = weighted.mean(log(m$aptos), m$aptos))
}
X_de <- function(PT, PL, OU, aptos, turno, centro) {
  X <- cbind(eta_base(PT, PL, OU, turno), le = log(aptos)); sweep(X, 2, centro[colnames(X)])
}
wls <- function(y, X, w) {
  ok <- is.finite(y) & rowSums(!is.finite(X)) == 0 & w > 0
  y <- y[ok]; X <- X[ok, , drop = FALSE]; w <- w[ok]
  if (length(y) <= ncol(X) + 2) return(NULL)
  f <- lm.wfit(cbind(1, X), y, w); b <- coef(f); b[is.na(b)] <- 0
  r <- y - drop(cbind(1, X) %*% b)
  list(b = b, s2 = sum(w * r^2) / sum(w), n = length(y), sw = sum(w), sw2 = sum(w^2))
}

estimar <- function(m, turno, prior, centro, peso = NULL, sortear_prior = FALSE, par = PAR_MOD) {
  m[, pb := if (is.null(peso)) 1 else peso]
  C <- m[elegivel == TRUE & obs_val > 0 & aptos_obs > 0 & pb > 0]
  C[, `:=`(bo_PT = bt_PT - bp_PT, bo_PL = bt_PL - bp_PL, bo_OU = bt_OU - bp_OU, bo_comp = bt_comp - bp_comp,
           bo_val = bt_val - bp_val, wt = aptos_obs * pb)]
  C <- C[bo_comp > 0]
  X <- X_de(C$bo_PT, C$bo_PL, C$bo_OU, C$aptos, turno, centro)
  Y <- if (turno == 1) cbind(lr(C$obs_PT, C$obs_OU) - lr(C$bo_PT, C$bo_OU), lr(C$obs_PL, C$obs_OU) - lr(C$bo_PL, C$bo_OU))
       else cbind(lr(C$obs_PT, C$obs_PL) - lr(C$bo_PT, C$bo_PL))
  np <- ncol(X) + 1
  regs <- setdiff(unique(m$regiao), "ZZ"); ufs <- unique(m[uf != "ZZ", .(uf, regiao)])
  coefs <- lapply(seq_len(ncol(Y)), \(k) {
    f <- if (nrow(C) >= par$min_n) wls(Y[, k], X, C$wt)
    if (is.null(f)) { bn <- c(prior$mu[k], rep(0, np - 1)); v_int <- prior$sd[k]^2 }
    else {
      v_dado <- max(f$s2 * f$sw2 / f$sw^2, 1e-8)
      v_int <- 1 / (1 / v_dado + 1 / prior$sd[k]^2)
      bn <- f$b; bn[1] <- v_int * (f$b[1] / v_dado + prior$mu[k] / prior$sd[k]^2)
    }
    if (sortear_prior) bn[1] <- bn[1] + rnorm(1, 0, sqrt(v_int))
    encolhe <- function(idx, alvo, kappa) {
      if (sum(idx) < par$min_n) return(alvo)
      f <- wls(Y[idx, k], X[idx, , drop = FALSE], C$wt[idx]); if (is.null(f)) return(alvo)
      w <- f$n / (f$n + kappa); w * f$b + (1 - w) * alvo
    }
    br <- setNames(lapply(regs, \(r) encolhe(C$regiao == r, bn, par$kappa_reg)), regs)
    bu <- setNames(lapply(seq_len(nrow(ufs)), \(i) encolhe(C$uf == ufs$uf[i], br[[ufs$regiao[i]]], par$kappa_uf)), ufs$uf)
    bu[["ZZ"]] <- bn                                                # exterior: nível Brasil
    do.call(rbind, bu)
  })
  razao <- function(d) c(comp = sum(d$pb * d$obs_comp) / sum(d$pb * d$bo_comp),
                         val = (sum(d$pb * d$obs_val) / sum(d$pb * d$obs_comp)) / (sum(d$pb * d$bo_val) / sum(d$pb * d$bo_comp)))
  CC <- C[obs_comp > 0]
  rn <- if (nrow(CC)) razao(CC) else c(comp = 1, val = 1)
  taxas <- CC[, { w <- .N / (.N + par$kappa_taxa); r <- w * razao(.SD) + (1 - w) * rn; .(r_comp = r[["comp"]], r_val = r[["val"]]) }, by = uf]
  taxas <- merge(data.table(uf = unique(m$uf)), taxas, by = "uf", all.x = TRUE)
  taxas[is.na(r_comp), `:=`(r_comp = rn[["comp"]], r_val = rn[["val"]])]
  m[, pb := NULL]
  list(coefs = coefs, taxas = taxas, n_unidades = nrow(C), turno = turno)
}

# detalhe = TRUE devolve também a parte pendente projetada de cada município (para a trajetória)
projetar <- function(m, est, centro, detalhe = FALSE) {
  tn <- est$turno
  obs <- c(PT = sum(m$obs_PT), PL = sum(m$obs_PL), OU = sum(m$obs_OU), val = sum(m$obs_val))
  P <- merge(m[completo == FALSE & aptos > aptos_obs & bp_comp > 0], est$taxas, by = "uf")
  if (!nrow(P)) {
    tot <- c(obs[c("PT", "PL", "OU")] / obs[["val"]], validos = obs[["val"]])
    return(if (detalhe) list(total = tot, obs = obs, pend = data.table()) else tot)
  }
  X <- X_de(P$bp_PT, P$bp_PL, P$bp_OU, P$aptos, tn, centro)
  sw <- matrix(sapply(est$coefs, \(B) rowSums(cbind(1, X) * B[match(P$uf, rownames(B)), , drop = FALSE])), nrow = nrow(P))
  val <- P$bp_comp * P$r_comp * (P$bp_val / P$bp_comp) * P$r_val
  if (tn == 1) {
    e1 <- lr(P$bp_PT, P$bp_OU) + sw[, 1]; e2 <- lr(P$bp_PL, P$bp_OU) + sw[, 2]
    den <- 1 + exp(e1) + exp(e2); sPT <- exp(e1) / den; sPL <- exp(e2) / den
  } else { sPT <- plogis(lr(P$bp_PT, P$bp_PL) + sw[, 1]); sPL <- 1 - sPT }
  tot <- obs + c(PT = sum(val * sPT), PL = sum(val * sPL), OU = sum(val * (1 - sPT - sPL)), val = sum(val))
  res <- c(tot[c("PT", "PL", "OU")] / tot[["val"]], validos = tot[["val"]])
  if (!detalhe) return(res)
  list(total = res, obs = obs, pend = data.table(uf = P$uf, aptos_pend = P$aptos - P$aptos_obs, val = val,
                                                  PT = val * sPT, PL = val * sPL, OU = val * (1 - sPT - sPL)))
}

# ---- trajetória projetada (o caminho da linha do apurado até 100%) -----------------------------------------------
# Ritmo de cada UF: fração do eleitorado da UF apurada por minuto nos últimos `janela` minutos.
# hist: data.table(hora, uf, f) acumulada entre ciclos (f = fração apurada da UF).
ritmo_uf <- function(hist, agora = max(hist$hora), janela = 15) {
  h <- hist[hora >= agora - janela * 60]
  h[, .(ritmo = if (.N >= 2 && diff(range(as.numeric(hora))) > 0)
          (f[which.max(hora)] - f[which.min(hora)]) / (diff(range(as.numeric(hora))) / 60) else NA_real_), by = uf]
}

# A parte pendente de cada UF chega no ritmo recente dela (UF atrasada termina depois); dentro da UF, os
# municípios pendentes avançam juntos. Sem ritmo medido (início da apuração), todas as UFs no mesmo passo.
# Devolve pontos (x = % do eleitorado apurado; PT, PL, OU = % dos válidos acumulados) do ponto atual até 100%.
trajetoria <- function(m, est, centro, ritmo = NULL, n_pontos = 24) {
  d <- projetar(m, est, centro, detalhe = TRUE)
  x0 <- sum(m$aptos_obs) / sum(m$aptos)
  if (!nrow(d$pend)) return(data.table(x = 100 * x0, PT = d$total[["PT"]], PL = d$total[["PL"]], OU = d$total[["OU"]]))
  u <- d$pend[, .(ap = sum(aptos_pend), val = sum(val), PT = sum(PT), PL = sum(PL), OU = sum(OU)), by = uf]
  u <- merge(u, m[, .(aptos_uf = sum(aptos)), by = uf], by = "uf")
  u[, frac_pend := ap / aptos_uf]
  if (!is.null(ritmo)) u <- merge(u, ritmo, by = "uf", all.x = TRUE) else u[, ritmo := NA_real_]
  med <- median(u$ritmo[u$ritmo > 0], na.rm = TRUE)
  if (!is.finite(med)) { u[, tempo := 1] } else {
    u[, tempo := frac_pend / fifelse(is.na(ritmo) | ritmo <= 0, med, ritmo)]
    u[, tempo := pmin(tempo, 5 * median(tempo))]                 # UF quase parada não estica o eixo sem fim
  }
  tau <- seq(0, max(u$tempo), length.out = n_pontos)
  rbindlist(lapply(tau, \(t) {
    w <- pmin(t / u$tempo, 1)
    num <- d$obs[c("PT", "PL", "OU")] + c(sum(w * u$PT), sum(w * u$PL), sum(w * u$OU))
    data.table(x = 100 * (sum(m$aptos_obs) + sum(w * u$ap)) / sum(m$aptos),
               PT = num[[1]] / sum(num), PL = num[[2]] / sum(num), OU = num[[3]] / sum(num))
  }))
}

# Projeção com faixa de 90% e probabilidade de 2º turno.
# Faixa: bootstrap de Poisson dos municípios (+ sorteio do intercepto nacional), réplicas ajustadas pela
# calibração do replay 2022: ponto + k(f) x (réplica - ponto) + ruído de piso (k = 3 antes de 2% apurado, 0,7 depois).
projetar_faixa <- function(m, turno, prior, centro, B = PAR_MOD$B) {
  est <- estimar(m, turno, prior, centro)
  ponto <- projetar(m, est, centro)
  reps <- t(replicate(B, projetar(m, estimar(m, turno, prior, centro, peso = rpois(nrow(m), 1), sortear_prior = TRUE),
                                  centro)[c("PT", "PL", "OU")]))
  reps <- reps[rowSums(!is.finite(reps)) == 0, , drop = FALSE]
  f <- sum(m$aptos_obs) / sum(m$aptos)
  k <- if (f < PAR_MOD$faixa_f_ini) PAR_MOD$faixa_k_ini else PAR_MOD$faixa_k
  pt <- matrix(ponto[c("PT", "PL", "OU")], nrow(reps), 3, byrow = TRUE)
  reps <- pt + k * (reps - pt) + matrix(rnorm(length(reps), 0, PAR_MOD$faixa_piso / qnorm(0.95)), nrow(reps))
  reps <- pmin(pmax(reps, 0), 1)                  # com quase nada apurado a inflação pode sair de [0, 1]
  q <- apply(reps, 2, quantile, probs = c(0.05, 0.95))
  out <- data.table(bloco = c("PT", "PL", "OUTROS"), proj = ponto[c("PT", "PL", "OU")], lo = q[1, ], hi = q[2, ],
                    p_2turno = mean(apply(reps, 1, max) < 0.5), pct_apurado = 100 * f, unidades = est$n_unidades)
  attr(out, "est") <- est                                           # reaproveitada pela trajetória
  out
}
