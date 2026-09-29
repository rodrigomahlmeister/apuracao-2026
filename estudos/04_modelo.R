# Fase 4: modelo de projeção (versão mínima), por município.
#
# Cada município tem uma parte apurada (votos reais) e uma parte pendente (eleitorado das seções não
# totalizadas). A parte pendente é projetada a partir de uma base (pseudo-contagens da eleição anterior,
# redistribuídas para a geografia atual pelo pareamento de locais):
#   variante A: base do município inteiro, reescalada ao eleitorado pendente
#   variante B: base dos locais das seções pendentes (via cs ao vivo; via BU no replay)
# Swing em razão log (1T: PT/OUTROS e PL/OUTROS; 2T: PT/PL), por regressão ponderada nos municípios
# completos: swing ~ e1 + e2 + le (eta da base e log do eleitorado, centrados na média nacional).
# Hierarquia: Brasil (intercepto encolhido para o prior das pesquisas) -> região -> UF, cada nível
# encolhido para o de cima com peso n / (n + kappa). Exterior (ZZ) usa o nível Brasil.
# Comparecimento e taxa de válidos: razão real/base nos municípios completos da UF, encolhida para o Brasil.
# Incerteza: bootstrap dos municípios completos dentro da UF + sorteio do intercepto nacional.
source("preparo/02_pareamento.R")
suppressPackageStartupMessages(library(yaml))

PAR_MOD <- list(kappa_reg = 30, kappa_uf = 30, kappa_taxa = 20, min_n = 8, nivel1_min = 0.9, B = 200,
                # calibração da faixa no replay 2022 (ver notas): multiplicador antes/depois de 2% apurado e piso
                faixa_f_ini = 0.02, faixa_k_ini = 3, faixa_k = 0.7, faixa_piso = 4e-4)

# ---- votos por bloco a partir dos candidatos do -u.json ------------------------------------------
# Só candidatos com destinação válida (dvt começa com "Válido") entram nos válidos. "Anulado" e
# "Anulado sub judice" ficam fora e são registrados à parte. Nunca somar vap de todos os candidatos.
e_valido <- function(dvt) !is.na(dvt) & startsWith(dvt, "Válido")

votos_blocos <- function(cand, blocos) {
  v <- cand[e_valido(dvt)]
  data.table(PT = sum(v[n %in% as.character(blocos$PT), vap]),
             PL = sum(v[n %in% as.character(blocos$PL), vap]),
             OUTROS = sum(v[!n %in% as.character(c(blocos$PT, blocos$PL)), vap]),
             anulados_sub_judice = sum(cand[!e_valido(dvt) & grepl("sub judice", dvt, ignore.case = TRUE), vap]),
             anulados = sum(cand[!e_valido(dvt) & !grepl("sub judice", dvt, ignore.case = TRUE), vap]))
}

blocos_de <- function(ano) read_yaml("config/blocos.yaml")$eleicoes[[as.character(ano)]]

# ---- prior -----------------------------------------------------------------------------------------
# Prior do intercepto nacional = razões log das pesquisas - razões log da base nacional.
# dp em proporção de válidos -> dp em razão log pelo método delta.
prior_swing <- function(prior_yaml, base_nac, turno, zero = FALSE) {
  pr <- read_yaml(prior_yaml)
  m <- pr$media_validos; dp <- pr$dp_validos$PT
  b <- base_nac                                               # list(PT, PL, OU) contagens da base nacional
  if (turno == 1) {
    mu <- c(log(m$PT / m$OUTROS) - log(b$PT / b$OU), log(m$PL / m$OUTROS) - log(b$PL / b$OU))
    sd <- c(sqrt((dp / m$PT)^2 + (dp / m$OUTROS)^2), sqrt((dp / m$PL)^2 + (dp / m$OUTROS)^2))
  } else {
    mu <- log(m$PT / m$PL) - log(b$PT / b$PL)
    sd <- sqrt((dp / m$PT)^2 + (dp / m$PL)^2)
  }
  if (zero) mu[] <- 0
  list(mu = mu, sd = sd)
}

# ---- preparação das unidades ----------------------------------------------------------------------
# `m`: uma linha por município com
#   uf, mun, regiao, aptos, aptos_obs, completo, elegivel (>= nivel1_min do eleitorado com base nível 1),
#   obs_{comp,val,PT,PL,OU} (apurado), bt_{comp,val,PT,PL,OU} (base do município inteiro, em contagens),
#   bp_{comp,val,PT,PL,OU} (base da parte pendente, em contagens; variante A ou B)
eta_base <- function(PT, PL, OU, turno) {
  if (turno == 1) cbind(e1 = lr(PT, OU), e2 = lr(PL, OU)) else cbind(e1 = lr(PT, PL))
}

centro_nacional <- function(m, turno) {
  E <- eta_base(m$bt_PT, m$bt_PL, m$bt_OU, turno)
  c(apply(E, 2, weighted.mean, w = m$aptos), le = weighted.mean(log(m$aptos), m$aptos))
}

X_de <- function(PT, PL, OU, aptos, turno, centro) {
  E <- eta_base(PT, PL, OU, turno)
  X <- cbind(E, le = log(aptos))
  sweep(X, 2, centro[colnames(X)])
}

wls <- function(y, X, w) {
  ok <- is.finite(y) & rowSums(!is.finite(X)) == 0 & w > 0
  y <- y[ok]; X <- X[ok, , drop = FALSE]; w <- w[ok]
  if (length(y) <= ncol(X) + 2) return(NULL)
  f <- lm.wfit(cbind(1, X), y, w)
  b <- coef(f); b[is.na(b)] <- 0
  r <- y - drop(cbind(1, X) %*% b)
  list(b = b, s2 = sum(w * r^2) / sum(w), n = length(y), sw = sum(w), sw2 = sum(w^2))
}

# ---- estimação do swing (hierarquia Brasil -> região -> UF) --------------------------------------
# peso: pesos de bootstrap por linha de m (NULL = 1); municípios com peso 0 ficam fora da estimação
# unidades = "completos": só municípios 100% totalizados, base = município inteiro (versão mínima)
#          = "apurado": parte apurada de todo município (completo ou parcial), base = base da parte apurada
#            (bt - bp; exige saber quais seções foram apuradas, i.e. variante B / cs)
estimar <- function(m, turno, prior, centro, par = PAR_MOD, sortear_prior = FALSE, peso = NULL,
                    unidades = "completos") {
  k_rat <- if (turno == 1) 2 else 1
  if (is.null(peso)) peso <- rep(1, nrow(m))
  m[, pb := peso]
  if (unidades == "completos") {
    C <- m[completo == TRUE & elegivel == TRUE & obs_val > 0 & pb > 0]
    C[, `:=`(bo_PT = bt_PT, bo_PL = bt_PL, bo_OU = bt_OU, bo_comp = bt_comp, bo_val = bt_val, wt = aptos * pb)]
  } else {
    C <- m[elegivel == TRUE & obs_val > 0 & aptos_obs > 0 & pb > 0]
    # unidade com cs e -u dessincronizados (sinc = FALSE) não tem base confiável da parte apurada
    if ("sinc" %in% names(C)) C <- C[sinc == TRUE | completo == TRUE]
    C[, `:=`(bo_PT = bt_PT - bp_PT, bo_PL = bt_PL - bp_PL, bo_OU = bt_OU - bp_OU, bo_comp = bt_comp - bp_comp,
             bo_val = bt_val - bp_val, wt = aptos_obs * pb)]
    C <- C[bo_comp > 0]
  }
  X <- X_de(C$bo_PT, C$bo_PL, C$bo_OU, C$aptos, turno, centro)
  Y <- if (turno == 1) cbind(lr(C$obs_PT, C$obs_OU) - lr(C$bo_PT, C$bo_OU), lr(C$obs_PL, C$obs_OU) - lr(C$bo_PL, C$bo_OU))
       else cbind(lr(C$obs_PT, C$obs_PL) - lr(C$bo_PT, C$bo_PL))
  np <- ncol(X) + 1
  coefs <- list()
  for (k in seq_len(k_rat)) {
    # Brasil: intercepto combina dado e prior por precisão; slopes do dado (zero sem dado)
    f <- if (nrow(C) >= par$min_n) wls(Y[, k], X, C$wt)
    if (is.null(f)) { bn <- c(prior$mu[k], rep(0, np - 1)); v_int <- prior$sd[k]^2 }
    else {
      v_dado <- max(f$s2 * f$sw2 / f$sw^2, 1e-8)     # ajuste perfeito (s2 = 0) não pode zerar a variância
      v_int <- 1 / (1 / v_dado + 1 / prior$sd[k]^2)
      bn <- f$b; bn[1] <- v_int * (f$b[1] / v_dado + prior$mu[k] / prior$sd[k]^2)
    }
    if (sortear_prior) bn[1] <- bn[1] + rnorm(1, 0, sqrt(v_int))
    # região e UF
    encolhe <- function(idx, alvo, kappa) {
      if (sum(idx) < par$min_n) return(alvo)
      f <- wls(Y[idx, k], X[idx, , drop = FALSE], C$wt[idx]); if (is.null(f)) return(alvo)
      w <- f$n / (f$n + kappa); w * f$b + (1 - w) * alvo
    }
    regs <- setdiff(unique(m$regiao), "ZZ")
    br <- setNames(lapply(regs, \(r) encolhe(C$regiao == r, bn, par$kappa_reg)), regs)
    ufs <- unique(m[uf != "ZZ", .(uf, regiao)])
    bu <- setNames(lapply(seq_len(nrow(ufs)), \(i) encolhe(C$uf == ufs$uf[i], br[[ufs$regiao[i]]], par$kappa_uf)), ufs$uf)
    bu[["ZZ"]] <- bn
    coefs[[k]] <- bu
  }
  # comparecimento e taxa de válidos: razão real/base, UF encolhida para Brasil
  razao <- function(d) c(comp = sum(d$pb * d$obs_comp) / sum(d$pb * d$bo_comp),
                         val = (sum(d$pb * d$obs_val) / sum(d$pb * d$obs_comp)) / (sum(d$pb * d$bo_val) / sum(d$pb * d$bo_comp)))
  CC <- C[obs_comp > 0]
  rn <- if (nrow(CC)) razao(CC) else c(comp = 1, val = 1)
  taxas <- CC[, { w <- .N / (.N + par$kappa_taxa); r <- w * razao(.SD) + (1 - w) * rn
                  .(r_comp = r[["comp"]], r_val = r[["val"]]) }, by = uf]
  taxas <- merge(data.table(uf = unique(m$uf)), taxas, by = "uf", all.x = TRUE)
  taxas[is.na(r_comp), `:=`(r_comp = rn[["comp"]], r_val = rn[["val"]])]
  m[, pb := NULL]
  # coeficientes como matriz (linhas = UF) para busca rápida na projeção
  coefs <- lapply(coefs, \(bu) do.call(rbind, bu))
  list(coefs = coefs, taxas = taxas, n_completos = nrow(C), turno = turno)
}

# ---- projeção -----------------------------------------------------------------------------------
projetar <- function(m, est, centro) {
  tn <- est$turno
  P <- m[completo == FALSE & aptos > aptos_obs & bp_comp > 0]
  P <- merge(P, est$taxas, by = "uf")
  obs <- c(PT = sum(m$obs_PT), PL = sum(m$obs_PL), OU = sum(m$obs_OU), val = sum(m$obs_val))
  if (!nrow(P)) return(c(obs[c("PT", "PL", "OU")] / obs[["val"]], validos = obs[["val"]]))   # nada pendente
  X <- X_de(P$bp_PT, P$bp_PL, P$bp_OU, P$aptos, tn, centro)
  sw <- sapply(est$coefs, \(Bm) rowSums(cbind(1, X) * Bm[match(P$uf, rownames(Bm)), , drop = FALSE]))
  sw <- matrix(sw, nrow = nrow(P))
  val <- P$bp_comp * P$r_comp * (P$bp_val / P$bp_comp) * P$r_val
  if (tn == 1) {
    e1 <- lr(P$bp_PT, P$bp_OU) + sw[, 1]; e2 <- lr(P$bp_PL, P$bp_OU) + sw[, 2]
    den <- 1 + exp(e1) + exp(e2); sPT <- exp(e1) / den; sPL <- exp(e2) / den
  } else {
    sPT <- plogis(lr(P$bp_PT, P$bp_PL) + sw[, 1]); sPL <- 1 - sPT
  }
  pend <- c(PT = sum(val * sPT), PL = sum(val * sPL), OU = sum(val * (1 - sPT - sPL)), val = sum(val))
  obs <- c(PT = sum(m$obs_PT), PL = sum(m$obs_PL), OU = sum(m$obs_OU), val = sum(m$obs_val))
  tot <- obs + pend
  c(tot[c("PT", "PL", "OU")] / tot[["val"]], validos = tot[["val"]])
}

# Projeção com faixa: ponto (sem bootstrap) + quantis de B réplicas. Bootstrap de Poisson: cada município
# completo recebe peso ~ Poisson(1) (equivale a reamostrar dentro da UF, sem copiar dados) + sorteio do
# intercepto nacional pela posterior.
projetar_faixa <- function(m, turno, prior, centro, B = PAR_MOD$B, niveis = c(0.05, 0.95), unidades = "completos",
                           calibrar = TRUE) {
  est <- estimar(m, turno, prior, centro, unidades = unidades)
  ponto <- projetar(m, est, centro)
  reps <- t(replicate(B, {
    e <- estimar(m, turno, prior, centro, sortear_prior = TRUE, peso = rpois(nrow(m), 1), unidades = unidades)
    projetar(m, e, centro)[c("PT", "PL", "OU")]
  }))
  ok <- rowSums(!is.finite(reps)) == 0
  if (!all(ok)) warning(sum(!ok), " réplica(s) do bootstrap descartada(s) por valor não finito")
  reps <- reps[ok, , drop = FALSE]
  # calibração (replay 2022, 1º e 2º turnos): o bootstrap é estreito demais no começo (< 2% apurado),
  # largo demais no meio e vai a zero no fim. Réplica ajustada = ponto + k(f) x (réplica - ponto) + ruído de piso.
  if (calibrar) {
    f <- sum(m$aptos_obs) / sum(m$aptos)
    k <- if (f < PAR_MOD$faixa_f_ini) PAR_MOD$faixa_k_ini else PAR_MOD$faixa_k
    pt <- matrix(ponto[c("PT", "PL", "OU")], nrow(reps), 3, byrow = TRUE)
    reps <- pt + k * (reps - pt) + matrix(rnorm(length(reps), 0, PAR_MOD$faixa_piso / qnorm(0.95)), nrow(reps))
  }
  q <- apply(reps, 2, quantile, probs = niveis)
  p2t <- mean(apply(reps, 1, max) < 0.5)
  data.table(bloco = c("PT", "PL", "OUTROS"), proj = ponto[c("PT", "PL", "OU")],
             lo = q[1, ], hi = q[2, ], p_2turno = p2t, n_completos = est$n_completos)
}
