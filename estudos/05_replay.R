# Fase 5: replay da apuração de 2022 na ordem real de recebimento dos boletins (base 2018).
# Uso: Rscript R/05_replay.R [turno = 1] [passo_min = 5] [B = 100]
# Métodos comparados a cada instante:
#   bruto: % dos válidos apurados | uf: extrapolação por UF (% atual da UF x eleitorado restante x comparecimento observado)
#   A_pesq / A_zero: modelo com a parte pendente = base do município inteiro (prior das pesquisas / swing zero)
#   B_pesq / B_zero: modelo com a parte pendente = base dos locais das seções pendentes (o que o cs permite ao vivo)
# "Fim da noite" = instante em que 99,9% das seções estavam totalizadas; seções recebidas depois só entram no
# resultado final, não na série.
source("estudos/04_modelo.R")
source("estudos/06_grafico.R")
set.seed(2026)

a  <- commandArgs(TRUE)
tn <- if (length(a) >= 1) as.integer(a[1]) else 1L
passo <- if (length(a) >= 2) as.numeric(a[2]) else 5
PAR_MOD$B <- if (length(a) >= 3) as.integer(a[3]) else 100L
tz <- "America/Sao_Paulo"
dir.create("dados/replay", showWarnings = FALSE); dir.create("figs", showWarnings = FALSE)
t0 <- Sys.time()

# ---- base por local 2022 (pareamento 2022 <- 2018, mesma numeração do 02_rodar_pareamento.R) ------------
fontes <- list(rd("locais_2022.parquet"), rd("locais_2024.parquet"), rd("locais_2026.parquet"))
par <- list(pesos = rd("pareamento_2022_2018_pesos.parquet"), nivel = rd("pareamento_2022_2018_nivel.parquet"))
par$alvo <- prep(imputar_coord(locais_base(2022), fontes[2:3]))[, t_id := .I]
par$base <- prep(imputar_coord(locais_base(2018), fontes)[aptos > 0])[, b_id := .I]
chk <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], par$nivel[, .(t_id, u2 = uf, m2 = mun, z2 = zona, l2 = local)], by = "t_id")
stopifnot(nrow(chk) == nrow(par$nivel), chk[uf != u2 | mun != m2 | zona != z2 | local != l2, .N] == 0)
bl <- base_projetada(par, rd("hist_local_2018.parquet")[turno == tn])          # por 1 apto
bl <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], bl, by = "t_id")[, t_id := NULL]
setnames(bl, c("comparecimento", "validos", "PT", "PL", "OUTROS"), c("pa_comp", "pa_val", "pa_PT", "pa_PL", "pa_OU"))
bl[, aptos := NULL]

# ---- seções 2022: votos por bloco, horário, base -----------------------------------------------------
b22 <- blocos_de(2022)
v <- rd("votos_secao_2022.parquet")[turno == tn]
v[, bloco := fcase(nr_votavel %in% b22$PT, "PT", nr_votavel %in% b22$PL, "PL", nr_votavel %in% c(95, 96), "BN", default = "OU")]
v <- dcast(v, uf + mun + zona + secao ~ bloco, value.var = "votos", fun.aggregate = sum, fill = 0)
for (k in setdiff(c("PT", "PL", "OU", "BN"), names(v))) v[, (k) := 0]
s <- merge(rd("secoes_2022.parquet")[turno == tn, .(uf, mun, zona, secao, local, aptos, comparecimento, dt_recebido)],
           v, by = c("uf", "mun", "zona", "secao"))
s[, val := PT + PL + OU]
s <- merge(s, bl, by = c("uf", "mun", "zona", "local"), all.x = TRUE)
# local sem base: média do município (por apto), depois da UF
for (g in list(c("uf", "mun"), "uf")) {
  med <- s[!is.na(pa_comp), lapply(.SD, \(x) weighted.mean(x, aptos)), by = g, .SDcols = patterns("^pa_")]
  s[med, on = g, `:=`(pa_comp = fcoalesce(pa_comp, i.pa_comp), pa_val = fcoalesce(pa_val, i.pa_val),
                      pa_PT = fcoalesce(pa_PT, i.pa_PT), pa_PL = fcoalesce(pa_PL, i.pa_PL), pa_OU = fcoalesce(pa_OU, i.pa_OU))]
}
s[, `:=`(b_comp = aptos * pa_comp, b_val = aptos * pa_val, b_PT = aptos * pa_PT, b_PL = aptos * pa_PL, b_OU = aptos * pa_OU)]
cat("seções:", nrow(s), "| sem base após preenchimento:", s[is.na(b_comp), .N], "\n")

# municípios: região e elegibilidade (>= 90% do eleitorado com base de nível 1)
mi <- rd("municipios_ibge.parquet")[, .(uf, mun, regiao)]
elg <- merge(par$nivel[, .(uf, mun, aptos, n1 = nivel == "1")], mi, by = c("uf", "mun"), all.x = TRUE)[
  , .(elegivel = sum(aptos[n1]) / sum(aptos) >= PAR_MOD$nivel1_min, regiao = regiao[1]), by = .(uf, mun)]
elg[uf == "ZZ" | is.na(regiao), regiao := fifelse(uf == "ZZ", "ZZ", regiao)]

# ---- linha do tempo ----------------------------------------------------------------------------------
dia <- if (tn == 1) "2022-10-02" else "2022-10-30"
inicio <- as.POSIXct(paste(dia, "17:00:00"), tz = tz)
tt <- sort(s$dt_recebido)
fim_noite <- tt[ceiling(0.999 * length(tt))]
tardias <- s[dt_recebido > fim_noite]
cat(sprintf("fim da noite (99,9%% das seções): %s | seções depois: %d (%.3f%% do eleitorado)\n",
            format(fim_noite, tz = tz), nrow(tardias), 100 * sum(tardias$aptos) / sum(s$aptos)))
instantes <- seq(inicio, fim_noite, by = passo * 60)
if (tail(instantes, 1) < fim_noite) instantes <- c(instantes, fim_noite)

final <- s[, .(PT = sum(PT), PL = sum(PL), OU = sum(OU))][, v := PT + PL + OU][
  , data.table(bloco = c("PT", "PL", "OUTROS"), real = c(PT, PL, OU) / v)]
print(final)

# ---- prior -------------------------------------------------------------------------------------------
base_nac <- s[, .(PT = sum(b_PT), PL = sum(b_PL), OU = sum(b_OU))]
pr_pesq <- prior_swing(sprintf("config/prior_2022_%dt.yaml", tn), base_nac, tn)
pr_zero <- prior_swing(sprintf("config/prior_2022_%dt.yaml", tn), base_nac, tn, zero = TRUE)

# ---- estado de cada município num instante -------------------------------------------------------------
estado <- function(T) {
  s[, rec := dt_recebido <= T]
  m <- s[, .(aptos = sum(aptos), aptos_obs = sum(aptos[rec]), n = .N, n_rec = sum(rec),
             obs_comp = sum(comparecimento[rec]), obs_val = sum(val[rec]), obs_PT = sum(PT[rec]),
             obs_PL = sum(PL[rec]), obs_OU = sum(OU[rec]),
             bt_comp = sum(b_comp), bt_val = sum(b_val), bt_PT = sum(b_PT), bt_PL = sum(b_PL), bt_OU = sum(b_OU),
             pB_comp = sum(b_comp[!rec]), pB_val = sum(b_val[!rec]), pB_PT = sum(b_PT[!rec]),
             pB_PL = sum(b_PL[!rec]), pB_OU = sum(b_OU[!rec])), by = .(uf, mun)]
  m[, completo := n_rec == n]
  merge(m, elg, by = c("uf", "mun"), all.x = TRUE)[is.na(elegivel), elegivel := FALSE][is.na(regiao), regiao := "ZZ"]
}
com_variante <- function(m, var) {
  m <- copy(m); f <- (m$aptos - m$aptos_obs) / m$aptos
  if (var == "A") m[, `:=`(bp_comp = bt_comp * f, bp_val = bt_val * f, bp_PT = bt_PT * f, bp_PL = bt_PL * f, bp_OU = bt_OU * f)]
  else m[, `:=`(bp_comp = pB_comp, bp_val = pB_val, bp_PT = pB_PT, bp_PL = pB_PL, bp_OU = pB_OU)]
  m
}
extrap_uf <- function(m) {             # nível 1 do prompt: % atual da UF aplicado ao restante da UF
  u <- m[, .(aptos = sum(aptos), ao = sum(aptos_obs), c = sum(obs_comp), v = sum(obs_val), PT = sum(obs_PT),
             PL = sum(obs_PL), OU = sum(obs_OU)), by = uf][ao > 0]
  u[, fat := 1 + (aptos - ao) / ao]
  x <- u[, .(PT = sum(PT * fat), PL = sum(PL * fat), OU = sum(OU * fat))]
  unlist(x) / sum(unlist(x))
}

centro <- centro_nacional(com_variante(estado(inicio), "A"), tn)
serie <- rbindlist(lapply(seq_along(instantes), \(i) {
  T <- instantes[i]; m <- estado(T)
  pest <- sum(m$aptos_obs) / sum(m$aptos)
  if (pest == 0) return(NULL)
  obs <- m[, c(PT = sum(obs_PT), PL = sum(obs_PL), OU = sum(obs_OU)) / sum(obs_val)]
  mA <- com_variante(m, "A"); mB <- com_variante(m, "B")
  pontos <- list(
    A_pesq = projetar(mA, estimar(mA, tn, pr_pesq, centro), centro),
    A_zero = projetar(mA, estimar(mA, tn, pr_zero, centro), centro),
    B_zero = projetar(mB, estimar(mB, tn, pr_zero, centro), centro))
  fx <- projetar_faixa(mB, tn, pr_pesq, centro)
  fp <- projetar_faixa(mB, tn, pr_pesq, centro, unidades = "apurado")
  uf <- extrap_uf(m)
  out <- rbind(
    data.table(metodo = "bruto", bloco = c("PT", "PL", "OUTROS"), proj = obs, lo = NA_real_, hi = NA_real_, p_2turno = NA_real_),
    data.table(metodo = "uf", bloco = c("PT", "PL", "OUTROS"), proj = uf, lo = NA_real_, hi = NA_real_, p_2turno = NA_real_),
    rbindlist(lapply(names(pontos), \(nm) data.table(metodo = nm, bloco = c("PT", "PL", "OUTROS"),
      proj = pontos[[nm]][c("PT", "PL", "OU")], lo = NA_real_, hi = NA_real_, p_2turno = NA_real_))),
    fx[, .(metodo = "B_pesq", bloco, proj, lo, hi, p_2turno)],
    fp[, .(metodo = "B_parc", bloco, proj, lo, hi, p_2turno)])
  out[, `:=`(hora = T, pestn = 100 * pest, pstn = 100 * sum(m$n_rec) / sum(m$n), apurado = rep(obs, length.out = .N),
             n_mun_completos = sum(m$completo))]
  if (i %% 6 == 1) message(sprintf("%s  %.1f%% apurado  B_pesq PT %.2f PL %.2f | B_parc PT %.2f PL %.2f", format(T, "%H:%M", tz = tz), 100 * pest,
                                   100 * fx$proj[1], 100 * fx$proj[2], 100 * fp$proj[1], 100 * fp$proj[2]))
  out
}))
serie <- merge(serie, final, by = "bloco")
serie[, erro := 100 * (proj - real)]
write_parquet(serie, sprintf("dados/replay/replay_2022_t%d.parquet", tn))

# ---- métricas ----------------------------------------------------------------------------------------------
blocos_m <- if (tn == 1) c("PT", "PL") else "PT"
cat("\n== erro absoluto (p.p.) por % apurado (média de PT e PL; 2T: PT) ==\n")
serie[, faixa := cut(pestn, c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), right = TRUE)]
tab <- serie[bloco %in% blocos_m, .(erro = mean(abs(erro))), by = .(metodo, faixa)]
print(dcast(tab, faixa ~ metodo, value.var = "erro")[, lapply(.SD, \(v) if (is.double(v)) round(v, 2) else v)])

marco <- function(lim) serie[bloco %in% blocos_m, .(ok = all(abs(erro) < lim)), by = .(metodo, hora, pestn)][
  order(hora), .(desde_pestn = { r <- rev(cumprod(rev(ok))) == 1; if (any(r)) pestn[which(r)[1]] else NA_real_ }), by = metodo]
m05 <- marco(0.5); m025 <- marco(0.25)
lider <- serie[, .SD[which.max(proj)], by = .(metodo, hora, pestn)][, ok := bloco == final[which.max(real), bloco]][
  order(hora), .(lider_certo_desde = { r <- rev(cumprod(rev(ok))) == 1; if (any(r)) pestn[which(r)[1]] else NA_real_ }), by = metodo]
res <- Reduce(\(a, b) merge(a, b, by = "metodo"), list(m05[, .(metodo, erro_lt_0.5_desde = desde_pestn)],
                                                         m025[, .(metodo, erro_lt_0.25_desde = desde_pestn)], lider))
cat("\n== a partir de que % apurado (eleitorado) o erro fica sempre abaixo do limite / o líder fica certo ==\n"); print(res)
cob <- dcast(serie[metodo %in% c("B_pesq", "B_parc") & bloco %in% blocos_m, .(cobertura_90 = mean(real >= lo & real <= hi)), by = .(metodo, faixa)], faixa ~ metodo, value.var = "cobertura_90")
cat("\n== cobertura do intervalo de 90% (B_pesq) por faixa de apuração ==\n"); print(cob)
fwrite(tab, sprintf("docs/replay_2022_t%d_erro_por_faixa.csv", tn)); fwrite(res, sprintf("docs/replay_2022_t%d_marcos.csv", tn))

# ---- gráficos -------------------------------------------------------------------------------------------------
for (mt in c("B_pesq", "B_parc", "A_pesq", "B_zero")) {
  d <- serie[metodo == mt, .(x = pestn, bloco, apurado, proj, lo, hi)]
  if (tn == 2) d <- d[bloco != "OUTROS"]
  g <- grafico_apuracao(d, final[if (tn == 2) bloco != "OUTROS" else TRUE],
                        titulo = sprintf("Replay 2022, %dº turno: projeção %s", tn, mt),
                        subtitulo = "linha cheia = apurado | pontilhada = projeção (faixa de 90% em B_pesq e B_parc) | fina = resultado final",
                        rotulos = list(PT = "Lula (PT)", PL = if (tn == 1) "Bolsonaro (PL)" else "Bolsonaro (PL)"))
  ggsave(sprintf("figs/replay_2022_t%d_%s.png", tn, mt), g, width = 10, height = 6, dpi = 120, bg = "white")
}
message(sprintf("tempo total: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
