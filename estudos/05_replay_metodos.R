# Fase 5: replay 2022 comparando todos os métodos de projeção sobre a MESMA apuração (ordem real dos BUs).
# Uso: Rscript R/05_replay_metodos.R [turno = 1] [passo_min = 5] [B = 40] [atraso_min = 2]
#
# A cada instante T cada método recebe só a informação que teria naquele momento:
#   votos (-u)       = seções recebidas até T_u;   status das seções (cs) = seções recebidas até T_cs
#   ua: -u atrasado (T_u = T - atraso, T_cs = T)  |  ca: cs atrasado (T_cs = T - atraso, T_u = T)
# Sincronia por unidade: nº de seções no -u = nº com hora no cs. Se o cs está à frente, "reconciliação"
# toma como apuradas as n_u seções com hora mais antiga. Unidade dessincronizada: parte pendente com a base
# do município inteiro (variante A) e fora da estimação do swing.
# Unidades: município; em P_zona, zona eleitoral nas 101 cidades grandes (capitais + > 200 mil eleitores).
source("estudos/04_modelo.R")
source("estudos/06_grafico.R")
set.seed(2026)

a  <- commandArgs(TRUE)
tn <- if (length(a) >= 1) as.integer(a[1]) else 1L
passo <- if (length(a) >= 2) as.numeric(a[2]) else 5
PAR_MOD$B <- if (length(a) >= 3) as.integer(a[3]) else 40L
atraso <- (if (length(a) >= 4) as.numeric(a[4]) else 2) * 60
tz <- "America/Sao_Paulo"
dir.create("dados/replay", showWarnings = FALSE); dir.create("figs", showWarnings = FALSE)
t0 <- Sys.time()

# ---- base por local 2022 (pareamento 2022 <- 2018) -------------------------------------------------
fontes <- list(rd("locais_2022.parquet"), rd("locais_2024.parquet"), rd("locais_2026.parquet"))
par <- list(pesos = rd("pareamento_2022_2018_pesos.parquet"), nivel = rd("pareamento_2022_2018_nivel.parquet"))
par$alvo <- prep(imputar_coord(locais_base(2022), fontes[2:3]))[, t_id := .I]
par$base <- prep(imputar_coord(locais_base(2018), fontes)[aptos > 0])[, b_id := .I]
chk <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], par$nivel[, .(t_id, u2 = uf, m2 = mun, z2 = zona, l2 = local)], by = "t_id")
stopifnot(nrow(chk) == nrow(par$nivel), chk[uf != u2 | mun != m2 | zona != z2 | local != l2, .N] == 0)
bl <- base_projetada(par, rd("hist_local_2018.parquet")[turno == tn])
bl <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], bl, by = "t_id")
bl <- merge(bl, par$nivel[, .(t_id, n1 = nivel == "1")], by = "t_id")[, t_id := NULL]
setnames(bl, c("comparecimento", "validos", "PT", "PL", "OUTROS"), c("pa_comp", "pa_val", "pa_PT", "pa_PL", "pa_OU"))
bl[, aptos := NULL]

# ---- seções 2022 ----------------------------------------------------------------------------------------
b22 <- blocos_de(2022)
v <- rd("votos_secao_2022.parquet")[turno == tn]
v[, bloco := fcase(nr_votavel %in% b22$PT, "PT", nr_votavel %in% b22$PL, "PL", nr_votavel %in% c(95, 96), "BN", default = "OU")]
v <- dcast(v, uf + mun + zona + secao ~ bloco, value.var = "votos", fun.aggregate = sum, fill = 0)
for (k in setdiff(c("PT", "PL", "OU", "BN"), names(v))) v[, (k) := 0]
s <- merge(rd("secoes_2022.parquet")[turno == tn, .(uf, mun, zona, secao, local, aptos, comparecimento, dt_recebido)],
           v, by = c("uf", "mun", "zona", "secao"))
s[, val := PT + PL + OU]
s <- merge(s, bl, by = c("uf", "mun", "zona", "local"), all.x = TRUE)
for (g in list(c("uf", "mun"), "uf")) {
  med <- s[!is.na(pa_comp), lapply(.SD, \(x) weighted.mean(x, aptos)), by = g, .SDcols = patterns("^pa_")]
  s[med, on = g, `:=`(pa_comp = fcoalesce(pa_comp, i.pa_comp), pa_val = fcoalesce(pa_val, i.pa_val),
                      pa_PT = fcoalesce(pa_PT, i.pa_PT), pa_PL = fcoalesce(pa_PL, i.pa_PL), pa_OU = fcoalesce(pa_OU, i.pa_OU))]
}
s[is.na(n1), n1 := FALSE]
s[, `:=`(b_comp = aptos * pa_comp, b_val = aptos * pa_val, b_PT = aptos * pa_PT, b_PL = aptos * pa_PL, b_OU = aptos * pa_OU)]

mi <- rd("municipios_ibge.parquet")[, .(uf, mun, regiao, capital)]
el26 <- rd("locais_2026.parquet")[, .(el = sum(eleitores)), by = .(uf, mun)]
grandes <- merge(el26, mi, by = c("uf", "mun"))[capital | el > 2e5, .(uf, mun)]
s <- merge(s, mi[, .(uf, mun, regiao)], by = c("uf", "mun"), all.x = TRUE)
s[uf == "ZZ" | is.na(regiao), regiao := "ZZ"]
s[, grande := FALSE][grandes, on = .(uf, mun), grande := TRUE]
s[, un_mun := paste(uf, mun)]
s[, un_zona := fifelse(grande, paste(uf, mun, zona), un_mun)]
cat(sprintf("seções: %d | cidades grandes: %d | unidades: %d municípios, %d com zonas nas grandes\n",
            nrow(s), nrow(grandes), uniqueN(s$un_mun), uniqueN(s$un_zona)))

# ---- linha do tempo --------------------------------------------------------------------------------------
dia <- if (tn == 1) "2022-10-02" else "2022-10-30"
inicio <- as.POSIXct(paste(dia, "17:00:00"), tz = tz)
tt <- sort(s$dt_recebido); fim_noite <- tt[ceiling(0.999 * length(tt))]
instantes <- seq(inicio, fim_noite, by = passo * 60)
if (tail(instantes, 1) < fim_noite) instantes <- c(instantes, fim_noite)
final <- s[, .(PT = sum(PT), PL = sum(PL), OU = sum(OU))][, v := PT + PL + OU][
  , data.table(bloco = c("PT", "PL", "OUTROS"), real = c(PT, PL, OU) / v)]
base_nac <- s[, .(PT = sum(b_PT), PL = sum(b_PL), OU = sum(b_OU))]
pr_pesq <- prior_swing(sprintf("config/prior_2022_%dt.yaml", tn), base_nac, tn)
pr_zero <- prior_swing(sprintf("config/prior_2022_%dt.yaml", tn), base_nac, tn, zero = TRUE)

# ---- unidades num instante ---------------------------------------------------------------------------------
# rv: seção nos votos (-u); rc: seção com hora no cs; variante "A" (base do município) ou "B" (cs)
# recon (quando nº de seções no -u = n_u difere do nº com hora no cs = n_c):
#   "nenhuma"      unidade dessincronizada (base do município, fora da estimação)
#   "ordem"        cs à frente: apuradas = as n_u com hora mais antiga (exato no replay: otimista)
#   "aleatoria"    cs à frente: apuradas = n_u sorteadas entre as com hora (pessimista quanto à ordem)
#   "proporcional" peso fracionário por seção, sem usar ordem: cs atrás -> as com hora contam inteiras e
#                  cada seção sem hora conta (n_u - n_c) / (n - n_c); cs à frente -> cada seção com hora
#                  conta n_u / n_c. Funciona nos dois sentidos.
unidades <- function(chave, Tu, Tc, var = "B", recon = "ordem") {
  s[, `:=`(rv = dt_recebido <= Tu, rc = dt_recebido <= Tc, un = get(chave))]
  n_u <- s[, .(n_u = sum(rv), n_c = sum(rc), n = .N), by = un]
  s[n_u, on = "un", `:=`(n_u = i.n_u, n_c = i.n_c, n_tot = i.n)]
  s[, w_ap := as.numeric(rc)]                                   # fração de cada seção tratada como apurada
  if (recon == "ordem")
    s[n_c > n_u, w_ap := as.numeric(rc & frank(fifelse(rc, as.numeric(dt_recebido), Inf), ties.method = "first") <= n_u[1]), by = un]
  if (recon == "aleatoria")
    s[n_c > n_u, w_ap := as.numeric(rc & frank(fifelse(rc, runif(.N), Inf), ties.method = "first") <= n_u[1]), by = un]
  if (recon == "proporcional") {
    s[n_c < n_u, w_ap := fifelse(rc, 1, (n_u - n_c) / (n_tot - n_c))]
    s[n_c > n_u, w_ap := fifelse(rc, n_u / n_c, 0)]
  }
  m <- s[, .(uf = uf[1], regiao = regiao[1], aptos = sum(aptos), aptos_obs = sum(aptos[rv]), n = .N, n_u = n_u[1],
             sinc = recon == "proporcional" | abs(sum(w_ap) - n_u[1]) < 1e-9,
             elegivel = sum(aptos[n1]) / sum(aptos) >= PAR_MOD$nivel1_min,
             obs_comp = sum(comparecimento[rv]), obs_val = sum(val[rv]), obs_PT = sum(PT[rv]), obs_PL = sum(PL[rv]), obs_OU = sum(OU[rv]),
             bt_comp = sum(b_comp), bt_val = sum(b_val), bt_PT = sum(b_PT), bt_PL = sum(b_PL), bt_OU = sum(b_OU),
             pB_comp = sum(b_comp * (1 - w_ap)), pB_val = sum(b_val * (1 - w_ap)), pB_PT = sum(b_PT * (1 - w_ap)),
             pB_PL = sum(b_PL * (1 - w_ap)), pB_OU = sum(b_OU * (1 - w_ap))),
         by = un]
  m[, completo := n_u == n]
  f <- (m$aptos - m$aptos_obs) / m$aptos
  usarB <- var == "B" & m$sinc
  for (k in c("comp", "val", "PT", "PL", "OU"))
    set(m, j = paste0("bp_", k), value = fifelse(usarB, m[[paste0("pB_", k)]], m[[paste0("bt_", k)]] * f))
  if (var == "A") m[, sinc := FALSE]
  m[]
}
extrap_uf <- function(m) {
  u <- m[, .(aptos = sum(aptos), ao = sum(aptos_obs), PT = sum(obs_PT), PL = sum(obs_PL), OU = sum(obs_OU)), by = uf][ao > 0]
  u[, fat := aptos / ao]
  x <- u[, c(sum(PT * fat), sum(PL * fat), sum(OU * fat))]; x / sum(x)
}

# conjunto de métodos (argumento 5): "v1" = primeira comparação; "recon" = modos de reconciliação (padrão)
conjunto <- if (length(a) >= 5) a[5] else "recon"
metodos <- if (conjunto == "v1") list(   # nome = (chave, atraso -u, atraso cs, variante, reconciliação, unidades, prior, faixa)
  M_A           = list("un_mun",  0, 0, "A", "nenhuma", "completos", "pesq", FALSE),
  M_B           = list("un_mun",  0, 0, "B", "ordem",   "completos", "pesq", TRUE),
  P_mun         = list("un_mun",  0, 0, "B", "ordem",   "apurado",   "pesq", FALSE),
  P_mun_ua      = list("un_mun",  1, 0, "B", "ordem",   "apurado",   "pesq", TRUE),
  P_mun_ua_sem  = list("un_mun",  1, 0, "B", "nenhuma", "apurado",   "pesq", FALSE),
  P_mun_ca      = list("un_mun",  0, 1, "B", "ordem",   "apurado",   "pesq", FALSE),
  P_zona_ua     = list("un_zona", 1, 0, "B", "ordem",   "apurado",   "pesq", TRUE),
  P_mun_ua_zero = list("un_mun",  1, 0, "B", "ordem",   "apurado",   "zero", FALSE)
) else list(
  M_B           = list("un_mun",  0, 0, "B", "ordem",        "completos", "pesq", FALSE),
  P_mun         = list("un_mun",  0, 0, "B", "ordem",        "apurado",   "pesq", FALSE),
  P_ua_ordem    = list("un_mun",  1, 0, "B", "ordem",        "apurado",   "pesq", FALSE),
  P_ua_aleat    = list("un_mun",  1, 0, "B", "aleatoria",    "apurado",   "pesq", FALSE),
  P_ua_prop     = list("un_mun",  1, 0, "B", "proporcional", "apurado",   "pesq", TRUE),
  P_ua_nenhuma  = list("un_mun",  1, 0, "B", "nenhuma",      "apurado",   "pesq", FALSE),
  P_ca_prop     = list("un_mun",  0, 1, "B", "proporcional", "apurado",   "pesq", TRUE),
  P_ca_nenhuma  = list("un_mun",  0, 1, "B", "nenhuma",      "apurado",   "pesq", FALSE),
  P_ua_prop_4m  = list("un_mun",  2, 0, "B", "proporcional", "apurado",   "pesq", FALSE))

centros <- list(un_mun = centro_nacional(unidades("un_mun", inicio, inicio, "A"), tn),
                un_zona = centro_nacional(unidades("un_zona", inicio, inicio, "A"), tn))

serie <- rbindlist(lapply(seq_along(instantes), \(i) {
  T <- instantes[i]
  mT <- unidades("un_mun", T, T, "A")
  pest <- sum(mT$aptos_obs) / sum(mT$aptos)
  if (pest == 0) return(NULL)
  obs <- mT[, c(sum(obs_PT), sum(obs_PL), sum(obs_OU)) / sum(obs_val)]
  lin <- list(data.table(metodo = "bruto", bloco = c("PT", "PL", "OUTROS"), proj = obs, lo = NA_real_, hi = NA_real_, p_2turno = NA_real_),
              data.table(metodo = "uf", bloco = c("PT", "PL", "OUTROS"), proj = extrap_uf(mT), lo = NA_real_, hi = NA_real_, p_2turno = NA_real_))
  dess <- list()
  for (nm in names(metodos)) {
    cfg <- metodos[[nm]]
    m <- unidades(cfg[[1]], T - cfg[[2]] * atraso, T - cfg[[3]] * atraso, cfg[[4]], cfg[[5]])
    pr <- if (cfg[[7]] == "pesq") pr_pesq else pr_zero
    ctr <- centros[[cfg[[1]]]]
    if (cfg[[8]]) {
      fx <- projetar_faixa(m, tn, pr, ctr, unidades = cfg[[6]])
      lin[[nm]] <- fx[, .(metodo = nm, bloco, proj, lo, hi, p_2turno)]
    } else {
      pj <- projetar(m, estimar(m, tn, pr, ctr, unidades = cfg[[6]]), ctr)
      lin[[nm]] <- data.table(metodo = nm, bloco = c("PT", "PL", "OUTROS"), proj = pj[c("PT", "PL", "OU")],
                              lo = NA_real_, hi = NA_real_, p_2turno = NA_real_)
    }
    parc <- m[aptos_obs > 0 & !completo]
    dess[[nm]] <- if (nrow(parc)) mean(!parc$sinc) else NA_real_
  }
  out <- rbindlist(lin)
  out[, `:=`(hora = T, pestn = 100 * pest, apurado = rep(obs, length.out = .N))]
  out[, dessinc := unlist(dess)[match(metodo, names(dess))]]
  com_fx <- names(metodos)[vapply(metodos, function(cf) isTRUE(cf[[8]]), TRUE)]
  if (i %% 6 == 1) message(sprintf("%s %5.1f%% | %s", format(T, "%H:%M", tz = tz), 100 * pest,
    paste(sprintf("%s PT %.2f", com_fx, 100 * out[bloco == "PT"][match(com_fx, metodo), proj]), collapse = " | ")))
  out
}))
serie <- merge(serie, final, by = "bloco")
serie[, erro := 100 * (proj - real)]
write_parquet(serie, sprintf("dados/replay/replay_metodos_%s_2022_t%d.parquet", conjunto, tn))

# ---- comparação -----------------------------------------------------------------------------------------------
blocos_m <- if (tn == 1) c("PT", "PL") else "PT"
ordem <- c("bruto", "uf", names(metodos))
serie[, faixa := cut(pestn, c(0, 2, 5, 10, 20, 30, 50, 70, 90, 100))]
tab <- dcast(serie[bloco %in% blocos_m, .(erro = mean(abs(erro))), by = .(metodo, faixa)], faixa ~ metodo, value.var = "erro")
setcolorder(tab, c("faixa", intersect(ordem, names(tab))))
cat(sprintf("\n== %dº turno: erro absoluto médio (p.p.) por faixa de %% apurado ==\n", tn))
print(tab[, lapply(.SD, \(v) if (is.double(v)) round(v, 2) else v)])
marco <- function(lim) serie[bloco %in% blocos_m, .(ok = all(abs(erro) < lim)), by = .(metodo, hora, pestn)][
  order(hora), .(v = { r <- rev(cumprod(rev(ok))) == 1; if (any(r)) round(pestn[which(r)[1]], 1) else NA_real_ }), by = metodo]
lider <- serie[, .SD[which.max(proj)], by = .(metodo, hora, pestn)][, ok := bloco == final[which.max(real), bloco]][
  order(hora), .(lider = { r <- rev(cumprod(rev(ok))) == 1; if (any(r)) round(pestn[which(r)[1]], 1) else NA_real_ }), by = metodo]
res <- Reduce(\(x, y) merge(x, y, by = "metodo"), list(marco(1)[, .(metodo, erro_lt_1 = v)], marco(0.5)[, .(metodo, erro_lt_0.5 = v)],
                                                       marco(0.25)[, .(metodo, erro_lt_0.25 = v)], lider))
res <- merge(res, serie[bloco == "PT", .(dessinc_medio_parciais = round(mean(dessinc, na.rm = TRUE), 3)), by = metodo], by = "metodo", all.x = TRUE)
res <- res[match(ordem, metodo)]
cat("\n== a partir de que % apurado o erro fica SEMPRE abaixo do limite / o líder fica certo ==\n"); print(res)
cob <- dcast(serie[!is.na(lo) & bloco %in% blocos_m, .(cob90 = round(mean(real >= lo & real <= hi), 2)), by = .(metodo, faixa)],
             faixa ~ metodo, value.var = "cob90")
cat("\n== cobertura da faixa de 90% ==\n"); print(cob)
fwrite(tab, sprintf("docs/replay_metodos_%s_t%d_erro.csv", conjunto, tn)); fwrite(res, sprintf("docs/replay_metodos_%s_t%d_marcos.csv", conjunto, tn))

# gráfico comparativo: erro absoluto x % apurado (escala log), um painel por bloco
d <- serie[bloco %in% blocos_m & !metodo %in% c("P_mun_ua_zero")]
g <- ggplot(d, aes(pestn, pmax(abs(erro), 0.005), colour = metodo)) + geom_line(linewidth = 0.6) +
  facet_wrap(~bloco) + scale_y_log10(breaks = c(0.01, 0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10)) +
  geom_hline(yintercept = c(0.25, 0.5), linetype = "dashed", colour = "grey50") +
  labs(x = "% do eleitorado apurado", y = "erro absoluto (p.p., log)", colour = NULL,
       title = sprintf("Replay 2022, %dº turno: erro de cada método ao longo da apuração", tn)) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(sprintf("figs/replay_metodos_%s_t%d_erro.png", conjunto, tn), g, width = 11, height = 6, dpi = 120, bg = "white")
for (mt in names(metodos)[vapply(metodos, function(cf) isTRUE(cf[[8]]), TRUE)]) {
  dd <- serie[metodo == mt, .(x = pestn, bloco, apurado, proj, lo, hi)]
  fn <- final
  if (tn == 2) { dd <- dd[bloco != "OUTROS"]; fn <- final[bloco != "OUTROS"] }
  gg <- grafico_apuracao(dd, fn, titulo = sprintf("Replay 2022, %dº turno: %s", tn, mt),
                         subtitulo = "linha cheia = apurado | pontilhada = projeção com faixa de 90% | fina = resultado final",
                         rotulos = list(PT = "Lula (PT)", PL = "Bolsonaro (PL)"))
  ggsave(sprintf("figs/replay_metodos_%s_t%d_%s.png", conjunto, tn, mt), gg, width = 10, height = 6, dpi = 120, bg = "white")
}
message(sprintf("tempo total: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
