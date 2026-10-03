# Replay da apuração de 2022 "como se fosse a eleição": a cada instante monta, com os dados reais de 2022 e na ordem
# real de recebimento dos boletins, os mesmos arquivos que a noite recebe do TSE (resultado por município,
# candidatos e arquivo de seções) e roda o MESMO código da noite (estado, projeção, trajetória, página).
# Base: 2018 levado às seções de 2022 (preparo/04_base_replay2022.R).
#
# Uso: Rscript R/replay_2022.R [turno = 1] [passo_min = 5] [pausa_s = 0] [B = 100] [publicar = nao] [ate_pct = 100]
#   pausa_s > 0 espera entre instantes (para acompanhar na página); publicar = sim roda R/publicar.R a cada instante.
args <- commandArgs(TRUE)
setwd(normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), "..")))
source("R/modelo.R"); source("R/pagina.R"); source("R/publicar.R"); source("R/pesquisas.R")
tn    <- if (length(args) >= 1) as.integer(args[1]) else 1L
passo <- if (length(args) >= 2) as.numeric(args[2]) else 5
pausa <- if (length(args) >= 3) as.numeric(args[3]) else 0
B     <- if (length(args) >= 4) as.integer(args[4]) else 100L
pub   <- length(args) >= 5 && args[5] == "sim"
ate   <- if (length(args) >= 6) as.numeric(args[6]) else 101          # para ao passar deste % apurado (testes)
tz <- "America/Sao_Paulo"
rd <- function(f) as.data.table(read_parquet(file.path("dados/base", f)))
logmsg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)

base <- carregar_base(tn, secoes = "replay2022_secao", municipios = "replay2022_municipios")
bl <- read_yaml("config/blocos.yaml")$eleicoes[["2022"]]
prior <- prior_swing(base, arq = sprintf("config/prior_2022_%dt.yaml", tn))
arq_pesq_uf <- sprintf("config/pesquisas_uf_2022_%dt.csv", tn)

s <- rd("secoes_2022.parquet")[turno == tn, .(uf, mun, zona, secao, aptos, comparecimento, dt_recebido)]
s[, sid := .I]
v <- rd("votos_secao_2022.parquet")[turno == tn & !nr_votavel %in% c(95, 96)]
v <- merge(v, s[, .(uf, mun, zona, secao, sid)], by = c("uf", "mun", "zona", "secao"))[, .(sid, mun, n = as.character(nr_votavel), votos)]
cs_fixo <- s[, .(uf = tolower(uf), mun = sprintf("%05d", mun), zona = sprintf("%04d", zona), secao = sprintf("%04d", secao), nsp = NA_character_)]

dia <- if (tn == 1) "2022-10-02" else "2022-10-30"
inicio <- as.POSIXct(paste(dia, "17:00:00"), tz = tz)
tt <- sort(s$dt_recebido); fim_noite <- tt[ceiling(0.999 * length(tt))]
instantes <- unique(c(seq(inicio, fim_noite, by = passo * 60), fim_noite))

# os arquivos que o TSE publicaria no instante T
ciclo_em <- function(T) {
  rec <- s$dt_recebido <= T
  u_tot <- s[, .(tpabr = "mu", cdabr = sprintf("%05d", mun[1]), ts = .N, st = sum(rec[sid]), te = sum(aptos),
                 est = sum(aptos[rec[sid]]), c = sum(comparecimento[rec[sid]])), by = mun][, mun := NULL]
  u_cand <- v[rec[sid], .(vap = sum(votos)), by = .(mun, n)][, .(tpabr = "mu", cdabr = sprintf("%05d", mun), n, vap, dvt = "Válido")]
  cs <- copy(cs_fixo)[, ha := fifelse(rec, format(s$dt_recebido, "%H:%M:%S", tz = tz), NA_character_)]
  list(u_tot = u_tot, u_cand = u_cand, cs = cs, rodada = 1L)
}

centro <- NULL; serie <- data.table(); hist_uf <- data.table()
logmsg(sprintf("replay 2022, %dº turno: %d instantes de %s a %s", tn, length(instantes),
               format(inicio, "%H:%M", tz = tz), format(fim_noite, "%H:%M", tz = tz)))
for (T in as.list(instantes)) {
  t0 <- Sys.time()
  m <- estado(ciclo_em(T), base, bl)
  if (is.null(centro)) centro <- centro_nacional(m, tn)
  hist_uf <- rbind(hist_uf, m[, .(hora = T, f = sum(aptos_obs) / sum(aptos)), by = uf])
  if (sum(m$aptos_obs) == 0) next
  fx <- projetar_faixa(m, tn, prior, centro, B = B)
  mostrar <- fx$pct_apurado[1] / 100 >= PAR_MOD$mostrar_a_partir
  traj <- if (mostrar) trajetoria(m, attr(fx, "est"), centro, ritmo_uf(hist_uf, T))
  obs <- m[, c(sum(obs_PT), sum(obs_PL), sum(obs_OU)) / sum(obs_val)]
  na_se <- function(x) if (mostrar) x else rep(NA_real_, length(x))
  serie <- rbind(serie, fx[, .(hora = T, bloco, apurado = obs, proj = na_se(proj), lo = na_se(lo), hi = na_se(hi),
                               p_2turno = na_se(p_2turno), pestn = pct_apurado,
                               prelim = if (mostrar) NA_real_ else proj)])
  escrever_pagina(serie, "Presidente 2022 (replay)", tn, "replay", traj = traj, preliminar = if (!mostrar) setNames(fx$proj, fx$bloco),
                 estados = projetar_uf(m, attr(fx, "est"), centro), pesq_uf = pesquisas_uf(arq_pesq_uf))
  if (pub) publicar_site(esperar = FALSE)
  logmsg(sprintf("%s %5.1f%% | PT %.2f PL %.2f%s | %.0f s", format(T, "%H:%M", tz = tz), fx$pct_apurado[1], 100 * obs[1], 100 * obs[2],
                 if (mostrar) sprintf(" -> %.2f / %.2f", 100 * fx$proj[1], 100 * fx$proj[2]) else "", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (fx$pct_apurado[1] >= ate) break
  if (pausa > 0) Sys.sleep(max(0, pausa - as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
logmsg("fim do replay")
