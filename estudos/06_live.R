# Projeção ao vivo a partir dos ciclos gravados pela ingestão (R/03_ingestao.R).
# Uso: Rscript R/06_live.R <pasta da execução ou da rodada> [ano_blocos = 2026] [prior = config/prior.yaml]
#   processa todos os ciclos da pasta (ordem cronológica), grava serie_projecao.parquet e um PNG por ciclo
#   em <pasta>/figs/. Pode rodar em paralelo à ingestão (só lê arquivos já gravados).
# Base: pareamento 2026 <- 2022 (locais) + resultados 2022 por local; 1º turno.
# Parte pendente: variante B (base dos locais das seções sem data/hora no cs), se o cs estiver no ciclo;
# senão variante A (base do município inteiro).
source("estudos/04_modelo.R")
source("estudos/06_grafico.R")
source("estudos/03_ingestao.R")

TURNO <- 1L

# ---- base 2026 por seção (fixa; calculada uma vez) ---------------------------------------------------
preparar_base_2026 <- function(cache = "dados/base/base_live_2026.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  par <- list(pesos = rd("pareamento_2026_2022_pesos.parquet"), nivel = rd("pareamento_2026_2022_nivel.parquet"))
  fontes <- list(rd("locais_2024.parquet"), rd("locais_2026.parquet"))
  par$alvo <- prep(locais_alvo_2026())[, t_id := .I]
  par$base <- prep(imputar_coord(locais_base(2022), fontes)[aptos > 0])[, b_id := .I]
  bl <- base_projetada(par, rd("hist_local_2022.parquet")[turno == TURNO])
  bl <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], bl, by = "t_id")[, t_id := NULL]
  setnames(bl, c("comparecimento", "validos", "PT", "PL", "OUTROS"), c("pa_comp", "pa_val", "pa_PT", "pa_PL", "pa_OU"))
  bl[, aptos := NULL]
  # seções 2026 (inclui agregadas; a agregada aponta para a principal)
  sec <- rd("eleitorado_secao_2026.parquet")[, .(uf, mun, zona, secao, local, eleitores, secao_principal, tipo_agregada)]
  sec <- merge(sec, bl, by = c("uf", "mun", "zona", "local"), all.x = TRUE)
  for (g in list(c("uf", "mun"), "uf")) {
    med <- sec[!is.na(pa_comp), lapply(.SD, \(x) weighted.mean(x, eleitores)), by = g, .SDcols = patterns("^pa_")]
    sec[med, on = g, `:=`(pa_comp = fcoalesce(pa_comp, i.pa_comp), pa_val = fcoalesce(pa_val, i.pa_val),
                          pa_PT = fcoalesce(pa_PT, i.pa_PT), pa_PL = fcoalesce(pa_PL, i.pa_PL), pa_OU = fcoalesce(pa_OU, i.pa_OU))]
  }
  sec[, `:=`(b_comp = eleitores * pa_comp, b_val = eleitores * pa_val, b_PT = eleitores * pa_PT,
             b_PL = eleitores * pa_PL, b_OU = eleitores * pa_OU)]
  # seção que conta no cs = principal (agregadas somam na principal)
  sec[, secao_cs := fifelse(!is.na(secao_principal) & secao_principal > 0 & tipo_agregada != 1, secao_principal, secao)]
  mi <- rd("municipios_ibge.parquet")[, .(uf, mun, regiao)]
  elg <- merge(par$nivel[, .(uf, mun, aptos, n1 = nivel == "1")], mi, by = c("uf", "mun"), all.x = TRUE)[
    , .(elegivel = sum(aptos[n1]) / sum(aptos) >= PAR_MOD$nivel1_min, regiao = regiao[1]), by = .(uf, mun)]
  elg[uf == "ZZ" | is.na(regiao), regiao := "ZZ"]
  out <- list(sec = sec, elg = elg)
  saveRDS(out, cache)             # apague o arquivo se o pareamento ou as bases mudarem
  out
}

# ---- estado dos municípios num ciclo ---------------------------------------------------------------------
estado_ciclo <- function(dir, base, blocos) {
  le <- function(f) { a <- file.path(dir, paste0(f, ".parquet")); if (file.exists(a)) as.data.table(read_parquet(a)) }
  ut <- le("u_tot")[tpabr == "mu"]; uc <- le("u_cand")[tpabr == "mu"]; cs <- le("cs")
  # mesma regra de votos_blocos(), vetorizada por município: só destinação válida entra nos blocos
  pt <- as.character(blocos$PT); pl <- as.character(blocos$PL)
  vb <- uc[, .(PT = sum(vap[e_valido(dvt) & n %in% pt]), PL = sum(vap[e_valido(dvt) & n %in% pl]),
               OUTROS = sum(vap[e_valido(dvt) & !n %in% c(pt, pl)])), by = .(mun = cdabr)]
  m <- merge(ut[, .(mun = cdabr, ts, st, te, est, c, vv)], vb, by = "mun", all.x = TRUE)
  m[, mun := as.integer(mun)]
  bt <- base$sec[, .(uf = uf[1], aptos_base = sum(eleitores), bt_comp = sum(b_comp), bt_val = sum(b_val),
                     bt_PT = sum(b_PT), bt_PL = sum(b_PL), bt_OU = sum(b_OU)), by = mun]
  m <- merge(m, bt, by = "mun")
  m[, `:=`(aptos = te, aptos_obs = est, completo = st >= ts, obs_comp = c, obs_val = fcoalesce(PT + PL + OUTROS, 0),
           obs_PT = fcoalesce(PT, 0), obs_PL = fcoalesce(PL, 0), obs_OU = fcoalesce(OUTROS, 0))]
  # base reescalada ao eleitorado da divulgação (te pode diferir levemente do arquivo de eleitorado)
  f_tot <- m$aptos / m$aptos_base
  for (k in c("comp", "val", "PT", "PL", "OU")) set(m, j = paste0("bt_", k), value = m[[paste0("bt_", k)]] * f_tot)
  if (!is.null(cs) && nrow(cs)) {
    # variante B: base das seções pendentes segundo o cs, reconciliada PROPORCIONALMENTE com o -u
    # (n_u = seções totalizadas no -u; n_c = seções principais com data/hora no cs; n = seções principais):
    #   n_c = n_u: seção com hora = apurada; n_c < n_u (cs atrás): com hora conta 1, sem hora conta
    #   (n_u - n_c)/(n - n_c); n_c > n_u (cs à frente): com hora conta n_u/n_c. Não depende da ordem.
    csp <- unique(cs[is.na(nsp), .(uf = toupper(uf), mun = as.integer(mun), zona = as.integer(zona),
                                  secao_cs = as.integer(secao), tem = !is.na(ha))])
    bsec <- base$sec[, .(b_comp = sum(b_comp), b_val = sum(b_val), b_PT = sum(b_PT), b_PL = sum(b_PL), b_OU = sum(b_OU)),
                     by = .(uf, mun, zona, secao_cs)]
    # junção pela esquerda: contagens n_c e n vêm do cs inteiro; seção do cs sem par no eleitorado (numeração
    # diferente) recebe a base média por seção do município
    x <- merge(csp, bsec, by = c("uf", "mun", "zona", "secao_cs"), all.x = TRUE)
    bcols <- c("b_comp", "b_val", "b_PT", "b_PL", "b_OU")
    x[, (bcols) := lapply(.SD, \(v) fcoalesce(v, mean(v, na.rm = TRUE))), by = mun, .SDcols = bcols]
    x <- x[!is.na(b_comp)]
    x <- merge(x, m[, .(mun, n_u = st)], by = "mun")
    x[, `:=`(n_c = sum(tem), n = .N), by = mun]
    x[, w_ap := fcase(n_c == n_u, as.numeric(tem),
                      n_c <  n_u, fifelse(tem, 1, (n_u - n_c) / pmax(n - n_c, 1)),
                      default = fifelse(tem, n_u / n_c, 0))]
    pb <- x[, .(pB_comp = sum(b_comp * (1 - w_ap)), pB_val = sum(b_val * (1 - w_ap)), pB_PT = sum(b_PT * (1 - w_ap)),
                pB_PL = sum(b_PL * (1 - w_ap)), pB_OU = sum(b_OU * (1 - w_ap)), n_c = n_c[1], n_cs = n[1]), by = mun]
    m <- merge(m, pb, by = "mun", all.x = TRUE)
    m[, dessinc := !is.na(n_c) & n_c != st]
    for (k in c("comp", "val", "PT", "PL", "OU")) {       # mesma escala da base total (f_tot)
      pk <- paste0("pB_", k); set(m, j = paste0("bp_", k), value = fcoalesce(m[[pk]], 0) * f_tot * fifelse(m$completo, 0, 1))
    }
    variante <- "B"
  } else {
    f <- pmax(m$aptos - m$aptos_obs, 0) / m$aptos
    for (k in c("comp", "val", "PT", "PL", "OU")) set(m, j = paste0("bp_", k), value = m[[paste0("bt_", k)]] * f)
    variante <- "A"
  }
  m <- merge(m, base$elg, by = c("uf", "mun"), all.x = TRUE)[is.na(elegivel), elegivel := FALSE][is.na(regiao), regiao := "ZZ"]
  list(m = m, variante = variante)
}

if (sys.nframe() == 0) {
  a <- commandArgs(TRUE)
  pasta <- a[1]; ano_bl <- if (length(a) >= 2) a[2] else "2026"
  blocos <- read_yaml("config/blocos.yaml")$eleicoes[[ano_bl]]
  base <- preparar_base_2026()
  base_nac <- base$sec[, .(PT = sum(b_PT), PL = sum(b_PL), OU = sum(b_OU))]
  prior <- prior_swing(if (length(a) >= 3) a[3] else "config/prior.yaml", base_nac, TURNO)
  ciclos <- sort(grep("/ciclo_[^/]+$", list.dirs(pasta, recursive = TRUE), value = TRUE))
  dir.create(file.path(pasta, "figs"), showWarnings = FALSE)
  serie <- rbindlist(lapply(ciclos, \(d) {
    e <- estado_ciclo(d, base, blocos); m <- e$m
    centro <- centro_nacional(m, TURNO)
    fx <- projetar_faixa(m, TURNO, prior, centro, unidades = if (e$variante == "B") "apurado" else "completos")
    obs <- m[, c(PT = sum(obs_PT), PL = sum(obs_PL), OU = sum(obs_OU)) / max(sum(obs_val), 1)]
    fx[, `:=`(ciclo = basename(d), rodada = basename(dirname(d)), variante = e$variante,
              pestn = 100 * sum(m$aptos_obs) / sum(m$aptos), pstn = 100 * sum(m$st) / sum(m$ts), apurado = obs)]
    logmsg(sprintf("%s  %.1f%% apurado  proj PT %.1f [%.1f-%.1f] PL %.1f  P(2T) %.2f", basename(d), fx$pestn[1],
                   100 * fx$proj[1], 100 * fx$lo[1], 100 * fx$hi[1], 100 * fx$proj[2], fx$p_2turno[1]))
    fx
  }))
  write_parquet(serie, file.path(pasta, "serie_projecao.parquet"))
  for (r in unique(serie$rodada)) {
    d <- serie[rodada == r, .(x = pestn, bloco, apurado, proj, lo, hi)]
    ult <- serie[rodada == r][.N]
    g <- grafico_apuracao(d, titulo = sprintf("Presidente 2026 (%s): projeção ao vivo", r),
                          subtitulo = sprintf("%.2f%% das seções totalizadas | %s | P(2º turno) = %.2f | parte pendente: variante %s",
                                              ult$pstn, ult$ciclo, ult$p_2turno, ult$variante))
    ggsave(file.path(pasta, "figs", sprintf("projecao_%s.png", r)), g, width = 10, height = 6, dpi = 120, bg = "white")
  }
}
