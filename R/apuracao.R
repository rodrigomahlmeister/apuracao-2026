# Apuração com projeção — Presidente 2026. Único script da noite da eleição.
#
# Uso (da raiz do projeto):
#   Rscript R/apuracao.R                         # oficial, 1º turno, até ser interrompido (Ctrl+C)
#   Rscript R/apuracao.R oficial 1 23:59         # ambiente, turno, hora de parar (Brasília)
#   Rscript R/apuracao.R simulado 1 16:10
#   Rscript R/apuracao.R oficial 1 23:59 sem_publicar   # não envia ao site (só grava o JSON local)
#
# A cada ciclo (~1–2 min): coleta os arquivos do TSE, monta o estado de cada município, projeta o resultado
# final com faixa de 90% e grava:
#   saida/<ambiente>_t<turno>_<data_hora>/   log.txt, serie.csv, ciclos/<ciclo>/bruto.parquet
#   site/dados/apuracao.json                  o que a página lê (publicado a cada ciclo)
args <- commandArgs(TRUE)
setwd(normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), "..")))
source("R/ingestao.R"); source("R/modelo.R"); source("R/pagina.R"); source("R/publicar.R")
publicar_no_ar <- !("sem_publicar" %in% args)     # argumento extra "sem_publicar": só grava o JSON local

p <- carregar_params()
if (length(args) >= 1) { p$ambiente <- args[1]; p$raiz <- p$ambientes[[p$ambiente]]$raiz }
turno <- if (length(args) >= 2) as.integer(args[2]) else 1L
tz <- "America/Sao_Paulo"
fim <- if (length(args) >= 3) as.POSIXct(paste(format(Sys.time(), "%Y-%m-%d", tz = tz), args[3]), tz = tz) else as.POSIXct(Inf)

out <- file.path("saida", sprintf("%s_t%d_%s", p$ambiente, turno, format(Sys.time(), "%Y-%m-%d_%H%M", tz = tz)))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
zz <- file(file.path(out, "log.txt"), open = "at"); sink(zz, type = "message"); sink(zz, type = "output", split = TRUE)

ctx <- contexto(p, turno)
cm <- parse_cm(baixar(url_cm(ctx), p, condicional = FALSE)$corpo[[1]])
base <- carregar_base(turno)
prior <- prior_swing(base)
blocos <- read_yaml("config/blocos.yaml")
bl <- blocos$eleicoes[[if (p$ambiente == "simulado") "simulado" else "2026"]]
centro <- NULL
logmsg(sprintf("ambiente %s | %dº turno | eleição %s, pleito %s | %d municípios | prior: %s", p$ambiente, turno,
               ctx$cd_eleicao, ctx$cd_pleito, nrow(cm), prior$fonte))

serie <- data.table()
hist_uf <- data.table()          # fração apurada de cada UF por ciclo, para medir o ritmo (trajetória)
publicar <- function(fx, ciclo, m, traj) {
  obs <- m[, c(sum(obs_PT), sum(obs_PL), sum(obs_OU)) / max(sum(obs_val), 1)]
  pstn <- 100 * sum(m$st) / sum(m$ts)
  mostrar <- fx$pct_apurado[1] / 100 >= PAR_MOD$mostrar_a_partir
  na_se <- function(v) if (mostrar) v else rep(NA_real_, length(v))    # projeção só a partir de 2% apurado
  lin <- fx[, .(hora = Sys.time(), rodada = ciclo$rodada, bloco, apurado = obs, proj = na_se(proj), lo = na_se(lo),
                hi = na_se(hi), p_2turno = na_se(p_2turno), pestn = pct_apurado, pstn = pstn)]
  serie <<- rbind(if (nrow(serie)) serie[rodada == ciclo$rodada], lin)    # reinício do simulado: série nova
  fwrite(lin, file.path(out, "serie.csv"), append = file.exists(file.path(out, "serie.csv")))
  escrever_pagina(serie, eleicao = if (p$ambiente == "simulado") "Simulado TSE 2026 · Presidente" else "Eleições 2026 · Presidente",
                  turno = turno, ambiente = p$ambiente, traj = if (mostrar) traj,
                  preliminar = if (!mostrar && sum(m$aptos_obs) > 0) setNames(fx$proj, fx$bloco))
  if (publicar_no_ar) publicar_site(esperar = FALSE)            # envia em paralelo; não segura o ciclo
  logmsg(sprintf("apurado %.2f%% | %s", fx$pct_apurado[1], paste(sprintf("%s %.2f%s", fx$bloco, 100 * obs,
    if (mostrar) sprintf(" -> %.2f [%.2f-%.2f]", 100 * fx$proj, 100 * fx$lo, 100 * fx$hi) else ""), collapse = " | ")))
}

k <- 0
repeat {
  k <- k + 1; t0 <- Sys.time()
  nome <- format(t0, "ciclo_%H%M%S", tz = tz)
  res <- tryCatch({
    ciclo <- coletar(ctx, p, cm, file.path(out, "ciclos", nome))
    if (nzchar(Sys.getenv("APURACAO_SALVAR_CICLO"))) saveRDS(ciclo, Sys.getenv("APURACAO_SALVAR_CICLO"))
    m <- estado(ciclo, base, bl)
    if (is.null(centro)) centro <<- centro_nacional(m, turno)
    agora <- Sys.time()
    hist_uf <<- rbind(if (nrow(hist_uf)) hist_uf[rodada == ciclo$rodada],
                      m[, .(hora = agora, rodada = ciclo$rodada, f = sum(aptos_obs) / sum(aptos)), by = uf])
    if (sum(m$aptos_obs) > 0) {
      fx <- projetar_faixa(m, turno, prior, centro)
      traj <- trajetoria(m, attr(fx, "est"), centro, ritmo_uf(hist_uf, agora))
      publicar(fx, ciclo, m, traj)
    } else logmsg("nenhuma seção apurada ainda")
    ciclo$n_req
  }, error = \(e) { logmsg("ERRO no ciclo: ", conditionMessage(e)); NA })
  dur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  logmsg(sprintf("ciclo %d: %s requisições, %.0f s", k, res, dur))
  if (Sys.time() >= fim) break
  espera <- p$ciclo$intervalo_min * 60 - dur
  if (espera > 0) Sys.sleep(espera)
}
logmsg("fim")
