# Roda ciclos contra o simulado até `fim` (hora de Brasília) e grava tudo em dados/simulado/<data>/.
# Uso: Rscript R/run_simulado.R [HH:MM fim, padrão 16:10]
setwd(normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), "..")))
source("estudos/03_ingestao.R")

args <- commandArgs(TRUE)
tz   <- "America/Sao_Paulo"
fim  <- as.POSIXct(paste(format(Sys.time(), "%Y-%m-%d", tz = tz), if (length(args)) args[1] else "16:10"), tz = tz)

p   <- carregar_params()
out <- file.path("dados", p$ambiente, format(Sys.time(), "%Y-%m-%d_%H%M", tz = tz))   # uma pasta por execução
dir.create(out, recursive = TRUE, showWarnings = FALSE)
sink(file.path(out, "log.txt"), append = TRUE, split = TRUE, type = "output")
zz <- file(file.path(out, "log.txt"), open = "at"); sink(zz, type = "message")

ctx <- contexto(p)
logmsg("ambiente=", p$ambiente, " eleição=", ctx$eleicao, " pleito=", ctx$pleito, " fim=", format(fim, tz = tz))
writeLines(ctx$elec_bruto$corpo[[1]], file.path(out, "ele-c.json"))
r_cm <- baixar(url_cm(ctx), p, condicional = FALSE); stopifnot(r_cm$status == 200)
writeLines(r_cm$corpo[[1]], file.path(out, "cm.json"))
cm <- parse_cm(r_cm$corpo[[1]])

# amostra fixa de seções das cidades de teste (do cs), para checar status (aux) e baixar BU
set.seed(2026)
secoes_aux <- rbindlist(lapply(p$cidades_teste, \(cid) {
  r <- baixar(url_cs(ctx, cid$uf), p, condicional = FALSE)
  s <- parse_cs(r$corpo[[1]])[mun == cid$mun]
  s <- s[sample(.N, min(.N, p$aux_amostra_por_cidade))]
  s[, bu := seq_len(.N) <= p$bu_secoes_por_cidade]
}))
logmsg("amostra aux: ", nrow(secoes_aux), " seções em ", length(p$cidades_teste), " cidades")

k <- 0
repeat {
  k <- k + 1
  t0 <- Sys.time()
  nome_c <- format(t0, "ciclo_%H%M%S", tz = tz)
  logmsg("== ciclo ", k, " (", nome_c, ")")
  st <- tryCatch(rodar_ciclo(ctx, p, cm, out, nome_c, secoes_aux),
                 error = \(e) { logmsg("ERRO no ciclo: ", conditionMessage(e)); NULL })
  dur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!is.null(st)) logmsg(sprintf("ciclo %d: %d requisições em %.0f s (%.1f req/s)", k, sum(st$n), dur, sum(st$n) / dur))
  if (Sys.time() >= fim) break
  espera <- p$ciclo$intervalo_min * 60 - dur
  if (espera > 0) Sys.sleep(min(espera, as.numeric(difftime(fim, Sys.time(), units = "secs"))))
  if (Sys.time() >= fim) break
}
logmsg("fim")
