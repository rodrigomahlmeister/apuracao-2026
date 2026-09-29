# Checagem do formato: reprocessa os arquivos BRUTOS gravados na janela do simulado do TSE (28/09/2026) com o
# código de produção (parsers de R/ingestao.R, estado/projeção/trajetória de R/modelo.R, página de R/pagina.R).
# Cada ciclo gravou só os arquivos que mudaram; aqui o estado completo é reconstruído acumulando o último corpo
# de cada URL, dentro de cada rodada (o simulado foi reiniciado duas vezes).
# Uso: Rscript estudos/replay_simulado.R [pasta = dados/simulado/2026-09-28_1358] [rodada = rodada_03]
source("R/ingestao.R"); source("R/modelo.R"); source("R/pagina.R")
a <- commandArgs(TRUE)
pasta <- if (length(a) >= 1) a[1] else "dados/simulado/2026-09-28_1358"
rodada <- if (length(a) >= 2) a[2] else "rodada_03"
ciclos <- sort(list.dirs(file.path(pasta, rodada), recursive = FALSE))
if (length(a) >= 3) ciclos <- ciclos[seq_len(as.integer(a[3]))]      # só os N primeiros (depuração)

base <- carregar_base(1)
bl <- read_yaml("config/blocos.yaml")$eleicoes$simulado
prior <- prior_swing(base)
ultimo <- new.env()                     # último corpo de cada URL
centro <- NULL; hist_uf <- data.table(); res <- list()

for (d in ciclos) {
  b <- as.data.table(read_parquet(file.path(d, "bruto.parquet")))
  for (i in seq_len(nrow(b))) assign(b$url[i], list(etapa = b$etapa[i], corpo = b$corpo[i]), envir = ultimo)
  arq <- mget(ls(ultimo), envir = ultimo)
  et <- vapply(arq, `[[`, "", "etapa")
  corpo <- lapply(arq, `[[`, "corpo")
  us <- lapply(corpo[et %in% c("u_mun", "u_uf")], parse_u)
  ciclo <- list(u_tot = rbindlist(lapply(us, `[[`, "tot"), fill = TRUE), u_cand = rbindlist(lapply(us, `[[`, "cand"), fill = TRUE),
                cs = rbindlist(lapply(corpo[et == "cs"], parse_cs), fill = TRUE), rodada = 1L)
  m <- estado(ciclo, base, bl)
  hora <- as.POSIXct(sub("ciclo_", "", basename(d)), format = "%H%M%S", tz = "America/Sao_Paulo")
  hist_uf <- rbind(hist_uf, m[, .(hora, f = sum(aptos_obs) / sum(aptos)), by = uf])
  parc <- m[aptos_obs > 0 & !completo]
  linha <- data.table(ciclo = basename(d), municipios = nrow(m), pestn = 100 * sum(m$aptos_obs) / sum(m$aptos),
                      parciais = nrow(parc), dessinc = sum(parc$dessinc, na.rm = TRUE),
                      validos_ok = isTRUE(all.equal(sum(m$obs_val), sum(ciclo$u_tot[tpabr == "mu", vv], na.rm = TRUE))))
  if (sum(m$aptos_obs) > 0) {
    if (is.null(centro)) centro <- centro_nacional(m, 1)
    fx <- projetar_faixa(m, 1, prior, centro, B = 30)
    traj <- trajetoria(m, attr(fx, "est"), centro, ritmo_uf(hist_uf, hora))
    linha[, `:=`(proj_PT = 100 * fx$proj[1], lo_PT = 100 * fx$lo[1], hi_PT = 100 * fx$hi[1], pontos_traj = nrow(traj),
                 traj_fim_ok = abs(tail(traj$PT, 1) - fx$proj[1]) < 1e-6)]
    serie <- fx[, .(hora, bloco, apurado = m[, c(sum(obs_PT), sum(obs_PL), sum(obs_OU)) / sum(obs_val)], proj, lo, hi, p_2turno, pestn = pct_apurado)]
    escrever_pagina(serie, "Simulado", 1, "simulado", traj = traj, arq = file.path(tempdir(), "pagina_teste.json"))
  }
  res[[d]] <- linha
}
r <- rbindlist(res, fill = TRUE)
fwrite(r, file.path(pasta, sprintf("checagem_producao_%s.csv", rodada)))
print(r[, lapply(.SD, \(v) if (is.double(v)) round(v, 2) else v)], nrows = 100)
cat(sprintf("\nciclos: %d | com projeção: %d | válidos por bloco = válidos do arquivo em todos: %s | trajetória termina na projeção: %s\n",
            nrow(r), r[!is.na(proj_PT), .N], all(r$validos_ok), all(r$traj_fim_ok, na.rm = TRUE)))
