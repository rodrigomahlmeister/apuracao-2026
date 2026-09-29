# Sonda (janela de 28/09): resultado por zona eleitoral e sincronização cs x -u com o cs baixado logo antes.
# Uso: Rscript R/sonda_zona.R <pasta da execução> [iteracoes = 6] [intervalo_s = 90]
source("estudos/03_ingestao.R")
a <- commandArgs(TRUE)
out <- file.path(a[1], "sonda_zona"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
n_it <- if (length(a) >= 2) as.integer(a[2]) else 6L
intervalo <- if (length(a) >= 3) as.numeric(a[3]) else 90

p <- carregar_params(); p$http$req_por_seg <- 10; p$http$max_ativos <- 5
ctx <- contexto(p)
cm <- parse_cm(paste(readLines(file.path(a[1], "cm.json"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
cm[, nz := lengths(strsplit(zonas, ","))]
cid <- cm[capital == TRUE][order(-nz)][1:5]
logmsg("sonda: ", paste(sprintf("%s-%s (%d zonas)", cid$uf, cid$nm, cid$nz), collapse = "; "))
zonas <- cid[, .(zona = strsplit(zonas, ",")[[1]]), by = .(uf, mun, nm)]
url_zona <- function(uf, mun, zona) sprintf("%s/%s%s-z%s-c%04d-%s-u.json", dir_de(ctx, "u", uf), uf, mun, zona,
                                             as.integer(ctx$cargo), e6(ctx))
zonas[, url := mapply(url_zona, uf, mun, zona)]

for (it in seq_len(n_it)) {
  t0 <- Sys.time()
  r_cs <- baixar(url_cs(ctx, unique(cid$uf)), p, condicional = FALSE)          # 1) cs primeiro
  t_cs <- Sys.time()
  r_z  <- baixar(zonas$url, p, condicional = FALSE)                              # 2) -u das zonas
  r_m  <- baixar(url_u(ctx, cid$uf, cid$mun), p, condicional = FALSE)            # 3) -u dos municípios
  cs <- rbindlist(lapply(r_cs$corpo[which(r_cs$status == 200)], parse_cs))
  cs <- unique(cs[is.na(nsp)], by = c("uf", "mun", "zona", "secao"))[mun %in% cid$mun]
  csz <- cs[, .(secoes_cs = .N, hora_cs = sum(!is.na(ha))), by = .(mun, zona)]
  uz <- rbindlist(Map(\(u, txt) { x <- parse_u(txt)$tot; x[, url := u] }, r_z$url[which(r_z$status == 200)], r_z$corpo[which(r_z$status == 200)]))
  uz <- merge(zonas[, .(url, mun, zona)], uz[, .(url, st_z = st, ts_z = ts, hg_z = hg)], by = "url")
  um <- rbindlist(lapply(r_m$corpo[which(r_m$status == 200)], \(t) parse_u(t)$tot))[, .(mun = cdabr, st_m = st, ts_m = ts, hg_m = hg)]
  x <- merge(merge(uz, csz, by = c("mun", "zona"), all = TRUE), um, by = "mun", all.x = TRUE)
  x[, `:=`(iter = it, hora = format(t0, "%H:%M:%S"), seg_cs_ate_zona = as.numeric(difftime(Sys.time(), t_cs, units = "secs")))]
  fwrite(x, file.path(out, "sonda.csv"), append = it > 1)
  s <- x[, .(zonas = .N, st_z = sum(st_z, na.rm = TRUE), hora_cs = sum(hora_cs, na.rm = TRUE),
             zonas_iguais = sum(st_z == hora_cs, na.rm = TRUE), zonas_parciais = sum(st_z > 0 & st_z < ts_z, na.rm = TRUE),
             parciais_iguais = sum(st_z > 0 & st_z < ts_z & st_z == hora_cs, na.rm = TRUE),
             soma_zonas_eq_mun = all(tapply(st_z, mun, sum) == tapply(st_m, mun, `[`, 1)))]
  logmsg(sprintf("it %d | zonas %d | st_zonas %d | hora_cs %d | zonas cs==u %d | parciais %d (iguais %d) | soma zonas = mun: %s | %d req",
                 it, s$zonas, s$st_z, s$hora_cs, s$zonas_iguais, s$zonas_parciais, s$parciais_iguais, s$soma_zonas_eq_mun,
                 nrow(r_cs) + nrow(r_z) + nrow(r_m)))
  if (it < n_it) Sys.sleep(max(0, intervalo - as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
