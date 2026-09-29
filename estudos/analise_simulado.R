# Análise de uma execução contra o simulado: custo do ciclo e como saber quais seções estão totalizadas.
# Uso: Rscript R/analise_simulado.R dados/simulado/<pasta da execução>
suppressPackageStartupMessages({ library(arrow); library(data.table) })

pasta  <- commandArgs(TRUE)[1]
# ciclos ficam em <pasta>/rodada_NN/ciclo_*; o nome do ciclo leva a rodada (o simulado reinicia a zero)
ciclos <- sort(grep("/ciclo_[^/]+$", list.dirs(pasta, recursive = TRUE), value = TRUE))
le <- function(d, f) { a <- file.path(d, paste0(f, ".parquet")); if (file.exists(a)) as.data.table(read_parquet(a)) }
nome_ciclo <- function(d) paste(basename(dirname(d)), basename(d), sep = "/")
por_ciclo <- function(f) rbindlist(lapply(ciclos, \(d) { x <- le(d, f); if (!is.null(x)) x[, ciclo := nome_ciclo(d)] }), fill = TRUE)

cat("\n== (c) custo por ciclo ==\n")
st <- por_ciclo("stats")
print(dcast(st, ciclo ~ etapa, value.var = "n"))
print(st[, .(req = sum(n), s200 = sum(s200), s304 = sum(s304), s404 = sum(s404), outros = sum(outros),
             seg_rede = round(sum(segundos))), by = ciclo])

cat("\n== andamento BR (ab) ==\n")
ab <- por_ciclo("ab")
print(ab[cdabr == "br" | (tpabr == "uf" & cdabr == "sp"), .(ciclo, cdabr, dg, hg, ts, st, pstn, pestn, c)])

cat("\n== (b) cs: seções com data/hora x seções totalizadas no ab, por município ==\n")
cs <- por_ciclo("cs")
if (nrow(cs)) {
  princ <- unique(cs[is.na(nsp)], by = c("ciclo", "uf", "mun", "zona", "secao"))   # agregadas têm nsp
  cs_mun <- princ[, .(secoes = .N, com_hora = sum(!is.na(ha))), by = .(ciclo, uf, mun)]
  ab_mun <- ab[tpabr == "mun", .(ciclo, mun = cdabr, ts, st)]
  x <- merge(cs_mun, ab_mun, by = c("ciclo", "mun"))
  print(x[, .(municipios = .N, secoes = sum(secoes), ts_ab = sum(ts), com_hora = sum(com_hora), st_ab = sum(st),
              mun_com_hora_igual_st = sum(com_hora == st), mun_parciais_ab = sum(st > 0 & st < ts)), by = ciclo])
  # a hora muda ao longo dos ciclos? (seções que ganharam/perderam hora)
  if (length(ciclos) > 1) {
    h <- dcast(princ[, .(ciclo, k = paste(uf, mun, zona, secao), tem = !is.na(ha))], k ~ ciclo, value.var = "tem")
    cat("seções cuja presença de hora mudou entre ciclos:",
        sum(apply(h[, -1], 1, \(r) length(unique(na.omit(r))) > 1)), "\n")
    print(princ[, .(horarios_distintos = uniqueN(paste(da, ha))), by = ciclo])
  }
}

cat("\n== (b) aux: status por seção da amostra, e concordância com cs ==\n")
aux <- por_ciclo("aux")
if (nrow(aux)) {
  print(aux[, .N, by = .(ciclo, st, n_arq)][order(ciclo)])
  j <- merge(aux[, .(ciclo, uf, mun, zona, secao, st)], cs[, .(ciclo, uf, mun, zona, secao, tem_hora = !is.na(ha))],
             by = c("ciclo", "uf", "mun", "zona", "secao"))
  print(j[, .N, by = .(ciclo, st, tem_hora)][order(ciclo)])
}

cat("\n== (d) arquivos de BU baixados ==\n")
bu <- list.files(pasta, recursive = TRUE, pattern = "[.](bu|imgbu)$")
print(table(tools::file_ext(bu)))

cat("\n== (3/4) seções não instaladas / não apuradas / anuladas: como aparecem ==\n")
ab_c <- ab[tpabr %in% c("br", "uf")]
print(ab_c[cdabr == "br", .(ciclo, ts, st, snt, si, sni, sa, sna, est, esni, esna)])
cat("-- UFs com seções não instaladas ou não apuradas (último ciclo) --\n")
ult <- ab[ciclo == max(ciclo)]
print(ult[tpabr %in% c("uf") & (sni > 0 | sna > 0), .(cdabr, ts, st, snt, sni, sna, esni, esna)])
cat("-- municípios com sni/sna > 0 (último ciclo) --\n")
print(ult[tpabr == "mun" & (sni > 0 | sna > 0), .(cdabr, ts, st, snt, sni, sna, pstn)])
cm <- tryCatch(jsonlite::fromJSON(file.path(pasta, "cm.json")), error = \(e) NULL)
if (!is.null(cm)) {
  ba <- cm$abr[cm$abr$cd == "ba", "mu"][[1]]
  ib <- ba[grepl("IBIRAPU", ba$nm), "cd"]
  cat("-- Ibirapuã/BA (", ib, ") ao longo dos ciclos --\n")
  print(ab[tpabr == "mun" & cdabr %in% ib, .(ciclo, ts, st, snt, si, sni, sa, sna, pstn)])
  if (nrow(cs)) print(cs[mun %in% ib & zona == "0153", .N, by = .(ciclo, tem_hora = !is.na(ha))])
}
if (nrow(aux)) { cat("-- status (st) das seções da amostra aux, todos os valores vistos --\n"); print(aux[, .N, by = st]) }
u <- por_ciclo("u_cand")
if (nrow(u)) { cat("-- destinação dos votos (dvt) dos candidatos, BR, último ciclo --\n")
  print(u[tpabr == "br" & ciclo == max(ciclo), .(n, nm = substr(nm, 1, 25), vap, dvt)][order(-vap)]) }
