# No simulado de 2026, 7% das seções do cs não têm par na base de 2026 (números de seção mais altos, espalhadas
# pelo país) e recebem a base média do município. Este teste mede o custo disso no replay de 2022: remove da base as
# seções de número mais alto de cada zona até 7% do total (como no simulado: números acima do último da zona) e compara o erro com a base completa.
# Uso: Rscript estudos/teste_cs_sem_par.R [turno = 1] [passo_min = 10] [frac = 0.07]
source("R/modelo.R")
a <- commandArgs(TRUE)
tn <- if (length(a) >= 1) as.integer(a[1]) else 1L
passo <- if (length(a) >= 2) as.numeric(a[2]) else 10
frac <- if (length(a) >= 3) as.numeric(a[3]) else 0.07
tz <- "America/Sao_Paulo"
rd <- function(f) as.data.table(read_parquet(file.path("dados/base", f)))
base <- carregar_base(tn, secoes = "replay2022_secao", municipios = "replay2022_municipios")
base_red <- copy(base)
base_red$sec <- base$sec[order(uf, mun, zona, -secao_cs)][, r := seq_len(.N) / .N, by = .(uf, mun, zona)][r > frac][, r := NULL][]
bl <- read_yaml("config/blocos.yaml")$eleicoes[["2022"]]
prior <- prior_swing(base, arq = sprintf("config/prior_2022_%dt.yaml", tn))
s <- rd("secoes_2022.parquet")[turno == tn, .(uf, mun, zona, secao, aptos, comparecimento, dt_recebido)][, sid := .I]
v <- rd("votos_secao_2022.parquet")[turno == tn & !nr_votavel %in% c(95, 96)]
v <- merge(v, s[, .(uf, mun, zona, secao, sid)], by = c("uf", "mun", "zona", "secao"))[, .(sid, mun, n = as.character(nr_votavel), votos)]
cs_fixo <- s[, .(uf = tolower(uf), mun = sprintf("%05d", mun), zona = sprintf("%04d", zona), secao = sprintf("%04d", secao), nsp = NA_character_)]
ciclo_em <- function(T) {
  rec <- s$dt_recebido <= T
  list(u_tot = s[, .(tpabr = "mu", cdabr = sprintf("%05d", mun[1]), ts = .N, st = sum(rec[sid]), te = sum(aptos),
                     est = sum(aptos[rec[sid]]), c = sum(comparecimento[rec[sid]])), by = mun][, mun := NULL],
       u_cand = v[rec[sid], .(vap = sum(votos)), by = .(mun, n)][, .(tpabr = "mu", cdabr = sprintf("%05d", mun), n, vap, dvt = "Válido")],
       cs = copy(cs_fixo)[, ha := fifelse(rec, "x", NA_character_)], rodada = 1L)
}
fin <- v[, .(vap = sum(votos)), by = n][, bloco := fcase(n %in% as.character(bl$PT), "PT", n %in% as.character(bl$PL), "PL", default = "OU")][
  , .(v = sum(vap)), by = bloco][, real := v / sum(v)]
real <- setNames(fin$real, fin$bloco)
dia <- if (tn == 1) "2022-10-02" else "2022-10-30"
tt <- sort(s$dt_recebido)
instantes <- unique(c(seq(as.POSIXct(paste(dia, "17:00:00"), tz = tz), tt[ceiling(0.999 * length(tt))], by = passo * 60)))
res <- rbindlist(lapply(as.list(instantes), \(T) {
  ci <- ciclo_em(T)
  rbindlist(lapply(list(completa = base, sem_7pct = base_red), \(bs) {
    m <- estado(ci, bs, bl); f <- sum(m$aptos_obs) / sum(m$aptos)
    if (f < 0.02) return(NULL)
    centro <- centro_nacional(m, tn); pj <- projetar(m, estimar(m, tn, prior, centro), centro)
    data.table(pestn = 100 * f, e_PT = 100 * (pj[["PT"]] - real[["PT"]]), e_PL = 100 * (pj[["PL"]] - real[["PL"]]))
  }), idcol = "base")
}))
res[, erro := if (tn == 1) (abs(e_PT) + abs(e_PL)) / 2 else abs(e_PT)]
res[, faixa := cut(pestn, c(2, 5, 10, 30, 50, 70, 90, 100))]
cat(sprintf("\n== %dº turno: erro absoluto médio (p.p.), base completa x sem %.0f%% das seções ==\n", tn, 100 * frac))
print(dcast(res[, .(e = round(mean(erro), 3)), by = .(faixa, base)], faixa ~ base, value.var = "e"))
print(res[, .(erro_medio = round(mean(erro), 3), erro_max = round(max(erro), 3)), by = base])
