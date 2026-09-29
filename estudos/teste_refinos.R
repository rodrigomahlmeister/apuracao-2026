# Testa dois refinamentos do modelo no replay de 2022 (ordem real dos boletins): resíduo próprio do município
# (kappa em eleitores apurados) e projeção da parte pendente seção a seção. Os dois foram descartados; o código
# deles está só no commit "estudo: refinamentos resíduo e seção" (R/modelo.R). Uso: Rscript estudos/teste_refinos.R [turno] [passo_min]
source("R/modelo.R")
a <- commandArgs(TRUE)
tn <- if (length(a) >= 1) as.integer(a[1]) else 1L
passo <- if (length(a) >= 2) as.numeric(a[2]) else 5
tz <- "America/Sao_Paulo"
rd <- function(f) as.data.table(read_parquet(file.path("dados/base", f)))

base <- carregar_base(tn, secoes = "replay2022_secao", municipios = "replay2022_municipios")
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
final <- v[, .(vap = sum(votos)), by = n][, bloco := fcase(n %in% as.character(bl$PT), "PT", n %in% as.character(bl$PL), "PL", default = "OU")][
  , .(v = sum(vap)), by = bloco][, real := v / sum(v)]
real <- setNames(final$real, final$bloco)

configs <- list(atual = list(NA, FALSE), secao = list(NA, TRUE),
                secao_res20k = list(2e4, TRUE))
dia <- if (tn == 1) "2022-10-02" else "2022-10-30"
inicio <- as.POSIXct(paste(dia, "17:00:00"), tz = tz)
tt <- sort(s$dt_recebido); fim_noite <- tt[ceiling(0.999 * length(tt))]
instantes <- unique(c(seq(inicio, fim_noite, by = passo * 60), fim_noite))
centro <- NULL
res <- rbindlist(lapply(as.list(instantes), \(T) {
  m <- estado(ciclo_em(T), base, bl)
  f <- sum(m$aptos_obs) / sum(m$aptos)
  if (f < 0.02) return(NULL)                                       # a página só mostra a partir de 2%
  if (is.null(centro)) centro <<- centro_nacional(m, tn)
  est <- estimar(m, tn, prior, centro)
  rbindlist(lapply(names(configs), \(nm) {
    PAR_MOD$residuo_kappa <<- configs[[nm]][[1]]; PAR_MOD$por_secao <<- configs[[nm]][[2]]
    pj <- projetar(m, est, centro)
    data.table(hora = T, pestn = 100 * f, config = nm, e_PT = 100 * (pj[["PT"]] - real[["PT"]]), e_PL = 100 * (pj[["PL"]] - real[["PL"]]))
  }))
}))
PAR_MOD$residuo_kappa <- NA_real_; PAR_MOD$por_secao <- FALSE
res[, erro := if (tn == 1) (abs(e_PT) + abs(e_PL)) / 2 else abs(e_PT)]
res[, faixa := cut(pestn, c(2, 5, 10, 20, 30, 50, 70, 90, 100))]
cat(sprintf("\n== %dº turno: erro absoluto médio (p.p.) por faixa de %% apurado ==\n", tn))
print(dcast(res[, .(e = round(mean(erro), 3)), by = .(faixa, config)], faixa ~ config, value.var = "e")[, c("faixa", names(configs)), with = FALSE])
cat("\n== média geral e % a partir do qual o erro fica sempre < 0,25 / < 0,1 p.p. ==\n")
marco <- function(d, lim) { d <- d[order(hora)]; r <- rev(cumprod(rev(d$erro < lim))) == 1; if (any(r)) round(d$pestn[which(r)[1]], 1) else NA_real_ }
print(res[, .(erro_medio = round(mean(erro), 3), lt_0.25 = marco(.SD, 0.25), lt_0.1 = marco(.SD, 0.1)), by = config])
fwrite(res, sprintf("docs/teste_refinos_t%d.csv", tn))
