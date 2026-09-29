# Base para o replay de 2022 no formato lido por R/modelo.R: resultado de 2018 levado às seções de 2022
# (pareamento 2022 <- 2018), por seção principal. Saídas: dados/base/replay2022_secao_t{1,2}.parquet e
# dados/base/replay2022_municipios.parquet. Rodar da raiz: Rscript preparo/04_base_replay2022.R
source("preparo/02_pareamento.R")

fontes <- list(rd("locais_2022.parquet"), rd("locais_2024.parquet"), rd("locais_2026.parquet"))
par <- list(pesos = rd("pareamento_2022_2018_pesos.parquet"), nivel = rd("pareamento_2022_2018_nivel.parquet"))
par$alvo <- prep(imputar_coord(locais_base(2022), fontes[2:3]))[, t_id := .I]
par$base <- prep(imputar_coord(locais_base(2018), fontes)[aptos > 0])[, b_id := .I]
chk <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], par$nivel[, .(t_id, u2 = uf, m2 = mun, z2 = zona, l2 = local)], by = "t_id")
stopifnot(nrow(chk) == nrow(par$nivel), chk[uf != u2 | mun != m2 | zona != z2 | local != l2, .N] == 0)

for (tn in 1:2) {
  sec <- rd("secoes_2022.parquet")[turno == tn, .(uf, mun, zona, secao_cs = secao, local, eleitores = aptos)]
  bl <- base_projetada(par, rd("hist_local_2018.parquet")[turno == tn])
  bl <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], bl, by = "t_id")[, `:=`(t_id = NULL, aptos = NULL)]
  x <- merge(sec, bl, by = c("uf", "mun", "zona", "local"), all.x = TRUE)
  cols <- c("comparecimento", "validos", "PT", "PL", "OUTROS")
  for (g in list(c("uf", "mun"), "uf")) {
    med <- x[!is.na(comparecimento), lapply(.SD, \(v) weighted.mean(v, eleitores)), by = g, .SDcols = cols]
    for (k in cols) x[med, on = g, (k) := fcoalesce(get(k), get(paste0("i.", k)))]
  }
  out <- x[, .(eleitores = sum(eleitores), b_comp = sum(eleitores * comparecimento), b_val = sum(eleitores * validos),
               b_PT = sum(eleitores * PT), b_PL = sum(eleitores * PL), b_OU = sum(eleitores * OUTROS)),
           by = .(uf, mun, zona, secao_cs)]
  write_parquet(out, sprintf("dados/base/replay2022_secao_t%d.parquet", tn), compression = "zstd")
  message(sprintf("turno %d: %d seções", tn, nrow(out)))
}
mi <- rd("municipios_ibge.parquet")[, .(uf, mun, regiao)]
elg <- merge(par$nivel[, .(uf, mun, aptos, n1 = nivel == "1")], mi, by = c("uf", "mun"), all.x = TRUE)[
  , .(elegivel = sum(aptos[n1]) / sum(aptos) >= 0.9, regiao = regiao[1]), by = .(uf, mun)]
elg[uf == "ZZ" | is.na(regiao), regiao := "ZZ"]
write_parquet(elg, "dados/base/replay2022_municipios.parquet")
