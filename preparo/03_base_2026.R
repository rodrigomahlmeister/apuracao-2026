# Base por seção de 2026 para a apuração: resultado de 2022 (1º e 2º turnos) levado aos locais de 2026 pelo
# pareamento (preparo/02_rodar_pareamento.R) e somado por seção principal (a unidade do arquivo de seções do TSE).
# Saídas: dados/base/base_2026_secao_t{1,2}.parquet e dados/base/municipios_2026_modelo.parquet
# Rodar da raiz do projeto: Rscript preparo/03_base_2026.R
source("preparo/02_pareamento.R")

par <- list(pesos = rd("pareamento_2026_2022_pesos.parquet"), nivel = rd("pareamento_2026_2022_nivel.parquet"))
par$alvo <- prep(locais_alvo_2026())[, t_id := .I]
par$base <- prep(imputar_coord(locais_base(2022), list(rd("locais_2024.parquet"), rd("locais_2026.parquet")))[aptos > 0])[, b_id := .I]
chk <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], par$nivel[, .(t_id, u2 = uf, m2 = mun, z2 = zona, l2 = local)], by = "t_id")
stopifnot(nrow(chk) == nrow(par$nivel), chk[uf != u2 | mun != m2 | zona != z2 | local != l2, .N] == 0)

sec <- rd("eleitorado_secao_2026.parquet")[, .(uf, mun, zona, secao, local, eleitores, secao_principal, tipo_agregada)]
# seção principal de cada seção agregada (tipo 2). A base sai seção a seção, com as duas chaves: se o cs da noite
# listar a agregada como seção própria, ela usa a própria base; se não, soma na principal (R/modelo.R, estado()).
sec[, secao_cs := fifelse(!is.na(secao_principal) & secao_principal > 0 & tipo_agregada != 1, secao_principal, secao)]

for (tn in 1:2) {
  bl <- base_projetada(par, rd("hist_local_2022.parquet")[turno == tn])        # por 1 apto
  bl <- merge(par$alvo[, .(t_id, uf, mun, zona, local)], bl, by = "t_id")[, `:=`(t_id = NULL, aptos = NULL)]
  x <- merge(sec, bl, by = c("uf", "mun", "zona", "local"), all.x = TRUE)
  # local sem base: média do município por eleitor, depois da UF
  cols <- c("comparecimento", "validos", "PT", "PL", "OUTROS")
  for (g in list(c("uf", "mun"), "uf")) {
    med <- x[!is.na(comparecimento), lapply(.SD, \(v) weighted.mean(v, eleitores)), by = g, .SDcols = cols]
    for (k in cols) x[med, on = g, (k) := fcoalesce(get(k), get(paste0("i.", k)))]
  }
  out <- x[, .(eleitores = sum(eleitores), b_comp = sum(eleitores * comparecimento), b_val = sum(eleitores * validos),
               b_PT = sum(eleitores * PT), b_PL = sum(eleitores * PL), b_OU = sum(eleitores * OUTROS)),
           by = .(uf, mun, zona, secao, secao_cs)]
  write_parquet(out, sprintf("dados/base/base_2026_secao_t%d.parquet", tn), compression = "zstd")
  message(sprintf("turno %d: %d seções, %d eleitores, base PT %.1f%% / PL %.1f%% dos válidos", tn, nrow(out),
                  sum(out$eleitores), 100 * sum(out$b_PT) / sum(out$b_val), 100 * sum(out$b_PL) / sum(out$b_val)))
}

# municípios: região e elegibilidade para estimar o swing (>= 90% do eleitorado com base de nível 1)
mi <- rd("municipios_ibge.parquet")[, .(uf, mun, regiao)]
elg <- merge(par$nivel[, .(uf, mun, aptos, n1 = nivel == "1")], mi, by = c("uf", "mun"), all.x = TRUE)[
  , .(elegivel = sum(aptos[n1]) / sum(aptos) >= 0.9, regiao = regiao[1]), by = .(uf, mun)]
elg[uf == "ZZ" | is.na(regiao), regiao := "ZZ"]
write_parquet(elg, "dados/base/municipios_2026_modelo.parquet")
message(sprintf("municípios: %d (elegíveis para estimar o swing: %d)", nrow(elg), sum(elg$elegivel)))
