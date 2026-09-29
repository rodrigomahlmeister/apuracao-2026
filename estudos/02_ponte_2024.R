# Ponte 2026 -> 2024 -> 2022 para municípios com > 10% do eleitorado nos níveis 4-5 no pareamento direto.
# Substitui o pareamento direto de um local quando o caminho pela ponte tem nível melhor.
# Saída: dados/base/pareamento_2026_2022_final_{pesos,nivel}.parquet (pesos já achatados, sem componente semi)
source("preparo/02_pareamento.R")
PAR$w_semi <- 0.75

alvo_mun <- fread("docs/fase2_municipios_nivel45.csv")[uf != "ZZ", .(uf, mun)]
cat("municípios na ponte:", nrow(alvo_mun), "\n")
sel <- function(d) d[alvo_mun, on = .(uf, mun), nomatch = 0]

alvo26 <- locais_alvo_2026()
meio24 <- rd("locais_2024.parquet")[, .(uf, mun, zona, local, cep, lat, lon, aptos = eleitores)]
base22 <- locais_base(2022)
cat("coordenadas 2024 (% eleitorado, municípios da ponte):",
    round(100 * sel(meio24)[, sum(aptos[!is.na(lat)]) / sum(aptos)], 1), "\n")

# pareamento direto (já salvo) e ponte, só nesses municípios
dir_par <- list(pesos = rd("pareamento_2026_2022_pesos.parquet"), nivel = rd("pareamento_2026_2022_nivel.parquet"))
pt <- ponte(sel(alvo26), sel(meio24), base22[unique(alvo_mun$uf), on = "uf"])   # base: UF inteira (município novo)

# casa os locais-alvo da ponte com os do pareamento direto (t_id diferentes: casa pela chave)
k <- c("uf", "mun", "zona", "local")
cmp <- merge(dir_par$nivel[, c("t_id", k, "nivel"), with = FALSE], pt$nivel[, c("t_id", k, "nivel", "n1", "n2"), with = FALSE],
             by = k, suffixes = c("_dir", "_ponte"))
cmp[, melhora := rank_nivel(nivel_ponte) < rank_nivel(nivel_dir)]
el <- merge(cmp, alvo26[, c(k, "aptos"), with = FALSE], by = k)
cat("\n== municípios da ponte: % do eleitorado por nível, direto x ponte x final ==\n")
tab <- rbind(el[, .(tipo = "direto", el = sum(aptos)), by = .(nivel = nivel_dir)],
             el[, .(tipo = "ponte", el = sum(aptos)), by = .(nivel = nivel_ponte)],
             el[, .(tipo = "final", el = sum(aptos)), by = .(nivel = fifelse(melhora, nivel_ponte, nivel_dir))])
tab[, pct := round(100 * el / sum(el), 1), by = tipo]
print(dcast(tab, nivel ~ tipo, value.var = "pct", fill = 0))
cat("locais que melhoram pela ponte:", el[melhora == TRUE, .N], "| eleitorado:", el[melhora == TRUE, sum(aptos)], "\n")
cat("salto 2026->2024 dos que melhoram:\n"); print(el[melhora == TRUE, .N, by = n1])

# pareamento final: direto (achatado) + substituições da ponte. b_id da ponte -> b_id do direto (pela chave).
base_dir <- prep(locais_base(2022)[aptos > 0])[, b_id := .I]                     # mesma numeração do pareamento direto
idmap <- merge(pt$base[, c("b_id", k), with = FALSE], base_dir[, c("b_id", k), with = FALSE],
               by = k, suffixes = c("_p", ""))[, .(b_id_p, b_id)]
troca <- cmp[melhora == TRUE, .(t_id = t_id_dir, t_id_p = t_id_ponte)]
novos <- merge(merge(pt$pesos[, .(t_id_p = t_id, b_id_p = b_id, v)], troca, by = "t_id_p"), idmap, by = "b_id_p")
final_pesos <- rbind(achatar_pesos(dir_par, PAR$w_semi)[!t_id %in% troca$t_id], novos[, .(t_id, b_id, v)])
final_nivel <- copy(dir_par$nivel)[, ponte := FALSE]
final_nivel[troca, on = "t_id", `:=`(nivel = cmp[melhora == TRUE][match(troca$t_id, t_id_dir), nivel_ponte], ponte = TRUE)]
chk <- merge(final_pesos, base_dir[, .(b_id, aptos)], by = "b_id")[, .(s = sum(v * aptos)), by = t_id]
ruim <- chk[abs(s - 1) > 1e-6]; if (nrow(ruim)) { print(merge(ruim, final_nivel, by = "t_id")[, .N, by = .(ponte, nivel, s_r = round(s, 3))]); stop("pesos inconsistentes") }
stopifnot(nrow(chk) == final_nivel[nivel != "sem_base", .N])
write_parquet(final_pesos, "dados/base/pareamento_2026_2022_final_pesos.parquet", compression = "zstd")
write_parquet(final_nivel, "dados/base/pareamento_2026_2022_final_nivel.parquet", compression = "zstd")
cat("\n== 2026 <- 2022 final (com ponte): % do eleitorado por nível ==\n")
print(final_nivel[, .(locais = .N, pct = round(100 * sum(aptos) / sum(final_nivel$aptos), 2)), by = nivel][order(nivel)])
m45 <- final_nivel[, .(el = sum(aptos), el45 = sum(aptos[nivel %in% c("4", "5", "5n", "sem_base")])), by = .(uf, mun)][
  , pct45 := 100 * el45 / el][pct45 > 10]
cat("municípios com > 10% nos níveis 4-5 depois da ponte:", nrow(m45), "(", sum(m45$el), "eleitores )\n")
