# Roda a cascata 2022 -> 2026 (produção) e 2018 -> 2022 (validação) e gera o diagnóstico da Fase 2.
# Saídas: dados/base/pareamento_<alvo>_<base>_{pesos,nivel}.parquet, docs/fase2_*.csv
source("preparo/02_pareamento.R")
PAR$w_semi <- 0.75                  # escolhido na grade de semi-novos (2º turno)
t0 <- Sys.time()
set.seed(2026)

grandes <- function() {           # capitais + municípios com > 200 mil eleitores em 2026
  m <- rd("municipios_ibge.parquet")
  e <- rd("locais_2026.parquet")[, .(el = sum(eleitores)), by = .(uf, mun)]
  merge(e, m[, .(uf, mun, nm_mun, capital)], by = c("uf", "mun"))[capital | el > 2e5][order(-el)]
}
arq <- function(nome, tipo) sprintf("dados/base/pareamento_%s_%s.parquet", nome, tipo)
anterior <- function(nome) if (file.exists(arq(nome, "nivel"))) as.data.table(read_parquet(arq(nome, "nivel")))
salvar <- function(par, nome) {
  write_parquet(par$pesos, arq(nome, "pesos"), compression = "zstd")
  write_parquet(par$nivel, arq(nome, "nivel"), compression = "zstd")
}
resumo_niveis <- function(niv) {
  niv[, .(locais = .N, eleitorado = sum(aptos)), by = nivel][
    , pct_eleitorado := round(100 * eleitorado / sum(eleitorado), 2)][order(nivel)]
}
mudancas_n2 <- function(ant, novo) {
  if (is.null(ant)) return(invisible())
  x <- merge(ant[, .(uf, mun, zona, local, antes = nivel)], novo[, .(uf, mun, zona, local, depois = nivel, aptos)],
             by = c("uf", "mun", "zona", "local"))
  cat("saíram do nível 2:", x[antes == "2" & depois != "2", .N], "locais; destino:\n")
  print(x[antes == "2" & depois != "2", .N, by = depois])
  cat("entraram no nível 2:", x[antes != "2" & depois == "2", .N], "\n")
}

# ---- 2022 -> 2026 ------------------------------------------------------------------
message("Pareamento 2026 <- 2022")
ant26 <- anterior("2026_2022")
fontes_coord <- list(rd("locais_2024.parquet"), rd("locais_2026.parquet"))
base22 <- imputar_coord(locais_base(2022), fontes_coord)
cat("base 2022: coordenadas imputadas em", base22[coord_imputada == TRUE, .N], "locais; cobertura (% eleitorado)",
    round(100 * base22[, sum(aptos[!is.na(lat)]) / sum(aptos)], 1), "
")
p26 <- parear(locais_alvo_2026(), base22)
salvar(p26, "2026_2022")
n26 <- p26$nivel
cat("\n== 2026 <- 2022: % do eleitorado 2026 por nível (total) ==\n"); print(resumo_niveis(n26))
mudancas_n2(ant26, n26)
cat("semi-novos:", n26[, sum(semi)], "locais,", round(100 * n26[semi == TRUE, sum(aptos)] / sum(n26$aptos), 2),
    "% do eleitorado | nível 1 sem verificação:", n26[verif == FALSE, .N], "\n")

g <- grandes()
ng <- merge(n26, g[, .(uf, mun, nm_mun)], by = c("uf", "mun"))
cat("\n== cidades grandes (", nrow(g), ") ==\n"); print(resumo_niveis(ng))
tab_g <- dcast(ng[, .(el = sum(aptos)), by = .(uf, nm_mun, nivel)], uf + nm_mun ~ nivel, value.var = "el", fill = 0)
cols <- setdiff(names(tab_g), c("uf", "nm_mun"))
tab_g[, total := rowSums(.SD), .SDcols = cols]
tab_g[, (cols) := lapply(.SD, \(x) round(100 * x / total, 1)), .SDcols = cols]
fwrite(tab_g[order(-total)], "docs/fase2_niveis_cidades_grandes.csv")

m45 <- n26[, .(el = sum(aptos), el45 = sum(aptos[nivel %in% c("4", "5", "5n", "sem_base")])), by = .(uf, mun)][
  , pct45 := round(100 * el45 / el, 1)][pct45 > 10]
m45 <- merge(m45, rd("municipios_ibge.parquet")[, .(uf, mun, nm_mun)], by = c("uf", "mun"))[order(-el45)]
cat("\nmunicípios com > 10% do eleitorado nos níveis 4-5:", nrow(m45), "|", sum(m45$el), "eleitores\n")
fwrite(m45, "docs/fase2_municipios_nivel45.csv")

# ---- 2018 -> 2022 (validação) ---------------------------------------------------------
message("Pareamento 2022 <- 2018")
ant22 <- anterior("2022_2018")
base18 <- imputar_coord(locais_base(2018), list(rd("locais_2022.parquet"), rd("locais_2024.parquet"), rd("locais_2026.parquet")))
cat("base 2018: coordenadas imputadas em", base18[coord_imputada == TRUE, .N], "locais; cobertura (% eleitorado)",
    round(100 * base18[, sum(aptos[!is.na(lat)]) / sum(aptos)], 1), "
")
p22 <- parear(imputar_coord(locais_base(2022), list(rd("locais_2024.parquet"), rd("locais_2026.parquet"))), base18)
salvar(p22, "2022_2018")
cat("\n== 2022 <- 2018: % do eleitorado 2022 por nível ==\n"); print(resumo_niveis(p22$nivel))
mudancas_n2(ant22, p22$nivel)

h18 <- rd("hist_local_2018.parquet"); h22 <- rd("hist_local_2022.parquet")
m18 <- rd("hist_mun_2018.parquet");   m22 <- rd("hist_mun_2022.parquet")

# tabela local-a-local de um turno: base projetada (por apto), resultado real, swing do município
montar <- function(tn, w_semi = PAR$w_semi) {
  bp  <- base_projetada(p22, h18[turno == tn], w_semi = w_semi)
  act <- merge(p22$alvo[, .(t_id, uf, mun, zona, local)], h22[turno == tn], by = c("uf", "mun", "zona", "local"))
  x <- merge(act[, .(t_id, uf, mun, PT, PL, OUTROS, validos, aptos)],
             bp[, .(t_id, bPT = PT, bPL = PL, bOU = OUTROS)], by = "t_id")
  sw <- merge(m18[turno == tn, .(uf, mun, PTb = PT, PLb = PL, OUb = OUTROS)],
              m22[turno == tn, .(uf, mun, PTa = PT, PLa = PL, OUa = OUTROS, Va = validos)], by = c("uf", "mun"))
  x <- merge(merge(x, sw, by = c("uf", "mun")), p22$nivel[, .(t_id, nivel, semi)], by = "t_id")[validos > 0]
  x[, turno := tn]
}

prever <- function(x, variante = "lr") {       # swing do município inteiro (resultado real de 2022)
  tn <- x$turno[1]
  s <- if (tn == 2) projetar_shares(x$bPT, x$bPL, x$bOU, x$aptos, lr(x$PTa, x$PLa) - lr(x$PTb, x$PLb), turno = 2)
       else if (variante == "lr") projetar_shares(x$bPT, x$bPL, x$bOU, x$aptos,
                                                  lr(x$PTa, x$OUa) - lr(x$PTb, x$OUb), lr(x$PLa, x$OUa) - lr(x$PLb, x$OUb))
       else projetar_shares_2b(x$bPT, x$bPL, x$bOU, x$aptos, lr(x$PTa, x$PLa) - lr(x$PTb, x$PLb),
                               lr(x$OUa, x$PTa + x$PLa) - lr(x$OUb, x$PTb + x$PLb))
  x[, `:=`(pPT = s$PT, pPL = s$PL, aPT = PT / validos, aPL = PL / validos, mPT = PTa / Va)]
  x[, `:=`(e_PT = 100 * (pPT - aPT), e_PL = 100 * (pPL - aPL), r_PT = 100 * (mPT - aPT))]
}

tab_erro <- function(d) {
  d[, .(locais = .N, pct_eleit = as.numeric(sum(aptos)),
        mae_PT = metricas(e_PT, validos)$mae, mae_PT_aptos = metricas(e_PT, aptos)$mae,
        rmse_PT = metricas(e_PT, validos)$rmse, mae_PL = metricas(e_PL, validos)$mae,
        ref_mae = metricas(r_PT, validos)$mae, ref_rmse = metricas(r_PT, validos)$rmse),
    by = .(turno, nivel)][, pct_eleit := 100 * pct_eleit / sum(pct_eleit), by = turno][order(turno, nivel)][
    , lapply(.SD, \(v) if (is.numeric(v) && !is.integer(v)) round(v, 2) else v)]
}

ev <- rbind(prever(montar(1)), prever(montar(2)))
excl <- c("MG", "ES", "BA", "SE")
cat("\n== (1) erro por local, 2018 -> 2022, p.p. da % de válidos ==\n")
cat("mae = erro abs. médio ponderado por válidos; mae_PT_aptos = ponderado por eleitorado; ref = só o município\n")
cat("-- todas as UFs --\n");       e_all <- tab_erro(ev);                 print(e_all)
cat("-- sem MG, ES, BA, SE --\n"); e_sem <- tab_erro(ev[!uf %in% excl]); print(e_sem)
ev2b <- prever(montar(1), "2b")
cat("-- 1T, variante logit PT x PL + OUTROS à parte --\n"); print(tab_erro(ev2b)[, .(nivel, mae_PT, rmse_PT, ref_mae)])
fwrite(rbind(e_all[, amostra := "todas"], e_sem[, amostra := "sem_MG_ES_BA_SE"]), "docs/fase2_erro_por_nivel.csv")
write_parquet(ev, "dados/base/validacao_pareamento_2022_2018.parquet", compression = "zstd")

cat("\n== (2) semi-novos: peso w no próprio histórico ==\n")
grade <- rbindlist(lapply(c(0, 0.25, 0.5, 0.75, 1), \(w) rbindlist(lapply(1:2, \(tn) {
  x <- prever(montar(tn, w_semi = w))[semi == TRUE]
  data.table(turno = tn, w = w, locais = nrow(x), mae_PT = metricas(x$e_PT, x$validos)$mae,
             rmse_PT = metricas(x$e_PT, x$validos)$rmse)
}))))
print(dcast(grade, w ~ turno, value.var = c("mae_PT", "rmse_PT")))
fwrite(grade, "docs/fase2_semi_novos_peso.csv")

# (3) erro agregado na cidade, simulando apuração parcial em ordem ALEATÓRIA de locais:
#     uma fração f dos locais é "apurada"; swing da cidade = swing observado nesses locais;
#     o resto é projetado (base + swing) e somado. Ref = % bruta dos locais apurados.
#     Válidos reais dos locais restantes são usados como peso (isola o erro de proporção).
cat("\n== (3) erro agregado por cidade (municípios com >= 10 locais), apuração aleatória ==\n")
agregado <- function(x, f, reps = 5) {
  x <- x[, if (.N >= 10) .SD, by = .(uf, mun)]
  rbindlist(lapply(seq_len(reps), \(r) {
    x[, obs := seq_len(.N) %in% sample(.N, ceiling(f * .N)), by = .(uf, mun)]
    tn <- x$turno[1]
    x[, {
      O <- .SD[obs == TRUE]; R <- .SD[obs == FALSE]
      bO <- lapply(list(PT = O$bPT, PL = O$bPL, OU = O$bOU), \(b) sum(b * O$aptos))
      if (tn == 1) {
        s <- projetar_shares(R$bPT, R$bPL, R$bOU, R$aptos,
                             lr(sum(O$PT), sum(O$OUTROS)) - lr(bO$PT, bO$OU), lr(sum(O$PL), sum(O$OUTROS)) - lr(bO$PL, bO$OU))
      } else {
        s <- projetar_shares(R$bPT, R$bPL, R$bOU, R$aptos, lr(sum(O$PT), sum(O$PL)) - lr(bO$PT, bO$PL), turno = 2)
      }
      real <- sum(PT) / sum(validos)
      .(validos = sum(validos),
        e_casc = 100 * ((sum(O$PT) + sum(s$PT * R$validos)) / sum(validos) - real),
        e_bruto = 100 * (sum(O$PT) / sum(O$validos) - real))
    }, by = .(uf, mun)][, `:=`(rep = r, f = f, turno = tn)]
  }))
}
ag <- rbindlist(lapply(1:2, \(tn) rbindlist(lapply(c(0.25, 0.5), \(f) agregado(ev[turno == tn], f)))))
ag <- merge(ag, g[, .(uf, mun, grande = TRUE)], by = c("uf", "mun"), all.x = TRUE)[is.na(grande), grande := FALSE]
res_ag <- ag[, .(cidades = uniqueN(paste(uf, mun)),
                 mae_casc = metricas(e_casc, validos)$mae, rmse_casc = metricas(e_casc, validos)$rmse,
                 mae_bruto = metricas(e_bruto, validos)$mae, rmse_bruto = metricas(e_bruto, validos)$rmse),
             by = .(turno, f, grande)][order(turno, f, -grande)]
print(res_ag[, lapply(.SD, \(v) if (is.double(v)) round(v, 2) else v)])
fwrite(res_ag, "docs/fase2_erro_agregado_cidade.csv")
message(sprintf("tempo total: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
