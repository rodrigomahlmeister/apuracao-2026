# Fase 2: pareamento de locais de votação entre eleições (alvo <- base), sem usar nome do local.
#
# Cascata (por local-alvo t, sempre dentro do mesmo município):
#   1  chave exata (zona + local) com checagem: CEP igual ou distância < 200 m
#   2  mesmo prédio sob outra chave:
#        com coordenadas nos dois anos -> distância < 100 m (CEP igual não basta)
#        sem coordenadas em algum dos anos -> CEP igual, único àquele local nos dois anos e não terminado em 000
#      (empate: menor distância, depois mesma zona)
#   3  vizinhos: k = 5 locais mais próximos num raio de 3 km, peso = eleitorado / distância
#   4  zona da base (mesmo número de zona)        <- local sem coordenadas desce direto para cá
#   5  município da base
#   5n município novo (sem base): vizinhos na UF num raio de 30 km
# Semi-novo: nível 1/2 com |eleitorado_alvo / eleitorado_base - 1| > 0,5 -> w x próprio + (1 - w) x vizinhos.
#
# Saída de parear(): pesos (t_id, b_id, v, comp). A "base" de t = soma de v_b x contagens_b, com
# sum(v_b x aptos_b) = 1 em cada componente. comp = "p" (principal) ou "s" (vizinhos de um semi-novo);
# o peso w do semi-novo é aplicado em base_projetada(), para poder ser calibrado sem refazer o pareamento.

suppressPackageStartupMessages({ library(arrow); library(data.table); library(yaml) })

PAR <- list(d_chave = 200, d_predio = 100, k = 5, raio_viz = 3000, d_min = 50, semi = 0.5, raio_novo = 30000,
            w_semi = 0.75, n2_semi = TRUE, w_n2 = 0.5)
# w_semi: peso do próprio histórico no semi-novo por tamanho (grade 2018 -> 2022, critério 2º turno)
# n2_semi/w_n2: todo nível 2 vira semi-novo com peso 0,5 (1T deixa de perder para o município; 2T 3,73 -> 3,46)
rd <- function(f) as.data.table(read_parquet(file.path("dados/base", f)))

dist_m <- function(lat1, lon1, lat2, lon2) {          # haversine, metros; matriz length(lat1) x length(lat2)
  r <- pi / 180
  a <- outer(lat1 * r, lat2 * r, \(x, y) sin((y - x) / 2)^2) +
       outer(cos(lat1 * r), cos(lat2 * r)) * outer(lon1 * r, lon2 * r, \(x, y) sin((y - x) / 2)^2)
  matrix(2 * 6371000 * asin(pmin(1, sqrt(a))), nrow = length(lat1))
}

moda <- function(x) { x <- x[!is.na(x)]; if (!length(x)) x[NA_integer_] else as.integer(names(which.max(table(x)))) }

# Locais de um ano-base: chave do dia da eleição (BU/detalhe), atributos do eleitorado via seção
# (preferindo linhas cujo local original = local do BU), aptos do 1º turno.
locais_base <- function(ano) {
  sec <- rd(sprintf("secoes_%d.parquet", ano))[turno == 1, .(uf, mun, zona, secao, local, aptos)]
  el  <- rd(sprintf("eleitorado_secao_%d.parquet", ano))[, .(uf, mun, zona, secao, lo = local_original, cep, lat, lon)]
  x <- merge(sec, el, by = c("uf", "mun", "zona", "secao"), all.x = TRUE)
  x[, pref := !is.na(lo) & lo == local]
  x[, usar := if (any(pref)) pref else rep(TRUE, .N), by = .(uf, mun, zona, local)]
  x[usar == TRUE, .(cep = moda(cep), lat = median(lat, na.rm = TRUE), lon = median(lon, na.rm = TRUE),
                    aptos = sum(aptos)), by = .(uf, mun, zona, local)][
    , `:=`(lat = fifelse(is.na(lon), NA_real_, lat), lon = fifelse(is.na(lat), NA_real_, lon))][]
}

# Locais do alvo 2026: chave atual do eleitorado 2026
locais_alvo_2026 <- function() {
  rd("locais_2026.parquet")[, .(uf, mun, zona, local, cep, lat, lon, aptos = eleitores)]
}

# CEP de 8 dígitos; "único" = não termina em 000 e só esse local do município o usa (naquele ano)
prep <- function(d) {
  d <- copy(d)
  d[, cep8 := fifelse(is.na(cep) | cep <= 0, NA_character_, sprintf("%08d", cep))]
  d[, n_cep := .N, by = .(uf, mun, cep8)]
  d[, cep_unico := !is.na(cep8) & !grepl("000$", cep8) & n_cep == 1]
  d
}

# normaliza pesos de um componente: sum(v * aptos) = 1
norm_v <- function(w, aptos) w / sum(w * aptos)

vizinhos <- function(B, D_row, excluir = integer(), raio = PAR$raio_viz) {
  ok <- which(!is.na(D_row) & D_row <= raio)
  ok <- setdiff(ok, excluir)
  if (!length(ok)) return(NULL)
  ok <- ok[order(D_row[ok])][seq_len(min(PAR$k, length(ok)))]
  data.table(b = ok, v = norm_v(1 / pmax(D_row[ok], PAR$d_min), B$aptos[ok]))
}

parear_mun <- function(T, B, B_uf = NULL) {
  nt <- nrow(T)
  res_v <- vector("list", nt)
  res_n <- data.table(i = seq_len(nt), nivel = NA_character_, semi = FALSE, verif = TRUE)
  if (!nrow(B)) {                                            # município novo: vizinhos na UF
    Bu <- B_uf[!is.na(lat)]
    D <- if (nrow(Bu) && any(!is.na(T$lat))) dist_m(T$lat, T$lon, Bu$lat, Bu$lon) else NULL
    for (i in seq_len(nt)) {
      vz <- if (!is.null(D)) vizinhos(Bu, D[i, ], raio = PAR$raio_novo)
      if (!is.null(vz)) { res_v[[i]] <- vz[, .(b_id = Bu$b_id[b], v, comp = "p")]; res_n[i, nivel := "5n"] }
      else res_n[i, nivel := "sem_base"]
    }
    return(list(v = rbindlist(res_v, idcol = "i", use.names = TRUE), n = res_n))
  }
  D <- dist_m(T$lat, T$lon, B$lat, B$lon)                   # NA quando falta coordenada
  m1 <- match(paste(T$zona, T$local), paste(B$zona, B$local))
  B_coord <- !is.na(B$lat)
  for (i in seq_len(nt)) {
    b <- NA_integer_; niv <- NA_character_; e_verif <- TRUE
    # 1: chave exata com checagem
    if (!is.na(m1[i])) {
      j <- m1[i]; cep_ok <- !is.na(T$cep8[i]) && identical(T$cep8[i], B$cep8[j]); d <- D[i, j]
      if (cep_ok || (!is.na(d) && d < PAR$d_chave)) { b <- j; niv <- "1" }
      else if (is.na(d) && (is.na(T$cep8[i]) || is.na(B$cep8[j]))) { b <- j; niv <- "1"; e_verif <- FALSE }
    }
    # 2: mesmo prédio sob outra chave
    if (is.na(b)) {
      t_coord <- !is.na(T$lat[i])
      por_dist <- t_coord & B_coord & !is.na(D[i, ]) & D[i, ] < PAR$d_predio
      por_cep  <- !(t_coord & B_coord) & T$cep_unico[i] & B$cep_unico & B$cep8 %in% T$cep8[i]
      cand <- which(por_dist | por_cep)
      if (length(cand)) {
        dd <- D[i, cand]; dd[is.na(dd)] <- Inf
        b <- cand[order(dd, B$zona[cand] != T$zona[i])][1]; niv <- "2"
      }
    }
    if (!is.na(b)) {
      e_semi <- isTRUE(abs(T$aptos[i] / B$aptos[b] - 1) > PAR$semi) || (PAR$n2_semi && niv == "2")
      own <- data.table(b = b, v = 1 / B$aptos[b], comp = "p")
      if (e_semi) {
        vz <- vizinhos(B, D[i, ], excluir = b)
        if (!is.null(vz)) own <- rbind(own, vz[, comp := "s"]) else e_semi <- FALSE
      }
      res_v[[i]] <- own; res_n[i, `:=`(nivel = niv, semi = e_semi, verif = e_verif)]
      next
    }
    # 3: vizinhos (só com coordenadas)
    vz <- vizinhos(B, D[i, ])
    if (!is.null(vz)) { res_v[[i]] <- vz[, comp := "p"]; res_n[i, nivel := "3"]; next }
    # 4: zona; 5: município
    cand <- which(B$zona == T$zona[i])
    if (length(cand)) {
      res_v[[i]] <- data.table(b = cand, v = norm_v(rep(1, length(cand)), B$aptos[cand]), comp = "p")
      res_n[i, nivel := "4"]; next
    }
    res_v[[i]] <- data.table(b = seq_len(nrow(B)), v = norm_v(rep(1, nrow(B)), B$aptos), comp = "p")
    res_n[i, nivel := "5"]
  }
  v <- rbindlist(res_v, idcol = "i", use.names = TRUE)
  if (nrow(v)) v[, b_id := B$b_id[b]][, b := NULL]
  list(v = v, n = res_n)
}

parear <- function(alvo, base) {
  alvo <- prep(alvo)[, t_id := .I]; base <- prep(base[aptos > 0])[, b_id := .I]   # local sem eleitor não serve de base
  muns <- unique(alvo[, .(uf, mun)])
  out <- lapply(seq_len(nrow(muns)), \(k) {
    T <- alvo[uf == muns$uf[k] & mun == muns$mun[k]]
    B <- base[uf == muns$uf[k] & mun == muns$mun[k]]
    r <- parear_mun(T, B, if (!nrow(B)) base[uf == muns$uf[k]])
    list(v = if (nrow(r$v)) r$v[, t_id := T$t_id[i]][, i := NULL], n = r$n[, t_id := T$t_id[i]][, i := NULL])
  })
  niv <- merge(alvo[, .(t_id, uf, mun, zona, local, aptos)], rbindlist(lapply(out, `[[`, "n")), by = "t_id")
  pesos <- rbindlist(lapply(out, `[[`, "v"), use.names = TRUE)
  setcolorder(pesos, c("t_id", "b_id", "v", "comp"))
  list(pesos = pesos, nivel = niv, alvo = alvo, base = base)
}

# fator de cada componente: semi-novo por tamanho usa w_semi; nível 2 usa w_n2 (vale o de nível 2 se ambos)
fator_semi <- function(semi, nivel, comp, w_semi = PAR$w_semi, w_n2 = PAR$w_n2) {
  w <- fifelse(nivel == "2", w_n2, w_semi)
  fifelse(!semi, 1, fifelse(comp == "p", w, 1 - w))
}

# pseudo-contagens da base (por 1 apto) para cada local-alvo
base_projetada <- function(par, hist_base, vars = c("aptos", "comparecimento", "validos", "PT", "PL", "OUTROS"),
                           w_semi = PAR$w_semi, w_n2 = PAR$w_n2) {
  hb <- merge(par$base[, .(b_id, uf, mun, zona, local)], hist_base, by = c("uf", "mun", "zona", "local"))
  x <- merge(par$pesos, hb[, c("b_id", vars), with = FALSE], by = "b_id")
  x <- merge(x, par$nivel[, .(t_id, semi, nivel)], by = "t_id")
  x[, f := fator_semi(semi, nivel, comp, w_semi, w_n2)]
  x[, lapply(.SD, \(c) sum(f * v * c)), by = t_id, .SDcols = vars]
}

# ---- projeção de um local (base + swing) e métricas ---------------------------------------
sm <- function(x) x + 0.5                                   # suavização em CONTAGENS (não em taxas)

# Proporções projetadas nos válidos. b* = pseudo-contagens por 1 apto; aptos = eleitorado do alvo
# (reescala antes de suavizar); sw1/sw2 = swing em razão log (1T: PT/OUTROS e PL/OUTROS; 2T: sw1 = PT/PL).
projetar_shares <- function(bPT, bPL, bOU, aptos, sw1, sw2 = NULL, turno = 1) {
  bPT <- bPT * aptos; bPL <- bPL * aptos; bOU <- bOU * aptos
  if (turno == 1) {
    e1 <- log(sm(bPT) / sm(bOU)) + sw1; e2 <- log(sm(bPL) / sm(bOU)) + sw2
    den <- 1 + exp(e1) + exp(e2)
    list(PT = exp(e1) / den, PL = exp(e2) / den)
  } else {
    e <- log(sm(bPT) / sm(bPL)) + sw1
    list(PT = plogis(e), PL = 1 - plogis(e))
  }
}

# variante 1T: logit PT x PL + logit OUTROS x (PT+PL), swings separados
projetar_shares_2b <- function(bPT, bPL, bOU, aptos, sw_tp, sw_ou) {
  bPT <- bPT * aptos; bPL <- bPL * aptos; bOU <- bOU * aptos
  tp <- plogis(log(sm(bPT) / sm(bPL)) + sw_tp)
  ou <- plogis(log(sm(bOU) / sm(bPT + bPL)) + sw_ou)
  list(PT = (1 - ou) * tp, PL = (1 - ou) * (1 - tp))
}

lr <- function(a, b) log(sm(a) / sm(b))

metricas <- function(erro, w) {       # erro em p.p.
  list(mae = weighted.mean(abs(erro), w), rmse = sqrt(weighted.mean(erro^2, w)))
}

# ---- ponte por um ano intermediário (2026 -> 2024 -> 2022) ------------------------------------
ORDEM_NIVEL <- c("1", "2", "3", "4", "5", "5n", "sem_base")
rank_nivel <- function(n) match(n, ORDEM_NIVEL)

# achata semi-novos (aplica w_semi) para compor caminhos
achatar_pesos <- function(par, w_semi = PAR$w_semi, w_n2 = PAR$w_n2) {
  x <- merge(par$pesos, par$nivel[, .(t_id, semi, nivel)], by = "t_id")
  x[, .(v = sum(v * fator_semi(semi, nivel, comp, w_semi, w_n2))), by = .(t_id, b_id)]
}

# Pareia alvo -> meio -> base. Devolve pesos compostos (t_id do alvo, b_id da base), nível do caminho
# (pior dos dois saltos) e o próprio pareamento alvo -> meio, para diagnóstico.
ponte <- function(alvo, meio, base, w_semi = PAR$w_semi) {
  meio <- meio[aptos > 0]                      # mesma numeração de k nos dois saltos
  p1 <- parear(alvo, meio)                     # t -> k (b_id de p1 = linha de `meio`)
  p2 <- parear(meio, base)                     # k -> b (t_id de p2 = linha de `meio`)
  a1 <- achatar_pesos(p1, w_semi); a2 <- achatar_pesos(p2, w_semi)
  apt_k <- p2$alvo[, .(k = t_id, aptos_k = aptos)]
  comp <- merge(a1[, .(t_id, k = b_id, v1 = v)], apt_k, by = "k")
  comp <- merge(comp, a2[, .(k = t_id, b_id, v2 = v)], by = "k", allow.cartesian = TRUE)
  pesos <- comp[, .(v = sum(v1 * aptos_k * v2)), by = .(t_id, b_id)]
  nk <- merge(a1[, .(t_id, k = b_id)], p2$nivel[, .(k = t_id, n2 = rank_nivel(nivel))], by = "k")[
    , .(n2 = max(n2)), by = t_id]
  niv <- merge(p1$nivel[, .(t_id, uf, mun, zona, local, aptos, n1 = rank_nivel(nivel))], nk, by = "t_id", all.x = TRUE)
  niv[, nivel := ORDEM_NIVEL[pmax(n1, n2, na.rm = TRUE)]]
  list(pesos = pesos, nivel = niv[, .(t_id, uf, mun, zona, local, aptos, nivel, n1 = ORDEM_NIVEL[n1], n2 = ORDEM_NIVEL[n2])],
       base = p2$base)
}

# ---- imputação de coordenadas da base -----------------------------------------------------------
# Local-base sem coordenadas herda lat/lon do mesmo local (mesma chave e mesmo CEP) em outro arquivo de
# eleitorado (coordenada é atributo do prédio; não usa voto). `fontes` em ordem de preferência.
imputar_coord <- function(base, fontes) {
  base <- copy(base)[, `:=`(coord_imputada = FALSE, lat = as.numeric(lat), lon = as.numeric(lon))]
  for (f in fontes) {
    f <- f[!is.na(lat), .(uf, mun, zona, local, cep_f = cep, lat_f = lat, lon_f = lon)]
    f <- unique(f, by = c("uf", "mun", "zona", "local"))
    base[f, on = .(uf, mun, zona, local), `:=`(ok = is.na(lat) & !is.na(cep) & cep == i.cep_f, lat_f = i.lat_f, lon_f = i.lon_f)]
    base[ok == TRUE, `:=`(lat = lat_f, lon = lon_f, coord_imputada = TRUE)]
    base[, c("ok", "lat_f", "lon_f") := NULL]
  }
  base[]
}
