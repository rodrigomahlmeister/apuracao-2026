# Cliente da divulgação de resultados do TSE (simulado | oficial).
# Diretórios vêm do ele-c.json (campo `arq`); nomes de arquivo seguem o padrão confirmado no simulado 2026
# (ver docs/notas_tecnicas.md). Nenhuma URL fora desses padrões é gerada.

source("estudos/00_utils.R")

# ---- parsers (funções puras: texto JSON -> data.table) ------------------------

n_ <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
s_ <- function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = ",")   # listas -> "a,b"

# rbindlist(lapply(...)) com fill
achatar <- function(x, f) rbindlist(lapply(x, f), fill = TRUE)

meta_de <- function(j) data.table(dg = s_(j$dg), hg = s_(j$hg), idg = s_(j$idg))

# ele-c.json: pleitos/eleições/cargos + templates de diretório
parse_elec <- function(txt) {
  j <- fromJSON(txt, simplifyVector = FALSE)
  el <- achatar(j$pl, \(pl) achatar(pl$e, \(e) achatar(e$abr, \(a) achatar(a$cp, \(cp)
    data.table(pleito = pl$cd, ciclo = s_(pl$c), eleicao = e$cd, cdt2 = s_(e$cdt2), turno = e$t,
               tp = e$tp, nm = e$nm, abr = a$cd, cargo = cp$cd, ds_cargo = cp$ds)))))
  dirs <- achatar(j$arq, as.data.table)
  list(meta = meta_de(j), eleicoes = el, dirs = dirs)
}

# mun-e<ele>-cm.json: municípios por UF
parse_cm <- function(txt) {
  j <- fromJSON(txt, simplifyVector = FALSE)
  achatar(j$abr, \(a) achatar(a$mu, \(m)
    data.table(uf = a$cd, mun = m$cd, ibge = s_(m$cdi), nm = m$nm, capital = identical(m$c, "s"),
               zonas = paste(unlist(m$z), collapse = ","))))
}

# <uf>-p<pleito>-cs.json: seções por município/zona, com data/hora (vetorizado por zona: SP tem ~100 mil seções)
parse_cs <- function(txt) {
  j <- fromJSON(txt, simplifyVector = FALSE)
  out <- list()
  for (a in j$abr) for (m in a$mu) for (z in m$zon) {
    sec <- z$sec
    campo <- function(f) vapply(sec, \(s) s_(s[[f]]), "")
    out[[length(out) + 1]] <- list(uf = a$cd, mun = m$cd, zona = z$cd, secao = campo("ns"),
                                   da = campo("da"), ha = campo("ha"), nsa = campo("nsa"), nsp = campo("nsp"))
  }
  rbindlist(out)
}

# blocos s (seções) e e (eleitorado): só os campos numéricos
# s: ts total, st totalizadas, snt não totalizadas, si/sni instaladas/não instaladas, sa/sna apuradas/não apuradas
# e: mesmos recortes para o eleitorado (te, est, esnt, esi, esni, esa, esna) + comparecimento c e abstenção a
bloco_se <- function(s, e) {
  list(ts = n_(s$ts), st = n_(s$st), snt = n_(s$snt), pstn = n_(s$pstn), si = n_(s$si), sni = n_(s$sni),
       sa = n_(s$sa), sna = n_(s$sna),
       te = n_(e$te), est = n_(e$est), esnt = n_(e$esnt), pestn = n_(e$pestn), esi = n_(e$esi),
       esni = n_(e$esni), esa = n_(e$esa), esna = n_(e$esna), c = n_(e$c), a = n_(e$a))
}

# <abr>-e<ele>-ab.json: andamento por abrangência (BR -> UFs; UF -> municípios)
parse_ab <- function(txt) {
  j <- fromJSON(txt, simplifyVector = FALSE)
  d <- rbindlist(lapply(j$abr, \(a) c(
    list(tpabr = a$tpabr, cdabr = a$cdabr, dt = s_(a$dt), ht = s_(a$ht),
         munf = n_(a$munf), munpt = n_(a$munpt), munnr = n_(a$munnr)),
    bloco_se(a$s, a$e))))
  d[, `:=`(dg = s_(j$dg), hg = s_(j$hg), idg = s_(j$idg))]
  d
}

# <abr>-c0001-e<ele>-u.json: resultado; devolve totais (1 linha) e candidatos
parse_u <- function(txt) {
  j <- fromJSON(txt, simplifyVector = FALSE)
  v <- j$v
  tot <- setDT(c(list(dg = s_(j$dg), hg = s_(j$hg), idg = s_(j$idg), tpabr = j$tpabr, cdabr = j$cdabr,
                      dt = s_(j$dt), ht = s_(j$ht)),
                 bloco_se(j$s, j$e),
                 list(tv = n_(v$tv), vv = n_(v$vv), vb = n_(v$vb), vn = n_(v$tvn),
                      van = n_(v$van), vansj = n_(v$vansj))))
  linhas <- list()
  for (cg in j$carg) for (ag in cg$agr) for (pa in ag$par) for (cd in pa$cand)
    linhas[[length(linhas) + 1]] <- list(cargo = cg$cd, n = cd$n, nm = cd$nm, partido = s_(pa$sg),
                                         vap = n_(cd$vap), pvapn = n_(cd$pvapn), st = s_(cd$st),
                                         dvt = s_(cd$dvt), tpabr = j$tpabr, cdabr = j$cdabr)
  list(tot = tot, cand = rbindlist(linhas))
}


# p<pleito>-<uf>-m<mun>-z<zona>-s<secao>-aux.json: status da seção e arquivos do BU
# arq pode vir como texto ("o00452...-bu.dat") ou como objeto {nm, tp} (formato do oficial de 2024)
parse_aux <- function(txt) {
  j <- fromJSON(txt, simplifyVector = FALSE)
  arq <- achatar(j$hashes, \(h) if (length(h$arq)) {
    nm <- vapply(h$arq, \(a) if (is.list(a)) s_(a$nm) else s_(a), "")
    tp <- vapply(h$arq, \(a) if (is.list(a)) s_(a$tp) else tools::file_ext(s_(a)), "")
    data.table(hash = s_(h$hash), st_hash = s_(h$st), dr = s_(h$dr), hr = s_(h$hr), arq = nm, tp = tp)
  })
  list(st = s_(j$st), meta = meta_de(j), arq = arq)
}

# ---- URLs ----------------------------------------------------------------------

# ctx: raiz, ciclo, eleicao (5 dígitos), pleito, dirs (templates do ele-c)
dir_de <- function(ctx, tp, uf = "br", mun = NULL, zona = NULL, secao = NULL) {
  d <- ctx$dirs$dir[ctx$dirs$tp == tp][1]
  if (is.na(d)) stop("tipo de arquivo '", tp, "' ausente do ele-c.json")
  d <- sub("<base>/<ambiente>", ctx$raiz, d, fixed = TRUE)
  d <- gsub("<ciclo>", ctx$ciclo, d, fixed = TRUE)
  d <- gsub("<cd_eleicao>", ctx$eleicao, d, fixed = TRUE)
  d <- gsub("<cd_pleito>", ctx$pleito, d, fixed = TRUE)
  d <- gsub("<uf>", uf, d, fixed = TRUE)
  if (!is.null(mun)) d <- gsub("<municipio>/<zona>/<secao>", paste(mun, zona, secao, sep = "/"), d, fixed = TRUE)
  d
}

e6 <- function(ctx) sprintf("e%06d", as.integer(ctx$eleicao))
p6 <- function(ctx) sprintf("p%06d", as.integer(ctx$pleito))

url_u   <- function(ctx, uf, mun = "") sprintf("%s/%s%s-c%04d-%s-u.json", dir_de(ctx, "u", uf), uf, mun,
                                               as.integer(ctx$cargo), e6(ctx))
url_ab  <- function(ctx, uf) sprintf("%s/%s-%s-ab.json", dir_de(ctx, "ab", uf), uf, e6(ctx))
url_cm  <- function(ctx) sprintf("%s/mun-%s-cm.json", dir_de(ctx, "cm"), e6(ctx))
url_cs  <- function(ctx, uf) sprintf("%s/%s-%s-cs.json", dir_de(ctx, "cs", uf), uf, p6(ctx))
url_aux <- function(ctx, uf, mun, zona, secao)
  sprintf("%s/%s-%s-m%s-z%s-s%s-aux.json", dir_de(ctx, "aux", uf, mun, zona, secao), p6(ctx), uf, mun, zona, secao)

# ---- contexto da eleição a partir do ele-c ---------------------------------------

contexto <- function(p) {
  r <- baixar(sprintf("%s/comum/config/ele-c.json", p$raiz), p, condicional = FALSE)
  stopifnot(r$status == 200)
  ec <- parse_elec(r$corpo[[1]])
  el <- ec$eleicoes[cargo == as.character(p$cargo) & turno == as.character(p$turno) & abr == "br"]
  if (!is.null(p$ciclo_eleicao)) el <- el[is.na(ciclo) | ciclo == p$ciclo_eleicao]
  stopifnot(nrow(el) == 1)
  list(raiz = p$raiz, ciclo = p$ciclo_eleicao, eleicao = el$eleicao, pleito = el$pleito, cdt2 = el$cdt2,
       cargo = p$cargo, dirs = ec$dirs, elec_bruto = r)
}

# ---- ciclo -----------------------------------------------------------------------
# Cache do último parse por URL: resposta 304 (ETag igual) reaproveita o parse anterior.
cache_parse <- new.env()

# Estado da ingestão incremental: assinatura do andamento (ab) na última baixa válida de cada município
inc <- new.env()
inc$n <- 0
inc$ultimo <- data.table(mun = character(), assin = character())
inc$rodada <- 1L                 # sobe quando o % totalizado cai (simulado reiniciado)
inc$st_ant <- numeric()          # seções totalizadas no ciclo anterior, por abrangência (br + UFs)

# Detecta reinício: alguma abrangência (BR ou UF) com menos seções totalizadas que no ciclo anterior.
# Zera marcas de "já baixado", ETags e cache de parse e abre nova rodada.
checar_reinicio <- function(ab) {
  st <- ab[tpabr %in% c("br", "uf")][, setNames(st, cdabr)]
  st <- st[!duplicated(names(st))]
  comum <- intersect(names(st), names(inc$st_ant))
  caiu <- comum[st[comum] < inc$st_ant[comum]]
  if (length(caiu)) {
    inc$rodada <- inc$rodada + 1L
    inc$ultimo <- inc$ultimo[0]; inc$n <- 0
    zerar_http(); rm(list = ls(cache_parse), envir = cache_parse)
    logmsg(sprintf("REINÍCIO detectado (%s): nova rodada %d", paste(head(caiu, 5), collapse = ","), inc$rodada))
  }
  inc$st_ant <- st
  length(caiu) > 0
}

# zonas das cidades grandes (config/cidades_grandes.csv) com a URL do resultado por zona (EA20):
# <uf><mun>-z<zona>-c<cargo>-e<eleição>-u.json no mesmo diretório dos -u
zonas_grandes <- function(ctx, cm, p, arq = "config/cidades_grandes.csv") {
  if (!file.exists(arq)) return(data.table(uf = character(), mun = character(), zona = character(), url = character()))
  g <- fread(arq, colClasses = c(uf = "character", mun = "character"))
  z <- cm[g[, .(uf, mun)], on = .(uf, mun), nomatch = 0][, .(zona = strsplit(zonas, ",")[[1]]), by = .(uf, mun)]
  z[, url := sprintf("%s/%s%s-z%s-c%04d-%s-u.json", vapply(uf, \(u) dir_de(ctx, "u", u), ""), uf, mun, zona,
                     as.integer(ctx$cargo), e6(ctx))][]
}

# linha de conferência (comparar com o app oficial): % totalizadas, válidos e 3 primeiros
conferencia <- function(u_tot, u_cand, abr = c("br", "sp", "ba", "rs")) {
  t <- u_tot[cdabr %in% abr & tpabr %in% c("br", "uf"), .(cdabr, dg, hg, pstn = 100 * st / ts, st, ts, vv)]   # pstn do arquivo vem vazio durante a apuração
  c3 <- merge(u_cand[cdabr %in% abr & tpabr %in% c("br", "uf") & dvt == "Válido"], t[, .(cdabr, vv)], by = "cdabr")
  c3 <- c3[order(-vap), head(.SD, 3), by = cdabr][
    , .(top3 = paste(sprintf("%s %.2f%%", n, 100 * vap / vv), collapse = " | ")), by = cdabr]
  merge(t, c3, by = "cdabr", all.x = TRUE)
}

# municípios a baixar: sem registro ou com assinatura diferente da última baixa válida
selecionar_mun <- function(atual, ultimo, completo = FALSE) {
  if (completo || !nrow(ultimo)) return(atual$mun)
  x <- merge(atual, ultimo, by = "mun", all.x = TRUE, suffixes = c("", "_ult"))
  x[is.na(assin_ult) | assin != assin_ult, mun]
}

parse_cache <- function(r, f) {
  lapply(seq_len(nrow(r)), \(i) {
    u <- r$url[i]
    if (isTRUE(r$status[i] == 200)) assign(u, f(r$corpo[[i]]), envir = cache_parse)
    if (exists(u, envir = cache_parse, inherits = FALSE)) get(u, envir = cache_parse) else NULL
  })
}

# Um ciclo completo. Grava em <out>/rodada_NN/<nome>: bruto.parquet (corpo de toda resposta 200),
# stats.parquet e tabelas processadas (ab, u_tot, u_cand, cs, aux); acrescenta <out>/conferencia.csv.
# A rodada muda quando o simulado é reiniciado (queda do % totalizado). Devolve as stats.
rodar_ciclo <- function(ctx, p, cm, out, nome, secoes_aux = NULL) {
  stopifnot(all(nchar(cm$mun) == 5))            # código TSE do município sempre com 5 dígitos (do cm.json)
  ufs <- unique(cm$uf)
  resp <- list(); t_etapa <- list()
  etapa <- function(nome, urls, binario = FALSE) {
    t0 <- Sys.time()
    r <- baixar(urls, p, binario = binario)
    t_etapa[[nome]] <<- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (nrow(r)) r[, etapa := nome]
    logmsg(sprintf("%-8s %5d req  200:%-5d 304:%-5d 404:%-3d outros:%-3d  %.1fs", nome, nrow(r),
                   sum(r$status == 200, na.rm = TRUE), sum(r$status == 304, na.rm = TRUE),
                   sum(r$status == 404, na.rm = TRUE), sum(!r$status %in% c(200, 304, 404)), t_etapa[[nome]]))
    ruins <- r[is.na(status) | !status %in% c(200, 304, 404)]
    if (nrow(ruins)) logmsg("  falhas: ", paste(head(sprintf("%s [%s %s]", ruins$url, ruins$status,
                                                             ruins$erro), 3), collapse = " | "))
    resp[[nome]] <<- r
    r
  }

  if (is.null(cm$url_u)) cm[, url_u := url_u(ctx, uf, mun)]
  r_ab <- etapa("ab", c(url_ab(ctx, "br"), url_ab(ctx, ufs)))
  ab <- rbindlist(parse_cache(r_ab, parse_ab), fill = TRUE)
  if (checar_reinicio(ab)) {                    # reinício: reprocessa ab sem cache
    r_ab <- etapa("ab", c(url_ab(ctx, "br"), url_ab(ctx, ufs)))
    ab <- rbindlist(parse_cache(r_ab, parse_ab), fill = TRUE)
    resp$ab <- r_ab
  }
  dir <- file.path(out, sprintf("rodada_%02d", inc$rodada), nome)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # resultado municipal onde o andamento (ab da UF, por município) mudou + fatia rotativa de 1/K dos
  # municípios a cada ciclo (com ETag), em vez de varrer todos de uma vez (a varredura completa custava ~200 s)
  inc$n <- inc$n + 1
  K <- p$ciclo$varredura_completa_cada
  abm <- ab[tpabr == "mun", .(mun = cdabr, st_ab = st, assin = paste(st, est, c, dt, ht))]
  sel <- selecionar_mun(unique(abm[, .(mun, assin)], by = "mun"), inc$ultimo, completo = inc$n == 1)
  n_mudou <- length(sel)
  fatia <- cm$mun[seq_len(nrow(cm)) %% K == inc$n %% K]
  sel <- union(union(sel, fatia), setdiff(cm$mun, abm$mun))   # sem andamento no ab: sempre baixa
  logmsg(sprintf("u_mun: %d de %d municípios (%d com andamento novo + fatia rotativa 1/%d)", length(sel), nrow(cm), n_mudou, K))

  # cs ANTES dos -u: o -u municipal tende a ficar atrás do cs/ab; baixar o cs depois só aumenta a defasagem
  r_cs  <- etapa("cs",    url_cs(ctx, ufs))
  r_ubr <- etapa("u_uf",  c(url_u(ctx, "br"), url_u(ctx, ufs)))
  r_mun <- etapa("u_mun", cm[mun %in% sel, url_u])
  # resultado por zona (EA20) nas cidades grandes cujo município foi selecionado neste ciclo
  if (is.null(inc$zonas)) inc$zonas <- zonas_grandes(ctx, cm, p)
  r_zon <- etapa("u_zona", inc$zonas[mun %in% sel, url])

  us_uf  <- parse_cache(r_ubr, parse_u)
  us_mun <- parse_cache(r_mun, parse_u)
  invisible(parse_cache(r_zon, parse_u))
  zs <- lapply(inc$zonas$url, \(u) if (exists(u, envir = cache_parse, inherits = FALSE)) get(u, envir = cache_parse))
  tem <- !vapply(zs, is.null, TRUE)
  u_zona <- rbindlist(Map(\(x, i) cbind(x$tot, inc$zonas[i, .(uf, mun, zona)]), zs[tem], which(tem)), fill = TRUE)
  u_zona_cand <- rbindlist(Map(\(x, i) if (nrow(x$cand)) cbind(x$cand, inc$zonas[i, .(uf, mun, zona)]), zs[tem], which(tem)), fill = TRUE)
  # marca como atualizado só se o resultado baixado já reflete o andamento do ab (o -u pode atrasar)
  ok <- rbindlist(Map(\(u, x) if (!is.null(x)) data.table(url_u = u, st_u = x$tot$st), r_mun$url, us_mun))
  if (nrow(ok)) {
    ok <- merge(merge(ok, cm[, .(url_u, mun)], by = "url_u"), abm, by = "mun")[st_u == st_ab]
    inc$ultimo <- rbind(inc$ultimo[!mun %in% ok$mun], ok[, .(mun, assin)])
  }
  us <- c(us_uf, lapply(cm$url_u, \(u) if (exists(u, envir = cache_parse, inherits = FALSE)) get(u, envir = cache_parse)))
  u_tot  <- rbindlist(lapply(us, `[[`, "tot"), fill = TRUE)
  u_cand <- rbindlist(lapply(us, `[[`, "cand"), fill = TRUE)
  cs <- rbindlist(parse_cache(r_cs, parse_cs), fill = TRUE)

  # status por seção (aux) numa amostra fixa de seções das cidades de teste
  aux <- data.table()
  if (!is.null(secoes_aux) && nrow(secoes_aux)) {
    secoes_aux[, url := url_aux(ctx, uf, mun, zona, secao)]
    r_aux <- etapa("aux", secoes_aux$url)
    pa <- parse_cache(r_aux, parse_aux)
    aux <- cbind(secoes_aux[match(r_aux$url, url)],
                 data.table(status_http = r_aux$status, st = sapply(pa, \(x) x$st %||% NA),
                            n_arq = sapply(pa, \(x) if (is.null(x)) NA_integer_ else nrow(x$arq))))
    # arquivos do BU: <dir da seção>/<hash>/<arquivo>, só dos tipos pedidos, até N seções por cidade
    arqs <- rbindlist(Map(\(u, x) if (!is.null(x) && nrow(x$arq)) cbind(url_aux = u, x$arq), r_aux$url, pa), fill = TRUE)
    if (nrow(arqs)) {
      arqs <- arqs[tp %in% p$bu_tipos | tools::file_ext(arq) %in% p$bu_tipos]
      arqs <- merge(arqs, secoes_aux[, .(url_aux = url, uf, mun, bu = bu)], by = "url_aux")[bu == TRUE]
      if (nrow(arqs)) {
        arqs[, url_bu := sprintf("%s/%s/%s", dirname(url_aux), hash, arq)]
        ja <- list.files(file.path(dirname(dir), "bu"))
        arqs <- arqs[!arq %in% ja]
        r_bu <- etapa("bu", arqs$url_bu, binario = TRUE)
        dir.create(file.path(dirname(dir), "bu"), showWarnings = FALSE)
        for (i in which(r_bu$status == 200)) writeBin(r_bu$corpo[[i]], file.path(dirname(dir), "bu", basename(r_bu$url[i])))
      }
    }
  }

  # grava: bruto (só respostas 200 de texto) + processados + stats
  todas <- rbindlist(resp, fill = TRUE)
  bruto <- todas[status == 200 & etapa != "bu", .(url, etapa, etag, corpo = vapply(corpo, \(x) x, ""))]
  write_parquet(bruto, file.path(dir, "bruto.parquet"), compression = "zstd")
  for (nm in c("ab", "u_tot", "u_cand", "u_zona", "u_zona_cand", "cs", "aux")) {
    x <- get(nm); if (nrow(x)) write_parquet(x, file.path(dir, paste0(nm, ".parquet")), compression = "zstd")
  }
  stats <- todas[, .(n = .N, s200 = sum(status == 200, na.rm = TRUE), s304 = sum(status == 304, na.rm = TRUE),
                     s404 = sum(status == 404, na.rm = TRUE), outros = sum(!status %in% c(200, 304, 404))),
                 by = etapa]
  stats[, segundos := unlist(t_etapa[etapa])]
  write_parquet(stats, file.path(dir, "stats.parquet"))
  cf <- tryCatch(conferencia(u_tot, u_cand), error = \(e) NULL)
  if (!is.null(cf) && nrow(cf)) {
    cf <- cbind(data.table(hora = format(Sys.time(), "%H:%M:%S"), rodada = inc$rodada, ciclo = nome), cf)
    fwrite(cf, file.path(out, "conferencia.csv"), append = file.exists(file.path(out, "conferencia.csv")))
    br <- cf[cdabr == "br"]
    if (nrow(br)) logmsg(sprintf("BR: %.2f%% totalizadas | válidos %s | %s", br$pstn, format(br$vv, big.mark = ".", decimal.mark = ","), br$top3))
  }
  stats
}

# versões vetorizadas (uma URL por elemento)
url_u   <- Vectorize(url_u,   c("uf", "mun"), USE.NAMES = FALSE)
url_ab  <- Vectorize(url_ab,  "uf", USE.NAMES = FALSE)
url_cs  <- Vectorize(url_cs,  "uf", USE.NAMES = FALSE)
url_aux <- Vectorize(url_aux, c("uf", "mun", "zona", "secao"), USE.NAMES = FALSE)
