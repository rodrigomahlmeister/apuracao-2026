# Leitura dos arquivos de divulgação do TSE (simulado | oficial) e ciclo de coleta.
# Diretórios vêm do arquivo de configuração de eleições (EA11, ele-c.json, campo `arq`); nomes de arquivo
# seguem as "Instruções para download dos arquivos da divulgação 2026" (docs/).
# Por ciclo: acompanhamento (EA14/EA15) -> configuração de seções (EA16) -> resultado BR/UF (EA20) ->
# resultado municipal (EA20) só dos municípios com andamento novo + uma fatia rotativa de 1/K.
source("R/http.R")
suppressPackageStartupMessages({ library(jsonlite); library(arrow) })

n_ <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
s_ <- function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = ",")
achatar <- function(x, f) rbindlist(lapply(x, f), fill = TRUE)

# ---- parsers ------------------------------------------------------------------------------------------
parse_elec <- function(txt) {                                                          # EA11
  j <- fromJSON(txt, simplifyVector = FALSE)
  el <- achatar(j$pl, \(pl) achatar(pl$e, \(e) achatar(e$abr, \(a) achatar(a$cp, \(cp)
    data.table(pleito = pl$cd, ciclo = s_(pl$c), eleicao = e$cd, cdt2 = s_(e$cdt2), turno = e$t,
               abr = a$cd, cargo = cp$cd)))))
  list(eleicoes = el, dirs = achatar(j$arq, as.data.table))
}

parse_cm <- function(txt) {                                                            # EA12
  j <- fromJSON(txt, simplifyVector = FALSE)
  achatar(j$abr, \(a) achatar(a$mu, \(m) data.table(uf = a$cd, mun = m$cd, nm = m$nm)))
}

parse_cs <- function(txt) {                                                            # EA16
  j <- fromJSON(txt, simplifyVector = FALSE)
  out <- list()
  for (a in j$abr) for (m in a$mu) for (z in m$zon) {
    sec <- z$sec
    campo <- function(f) vapply(sec, \(s) s_(s[[f]]), "")
    out[[length(out) + 1]] <- list(uf = a$cd, mun = m$cd, zona = z$cd, secao = campo("ns"),
                                   ha = campo("ha"), nsp = campo("nsp"))
  }
  rbindlist(out)
}

bloco_se <- function(s, e) list(ts = n_(s$ts), st = n_(s$st), te = n_(e$te), est = n_(e$est), c = n_(e$c))

parse_ab <- function(txt) {                                                            # EA14 / EA15
  j <- fromJSON(txt, simplifyVector = FALSE)
  rbindlist(lapply(j$abr, \(a) c(list(tpabr = a$tpabr, cdabr = a$cdabr, dt = s_(a$dt), ht = s_(a$ht)), bloco_se(a$s, a$e))))
}

parse_u <- function(txt) {                                                             # EA20
  j <- fromJSON(txt, simplifyVector = FALSE)
  tot <- setDT(c(list(tpabr = j$tpabr, cdabr = j$cdabr, dg = s_(j$dg), hg = s_(j$hg)), bloco_se(j$s, j$e),
                 list(vv = n_(j$v$vv))))
  linhas <- list()
  for (cg in j$carg) for (ag in cg$agr) for (pa in ag$par) for (cd in pa$cand)
    linhas[[length(linhas) + 1]] <- list(n = cd$n, nm = cd$nm, vap = n_(cd$vap), dvt = s_(cd$dvt),
                                         tpabr = j$tpabr, cdabr = j$cdabr)
  list(tot = tot, cand = rbindlist(linhas))
}

# ---- URLs (diretórios do EA11; nomes das instruções de download) -------------------------------------------
dir_de <- function(ctx, tp, uf = "br") {
  d <- ctx$dirs$dir[ctx$dirs$tp == tp][1]
  if (is.na(d)) stop("tipo de arquivo '", tp, "' ausente do ele-c.json")
  d <- sub("<base>/<ambiente>", ctx$raiz, d, fixed = TRUE)
  for (k in c("ciclo", "cd_eleicao", "cd_pleito", "uf")) d <- gsub(sprintf("<%s>", k), ctx[[k]] %||% uf, d, fixed = TRUE)
  gsub("<uf>", uf, d, fixed = TRUE)
}
e6 <- function(ctx) sprintf("e%06d", as.integer(ctx$cd_eleicao))
p6 <- function(ctx) sprintf("p%06d", as.integer(ctx$cd_pleito))
url_u  <- function(ctx, uf, mun = "") mapply(\(u, m) sprintf("%s/%s%s-c0001-%s-u.json", dir_de(ctx, "u", u), u, m, e6(ctx)), uf, mun, USE.NAMES = FALSE)
url_ab <- function(ctx, uf) vapply(uf, \(u) sprintf("%s/%s-%s-ab.json", dir_de(ctx, "ab", u), u, e6(ctx)), "", USE.NAMES = FALSE)
url_cs <- function(ctx, uf) vapply(uf, \(u) sprintf("%s/%s-%s-cs.json", dir_de(ctx, "cs", u), u, p6(ctx)), "", USE.NAMES = FALSE)
url_cm <- function(ctx) sprintf("%s/mun-%s-cm.json", dir_de(ctx, "cm"), e6(ctx))

# Eleição federal de Presidente do turno pedido, a partir do ele-c.json
contexto <- function(p, turno = 1) {
  r <- baixar(sprintf("%s/comum/config/ele-c.json", p$raiz), p, condicional = FALSE)
  stopifnot(isTRUE(r$status == 200))
  ec <- parse_elec(r$corpo[[1]])
  el <- ec$eleicoes[cargo == "1" & abr == "br" & turno == "1" & (is.na(ciclo) | ciclo == p$ciclo_eleicao)]
  stopifnot(nrow(el) == 1)
  cd <- if (turno == 1) el$eleicao else el$cdt2
  if (is.na(cd) || cd == "") stop("código da eleição do ", turno, "º turno ainda não publicado no ele-c.json")
  list(raiz = p$raiz, ciclo = p$ciclo_eleicao, cd_eleicao = cd, cd_pleito = el$pleito, dirs = ec$dirs, turno = turno,
       elec = r$corpo[[1]])
}

# ---- estado entre ciclos ------------------------------------------------------------------------------------
cache_parse <- new.env()
inc <- new.env(); inc$n <- 0; inc$rodada <- 1L; inc$st_ant <- numeric()
inc$ultimo <- data.table(mun = character(), assin = character())

parse_cache <- function(r, f) lapply(seq_len(nrow(r)), \(i) {
  u <- r$url[i]
  if (isTRUE(r$status[i] == 200)) assign(u, f(r$corpo[[i]]), envir = cache_parse)
  if (exists(u, envir = cache_parse, inherits = FALSE)) get(u, envir = cache_parse)
})
do_cache <- function(urls) lapply(urls, \(u) if (exists(u, envir = cache_parse, inherits = FALSE)) get(u, envir = cache_parse))

# municípios cujo andamento (assinatura) mudou desde a última baixa válida
selecionar_mun <- function(atual, ultimo, completo = FALSE) {
  if (completo || !nrow(ultimo)) return(atual$mun)
  x <- merge(atual, ultimo, by = "mun", all.x = TRUE, suffixes = c("", "_ult"))
  x[is.na(assin_ult) | assin != assin_ult, mun]
}

# simulado reiniciado (seções totalizadas caem em BR ou em alguma UF): zera o estado e abre nova rodada
checar_reinicio <- function(ab) {
  st <- ab[tpabr %in% c("br", "uf")][!duplicated(cdabr)][, setNames(st, cdabr)]
  comum <- intersect(names(st), names(inc$st_ant))
  caiu <- comum[st[comum] < inc$st_ant[comum]]
  if (length(caiu)) {
    inc$rodada <- inc$rodada + 1L; inc$ultimo <- inc$ultimo[0]; inc$n <- 0
    zerar_etags(); rm(list = ls(cache_parse), envir = cache_parse)
    logmsg(sprintf("REINÍCIO detectado (%s): nova rodada %d", paste(head(caiu, 5), collapse = ","), inc$rodada))
  }
  inc$st_ant <- st
  length(caiu) > 0
}

# ---- um ciclo de coleta ---------------------------------------------------------------------------------------
# Grava os arquivos brutos que mudaram em <dir>/bruto.parquet e devolve as tabelas do ciclo.
coletar <- function(ctx, p, cm, dir) {
  ufs <- unique(cm$uf)
  if (is.null(cm$url_u)) cm[, url_u := url_u(ctx, uf, mun)]
  resp <- list()
  etapa <- function(nome, urls) {
    t0 <- Sys.time(); r <- baixar(urls, p)
    if (nrow(r)) r[, etapa := nome]
    ruins <- r[is.na(status) | !status %in% c(200, 304, 404)]
    logmsg(sprintf("%-6s %5d req  novos:%-5d iguais:%-5d falhas:%-3d %.1fs", nome, nrow(r), sum(r$status == 200, na.rm = TRUE),
                   sum(r$status == 304, na.rm = TRUE), nrow(ruins), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    resp[[nome]] <<- r
    r
  }
  r_ab <- etapa("ab", c(url_ab(ctx, "br"), url_ab(ctx, ufs)))
  ab <- rbindlist(parse_cache(r_ab, parse_ab), fill = TRUE)
  if (checar_reinicio(ab)) {
    r_ab <- etapa("ab", c(url_ab(ctx, "br"), url_ab(ctx, ufs)))
    ab <- rbindlist(parse_cache(r_ab, parse_ab), fill = TRUE)
  }
  inc$n <- inc$n + 1
  K <- p$ciclo$fatia_rotativa
  abm <- ab[tpabr == "mun", .(mun = cdabr, st_ab = st, assin = paste(st, est, c, dt, ht))][!duplicated(mun)]
  sel <- selecionar_mun(abm[, .(mun, assin)], inc$ultimo, completo = inc$n == 1)
  sel <- union(union(sel, cm$mun[seq_len(nrow(cm)) %% K == inc$n %% K]), setdiff(cm$mun, abm$mun))

  r_cs  <- etapa("cs",    url_cs(ctx, ufs))                       # antes dos -u (o -u tende a atrasar)
  r_uf  <- etapa("u_uf",  c(url_u(ctx, "br"), url_u(ctx, ufs)))
  r_mun <- etapa("u_mun", cm[mun %in% sel, url_u])
  us_uf <- parse_cache(r_uf, parse_u)
  us_mun <- parse_cache(r_mun, parse_u)
  ok <- rbindlist(Map(\(u, x) if (!is.null(x)) data.table(url_u = u, st_u = x$tot$st), r_mun$url, us_mun))
  if (nrow(ok)) {                       # marca como atualizado só se o -u já reflete o andamento do ab
    ok <- merge(merge(ok, cm[, .(url_u, mun)], by = "url_u"), abm, by = "mun")[st_u == st_ab]
    inc$ultimo <- rbind(inc$ultimo[!mun %in% ok$mun], ok[, .(mun, assin)])
  }
  us <- c(us_uf, do_cache(cm$url_u))
  cs <- rbindlist(parse_cache(r_cs, parse_cs), fill = TRUE)

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  todas <- rbindlist(resp, fill = TRUE)
  bruto <- todas[status == 200, .(url, etapa, etag, corpo = vapply(corpo, \(x) x, ""))]
  if (nrow(bruto)) write_parquet(bruto, file.path(dir, "bruto.parquet"), compression = "zstd")
  list(ab = ab, cs = cs, u_tot = rbindlist(lapply(us, `[[`, "tot"), fill = TRUE),
       u_cand = rbindlist(lapply(us, `[[`, "cand"), fill = TRUE), rodada = inc$rodada,
       n_req = nrow(todas))
}
