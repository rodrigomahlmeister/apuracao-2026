# Utilidades: configuração, cliente HTTP (fila, limite de taxa, retry, ETag, registro de 404) e log.

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(data.table); library(arrow); library(yaml)
})

carregar_params <- function(arq = "config/params.yaml") {
  p <- read_yaml(arq)
  p$raiz <- p$ambientes[[p$ambiente]]$raiz
  p
}

logmsg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)

# ---- cliente HTTP ------------------------------------------------------------
# Estado por sessão: ETag por URL (só baixa o que mudou), URLs que deram 404 (nunca repetir) e
# bloqueio: o TSE bloqueia o IP por 10 min e qualquer tentativa durante o bloqueio reinicia a contagem.
# Por isso 429/403 NÃO são repetidos: ao recebê-los, tudo para por BLOQUEIO_MIN, sem nenhuma requisição.

`%||%` <- function(a, b) if (is.null(a)) b else a

BLOQUEIO_MIN <- 11
LOTE <- 200                 # requisições por lote paralelo (o bloqueio é checado entre lotes)

http_estado <- new.env()
http_estado$etag <- character()
http_estado$nao_existe <- character()
http_estado$bloqueado_ate <- as.POSIXct(NA)
http_estado$n_bloqueios <- 0L

zerar_http <- function() {                      # nova rodada (simulado reiniciado): esquece ETags
  http_estado$etag <- character()
}

esperar_bloqueio <- function() {
  b <- http_estado$bloqueado_ate
  if (!is.na(b) && Sys.time() < b) {
    s <- as.numeric(difftime(b, Sys.time(), units = "secs"))
    logmsg(sprintf("bloqueio: aguardando %.0f s sem nenhuma requisição", s))
    Sys.sleep(s)
  }
}

req_tse <- function(url, p, condicional = TRUE) {
  r <- request(url) |>
    req_user_agent("apuracao-projecao-2026 (pesquisa academica)") |>
    req_headers(`Accept-Encoding` = "gzip") |>
    req_timeout(p$http$timeout_s) |>
    req_throttle(capacity = p$http$req_por_seg, fill_time_s = 1, realm = "tse") |>
    req_retry(max_tries = p$http$tentativas, retry_on_failure = TRUE,   # inclui timeout
              is_transient = \(resp) resp_status(resp) %in% c(500, 502, 503, 504),   # 429/403: nunca
              backoff = \(i) min(2^i, 60)) |>
    req_error(is_error = \(resp) FALSE)          # status tratado por nós
  et <- http_estado$etag[url]
  if (condicional && !is.na(et)) r <- req_headers(r, `If-None-Match` = et)
  r
}

baixar_lote <- function(urls, p, binario, condicional) {
  reqs <- lapply(urls, req_tse, p = p, condicional = condicional)
  resps <- req_perform_parallel(reqs, max_active = p$http$max_ativos, on_error = "continue", progress = FALSE)
  rbindlist(Map(function(u, r) {
    if (inherits(r, "error")) return(data.table(url = u, status = NA_integer_, etag = NA_character_,
                                                erro = conditionMessage(r), corpo = list(NULL)))
    st <- resp_status(r)
    data.table(url = u, status = st, etag = resp_header(r, "etag") %||% NA_character_,
               erro = NA_character_,
               corpo = list(if (st == 200) { if (binario) resp_body_raw(r) else resp_body_string(r, "UTF-8") }))
  }, urls, resps))
}

vazio_http <- function() data.table(url = character(), status = integer(), etag = character(),
                                    erro = character(), corpo = list(), t_req = numeric())

# Baixa um vetor de URLs em lotes paralelos. Devolve data.table com url, status, etag, corpo (texto ou raw).
# URLs já marcadas como 404 são puladas. `binario = TRUE` para arquivos de BU.
baixar <- function(urls, p, binario = FALSE, condicional = TRUE) {
  urls <- setdiff(unique(urls), http_estado$nao_existe)
  if (!length(urls)) return(vazio_http())
  t0 <- Sys.time()
  lotes <- split(urls, ceiling(seq_along(urls) / LOTE))
  out <- rbindlist(lapply(lotes, \(u) {
    esperar_bloqueio()
    r <- baixar_lote(u, p, binario, condicional)
    if (any(r$status %in% c(429, 403))) {
      http_estado$bloqueado_ate <- Sys.time() + BLOQUEIO_MIN * 60
      http_estado$n_bloqueios <- http_estado$n_bloqueios + 1L
      logmsg(sprintf("BLOQUEIO (%s): parando tudo por %d min", paste(unique(r$status[r$status %in% c(429, 403)]), collapse = "/"),
                     BLOQUEIO_MIN))
    }
    r
  }))
  out[, t_req := as.numeric(difftime(Sys.time(), t0, units = "secs"))]
  novos404 <- out[status == 404, url]
  if (length(novos404)) {
    http_estado$nao_existe <- union(http_estado$nao_existe, novos404)
    logmsg("404 (não será repetida): ", length(novos404), " URL(s), ex.: ", novos404[1])
  }
  ok <- out[status == 200 & !is.na(etag)]
  if (condicional && nrow(ok)) http_estado$etag[ok$url] <- ok$etag
  out
}
