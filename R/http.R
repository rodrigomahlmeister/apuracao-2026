# Cliente HTTP para os servidores de divulgação do TSE: limite de taxa, requisições paralelas em lotes,
# ETag (só baixa o que mudou), registro de 404 (nunca repetir) e pausa total em caso de bloqueio.
# Regras do TSE: até 100 req/s por IP; acima disso, bloqueio de 10 min, e qualquer tentativa durante o
# bloqueio reinicia a contagem. Por isso 429/403 nunca são repetidos: tudo para por BLOQUEIO_MIN.
suppressPackageStartupMessages({ library(httr2); library(data.table); library(yaml) })

`%||%` <- function(a, b) if (is.null(a)) b else a
logmsg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)

carregar_params <- function(arq = "config/params.yaml") {
  p <- read_yaml(arq)
  p$raiz <- p$ambientes[[p$ambiente]]$raiz
  p
}

BLOQUEIO_MIN <- 11
LOTE <- 200

http_estado <- new.env()
http_estado$etag <- character()
http_estado$nao_existe <- character()
http_estado$bloqueado_ate <- as.POSIXct(NA)

zerar_etags <- function() http_estado$etag <- character()

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
    req_retry(max_tries = p$http$tentativas, retry_on_failure = TRUE,
              is_transient = \(resp) resp_status(resp) %in% c(500, 502, 503, 504),
              backoff = \(i) min(2^i, 60)) |>
    req_error(is_error = \(resp) FALSE)
  et <- http_estado$etag[url]
  if (condicional && !is.na(et)) r <- req_headers(r, `If-None-Match` = et)
  r
}

baixar_lote <- function(urls, p, condicional) {
  reqs <- lapply(urls, req_tse, p = p, condicional = condicional)
  resps <- req_perform_parallel(reqs, max_active = p$http$max_ativos, on_error = "continue", progress = FALSE)
  rbindlist(Map(function(u, r) {
    if (inherits(r, "error")) return(data.table(url = u, status = NA_integer_, etag = NA_character_,
                                                erro = conditionMessage(r), corpo = list(NULL)))
    st <- resp_status(r)
    data.table(url = u, status = st, etag = resp_header(r, "etag") %||% NA_character_, erro = NA_character_,
               corpo = list(if (st == 200) resp_body_string(r, "UTF-8")))
  }, urls, resps))
}

# Baixa URLs em lotes paralelos; devolve url, status, etag, erro, corpo (texto das respostas 200).
baixar <- function(urls, p, condicional = TRUE) {
  urls <- setdiff(unique(urls), http_estado$nao_existe)
  if (!length(urls)) return(data.table(url = character(), status = integer(), etag = character(),
                                       erro = character(), corpo = list()))
  out <- rbindlist(lapply(split(urls, ceiling(seq_along(urls) / LOTE)), \(u) {
    esperar_bloqueio()
    r <- baixar_lote(u, p, condicional)
    if (any(r$status %in% c(429, 403))) {
      http_estado$bloqueado_ate <- Sys.time() + BLOQUEIO_MIN * 60
      logmsg(sprintf("BLOQUEIO (HTTP %s): parando tudo por %d min", paste(unique(r$status[r$status %in% c(429, 403)]), collapse = "/"), BLOQUEIO_MIN))
    }
    r
  }))
  novos404 <- out[status == 404, url]
  if (length(novos404)) {
    http_estado$nao_existe <- union(http_estado$nao_existe, novos404)
    logmsg("404 (não será repetida): ", length(novos404), " URL(s), ex.: ", novos404[1])
  }
  ok <- out[status == 200 & !is.na(etag)]
  if (condicional && nrow(ok)) http_estado$etag[ok$url] <- ok$etag
  out
}
