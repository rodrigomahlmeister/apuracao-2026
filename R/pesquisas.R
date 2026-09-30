# Lê as pesquisas (CSV), converte para votos válidos em três blocos (PT, PL, OUTROS), tira a média simples
# e grava o prior em YAML: média por bloco, desvio-padrão do prior e razões log (PT/OUTROS, PL/OUTROS; 2T: PT/PL).
# Uso: Rscript R/pesquisas.R [csv = config/pesquisas.csv] [saida = config/prior.yaml] [turno = 1]
# O modelo usa a média e aplica ao prior um dp de PAR_MOD$prior_dp (4 p.p.). Sem config/prior.yaml, o prior é swing zero.
# A coluna do candidato do PL pode se chamar `flavio`, `bolsonaro` ou `pl` (para reusar o esquema em 2022).
suppressPackageStartupMessages({ library(data.table); library(yaml) })

ler_pesquisas <- function(arq) {
  d <- as.data.table(read.csv(arq, comment.char = "#", strip.white = TRUE, colClasses = "character"))
  if (!nrow(d)) stop("nenhuma pesquisa em ", arq)
  num <- function(x) { x <- sub(",", ".", x, fixed = TRUE); x[x == ""] <- NA; as.numeric(x) }
  pl <- intersect(c("flavio", "bolsonaro", "pl"), names(d))[1]
  if (is.na(pl)) stop("falta a coluna do candidato do PL (flavio/bolsonaro/pl)")
  d[, `:=`(PT = num(lula), PL = num(get(pl)), OUTROS = num(outros))]
  d[is.na(OUTROS), OUTROS := 0]
  if ("base" %in% names(d) && !all(tolower(d$base) %in% c("totais", "validos", ""))) stop("base deve ser 'totais' ou 'validos'")
  # válidos: exclui brancos/nulos e indecisos e reescala (vale para as duas bases)
  d[, tot := PT + PL + OUTROS]
  d[, `:=`(PT = PT / tot, PL = PL / tot, OUTROS = OUTROS / tot)]
  d[]
}

# Pesquisas estaduais (config/pesquisas_uf.csv: uf, lula, flavio, outros; colunas extras são ignoradas):
# média simples por UF, em válidos.
# Lida a cada ciclo da noite; arquivo ausente, vazio ou com erro -> NULL (a tabela da página sai sem a diferença).
pesquisas_uf <- function(arq = "config/pesquisas_uf.csv") {
  if (!file.exists(arq) || sum(!grepl("^[[:space:]]*(#|$)", readLines(arq, warn = FALSE))) < 2) return(NULL)   # só cabeçalho
  tryCatch({
    d <- ler_pesquisas(arq)
    d[, .(PT = mean(PT), PL = mean(PL), n = .N), by = .(uf = toupper(trimws(uf)))]
  }, error = function(e) { message("pesquisas por UF ignoradas: ", conditionMessage(e)); NULL })
}

prior_de <- function(d, turno = 1, dp_pp = 3) {
  m <- d[, lapply(.SD, mean), .SDcols = c("PT", "PL", "OUTROS")]
  lr <- if (turno == 1) list(PT_OUTROS = log(m$PT / m$OUTROS), PL_OUTROS = log(m$PL / m$OUTROS))
        else list(PT_PL = log(m$PT / m$PL))
  list(turno = as.integer(turno), n_pesquisas = nrow(d), institutos = d$instituto,
       media_validos = lapply(as.list(m), \(x) round(x, 5)),
       dp_validos = list(PT = dp_pp / 100, PL = dp_pp / 100, OUTROS = dp_pp / 100),
       log_ratios = lapply(lr, \(x) round(x, 5)),
       gerado_em = format(Sys.time(), "%Y-%m-%d %H:%M"))
}

if (sys.nframe() == 0) {
  a <- commandArgs(TRUE)
  arq <- if (length(a) >= 1) a[1] else "config/pesquisas.csv"
  out <- if (length(a) >= 2) a[2] else "config/prior.yaml"
  tn  <- if (length(a) >= 3) as.integer(a[3]) else 1L
  dp  <- if (length(a) >= 4) as.numeric(a[4]) else 3
  d <- ler_pesquisas(arq)
  pr <- prior_de(d, tn, dp)
  write_yaml(pr, out)
  print(d[, .(instituto, campo_fim, PT = round(100 * PT, 1), PL = round(100 * PL, 1), OUTROS = round(100 * OUTROS, 1))])
  cat(sprintf("média (válidos): PT %.1f | PL %.1f | OUTROS %.1f  -> %s\n",
              100 * pr$media_validos$PT, 100 * pr$media_validos$PL, 100 * pr$media_validos$OUTROS, out))
}
