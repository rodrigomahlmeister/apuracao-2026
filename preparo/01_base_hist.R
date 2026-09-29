# Fase 1: bases históricas por local de votação e por município (2018 e 2022, 1º e 2º turnos).
# Entrada: dados/base/{votos_secao,secoes}_<ano>.parquet (R/00_extrair.R) e config/blocos.yaml.
# Saída:   dados/base/hist_local_<ano>.parquet, hist_mun_<ano>.parquet, municipios_ibge.parquet
#
# Unidade local = uf + mun + zona + local (NR_LOCAL_VOTACAO). Exterior (uf = "ZZ") fica marcado em `exterior`.
# Votos por bloco: PT, PL, OUTROS (demais candidatos); brancos (95) e nulos (96) à parte.

suppressPackageStartupMessages({ library(arrow); library(data.table); library(yaml); library(jsonlite) })

base   <- "dados/base"
blocos <- read_yaml("config/blocos.yaml")$eleicoes
rd <- function(f) as.data.table(read_parquet(file.path(base, f)))
gravar <- function(d, f) {
  write_parquet(d, file.path(base, f), compression = "zstd")
  message(sprintf("  %s: %s linhas", f, format(nrow(d), big.mark = ".", decimal.mark = ",")))
}

# votos de uma seção -> colunas por bloco
por_bloco <- function(v, ano) {
  b <- blocos[[as.character(ano)]]
  v[, bloco := fcase(nr_votavel %in% b$PT, "PT", nr_votavel %in% b$PL, "PL",
                     nr_votavel == 95, "brancos", nr_votavel == 96, "nulos", default = "OUTROS")]
  w <- dcast(v, turno + uf + mun + zona + secao ~ bloco, value.var = "votos", fun.aggregate = sum, fill = 0L)
  for (k in setdiff(c("PT", "PL", "OUTROS", "brancos", "nulos"), names(w))) set(w, j = k, value = 0L)
  w
}

soma <- c("secoes", "aptos", "comparecimento", "brancos", "nulos", "validos", "PT", "PL", "OUTROS")

for (ano in c(2018, 2022)) {
  message("Base ", ano)
  sec <- rd(sprintf("secoes_%d.parquet", ano))
  vot <- por_bloco(rd(sprintf("votos_secao_%d.parquet", ano)), ano)
  d <- merge(sec[, .(turno, uf, mun, zona, secao, local, aptos, comparecimento)], vot,
             by = c("turno", "uf", "mun", "zona", "secao"), all = TRUE)

  # conferência: seções sem par e diferença de comparecimento (votos x BU/detalhe)
  d[, votos_tot := PT + PL + OUTROS + brancos + nulos]
  message(sprintf("  seções só no BU/detalhe: %d | só nos votos: %d | comparecimento diverge: %d",
                  d[is.na(votos_tot), .N], d[is.na(aptos), .N], d[votos_tot != comparecimento, .N]))
  d <- d[!is.na(aptos) & !is.na(votos_tot)]
  d[, `:=`(validos = PT + PL + OUTROS, secoes = 1L, votos_tot = NULL)]

  loc <- d[, lapply(.SD, sum), by = .(turno, uf, mun, zona, local), .SDcols = soma]
  loc[, exterior := uf == "ZZ"]
  gravar(loc, sprintf("hist_local_%d.parquet", ano))

  hm <- d[, lapply(.SD, sum), by = .(turno, uf, mun), .SDcols = soma]
  hm <- merge(hm, loc[, .(locais = .N), by = .(turno, uf, mun)], by = c("turno", "uf", "mun"))
  hm[, exterior := uf == "ZZ"]
  gravar(hm, sprintf("hist_mun_%d.parquet", ano))

  print(hm[, .(aptos = sum(aptos), comparecimento = sum(comparecimento), validos = sum(validos),
                PT = sum(PT), PL = sum(PL), OUTROS = sum(OUTROS),
                pPT = round(100 * sum(PT) / sum(validos), 2), pPL = round(100 * sum(PL) / sum(validos), 2)),
            by = turno])
}

# Municípios: código TSE -> IBGE (do cm.json da divulgação 2026) + região imediata/intermediária (API do IBGE)
cm_arq <- "dados_tse/amostras_simulado/mun-e021270-cm.json"
cm <- fromJSON(cm_arq, simplifyVector = FALSE)
mun_tse <- rbindlist(lapply(cm$abr, \(a) rbindlist(lapply(a$mu, \(m)
  list(uf = toupper(a$cd), mun = as.integer(m$cd), ibge = as.integer(if (is.null(m$cdi)) NA else m$cdi), nm_mun = m$nm,
       capital = identical(m$c, "s"))))))

ibge <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/municipios?view=nivelado")
ibge <- as.data.table(ibge)[, .(ibge = as.integer(`municipio-id`), regiao = `regiao-sigla`,
                                rgi = as.integer(`regiao-imediata-id`), nm_rgi = `regiao-imediata-nome`,
                                rgint = as.integer(`regiao-intermediaria-id`))]
mun_tse <- merge(mun_tse, ibge, by = "ibge", all.x = TRUE)
# Boa Esperança do Norte (MT): criado depois de 2022 (desmembrado de Sorriso e Nova Ubiratã); ainda fora
# da API do IBGE. Usa a região de Sorriso (IBGE 5107925). Sem base própria em 2022: ver pareamento (Fase 2).
sorriso <- mun_tse[ibge == 5107925, .(regiao, rgi, nm_rgi, rgint)]
mun_tse[is.na(rgi) & uf == "MT", c("regiao", "rgi", "nm_rgi", "rgint") := sorriso]
message(sprintf("  municípios TSE: %d | sem região imediata: %d (exterior: %d)", nrow(mun_tse),
                mun_tse[is.na(rgi), .N], mun_tse[is.na(rgi) & uf == "ZZ", .N]))
gravar(mun_tse, "municipios_ibge.parquet")
