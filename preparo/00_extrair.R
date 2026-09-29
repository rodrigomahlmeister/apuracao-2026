# Extrai dos arquivos brutos do TSE (dados_tse/) só o necessário, em parquet enxuto (dados/base/).
# Os CSVs são lidos em streaming (arrow), descompactados um de cada vez em pasta temporária:
# o boletim de urna de SP (1º turno) tem 9,4 GB descompactado.
#
# Saídas (uma linha por seção; chaves inteiras, sem texto repetido):
#   votos_secao_<ano>.parquet        turno, uf, mun, zona, secao, nr_votavel, votos      (Presidente)
#   secoes_<ano>.parquet             turno, uf, mun, zona, secao, local, aptos, comparecimento,
#                                    abstencoes, dt_recebido (+ extras de cada fonte)
#   eleitorado_secao_<ano>.parquet   uf, mun, zona, secao, local, tipo_agregada, secao_principal, eleitores
#   locais_<ano>.parquet             uf, mun, zona, local, nm_local, cep, lat, lon, tipo_local, eleitores
#   municipios_<ano>.parquet         uf, mun, nm_mun
# Rodar da raiz do projeto: Rscript R/00_extrair.R

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(data.table)
})

dir_raw <- "dados_tse"
dir_out <- "dados/base"
dir.create(file.path(dir_out, "bweb_2022"), recursive = TRUE, showWarnings = FALSE)

# ---- leitura ---------------------------------------------------------------

# descompacta `membro` em tmp e abre como dataset arrow, todas as colunas como texto
abrir_csv <- function(zip, membro, tmp) {
  zip::unzip(zip, files = membro, exdir = tmp)
  f <- file.path(tmp, membro)
  cab <- strsplit(gsub('"', "", readLines(f, n = 1, warn = FALSE)), ";")[[1]]
  open_dataset(f, format = "csv", delim = ";",
               schema = schema(setNames(lapply(cab, \(x) utf8()), cab)),
               read_options = csv_read_options(encoding = "latin1", skip_rows = 1, column_names = cab))
}

# roda `f(dataset)` sobre um CSV de dentro do zip e apaga o temporário
com_csv <- function(zip, membro, f) {
  tmp <- tempfile("tse_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  as.data.table(f(abrir_csv(file.path(dir_raw, zip), membro, tmp)))
}

# Obs.: open_dataset ignora `encoding`; só serve para colunas sem acento (números, datas, códigos).
# Para colunas com texto (nomes de locais/municípios) use com_fread (arquivos até ~400 MB).
com_fread <- function(zip, membro, cols) {
  tmp <- tempfile("tse_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  zip::unzip(file.path(dir_raw, zip), files = membro, exdir = tmp)
  d <- fread(file.path(tmp, membro), sep = ";", encoding = "Latin-1", select = cols,
             colClasses = "character")
  for (j in names(d)) set(d, j = j, value = enc2utf8(d[[j]]))
  d
}

membro <- function(zip, padrao) {
  m <- grep(padrao, zip::zip_list(file.path(dir_raw, zip))$filename, value = TRUE)
  stopifnot(length(m) == 1)
  m
}

num <- function(x) as.numeric(sub(",", ".", x, fixed = TRUE))   # 2026 usa vírgula decimal
dt_br <- function(x) as.POSIXct(x, format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")

gravar <- function(d, nome) {
  write_parquet(d, file.path(dir_out, nome), compression = "zstd")
  message(sprintf("  %s: %s linhas, %.1f MB", nome, format(nrow(d), big.mark = ".", decimal.mark = ","),
                  file.size(file.path(dir_out, nome)) / 1e6))
}

feito <- function(nome) file.exists(file.path(dir_out, nome))

# ---- votos por seção (Presidente), 2018 e 2022 -------------------------------

for (ano in c(2018, 2022)) {
  saida <- sprintf("votos_secao_%d.parquet", ano)
  if (feito(saida)) next
  message("votacao_secao ", ano)
  zip <- sprintf("votacao_secao_%d_BR.zip", ano)
  d <- com_csv(zip, membro(zip, "\\.csv$"), \(ds) ds |>
    filter(CD_CARGO == "1") |>
    transmute(turno = as.integer(NR_TURNO), uf = SG_UF, mun = as.integer(CD_MUNICIPIO),
              zona = as.integer(NR_ZONA), secao = as.integer(NR_SECAO),
              nr_votavel = as.integer(NR_VOTAVEL), votos = as.integer(QT_VOTOS)) |>
    collect())
  gravar(d, saida)
}

# ---- seções 2022: boletins de urna (aptos, comparecimento, horário de recebimento) ----
# Um parquet por arquivo em dados/base/bweb_2022/, para poder retomar e acrescentar UFs depois.

zips_bweb <- list.files(dir_raw, "^bweb_[12]t_[A-Z]{2}_\\d+\\.zip$")
for (zip in zips_bweb) {
  saida <- file.path("bweb_2022", sub("\\.zip$", ".parquet", zip))
  if (feito(saida)) next
  message(zip)
  d <- com_csv(zip, membro(zip, "\\.csv$"), \(ds) ds |>
    filter(CD_CARGO_PERGUNTA == "1") |>
    distinct(NR_TURNO, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, DT_BU_RECEBIDO,
             QT_APTOS, QT_COMPARECIMENTO, QT_ABSTENCOES, CD_TIPO_URNA, DS_AGREGADAS) |>
    collect())
  d <- d[, .(turno = as.integer(NR_TURNO), uf = SG_UF, mun = as.integer(CD_MUNICIPIO),
             zona = as.integer(NR_ZONA), secao = as.integer(NR_SECAO),
             local = as.integer(NR_LOCAL_VOTACAO), aptos = as.integer(QT_APTOS),
             comparecimento = as.integer(QT_COMPARECIMENTO), abstencoes = as.integer(QT_ABSTENCOES),
             dt_recebido = dt_br(DT_BU_RECEBIDO), tipo_urna = as.integer(CD_TIPO_URNA),
             agregadas = fifelse(DS_AGREGADAS == "#NULO#", NA_character_, DS_AGREGADAS))]
  gravar(d, saida)
}

ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR",
         "RJ","RN","RO","RR","RS","SC","SE","SP","TO","ZZ")
esperados <- as.vector(outer(c("1t", "2t"), ufs, paste, sep = "_"))
tem <- sub("^bweb_(\\dt_[A-Z]{2})_.*", "\\1", list.files(file.path(dir_out, "bweb_2022")))
if (length(falta <- setdiff(esperados, tem)))
  warning("Boletins de urna 2022 faltando: ", paste(falta, collapse = ", "), call. = FALSE)

d <- rbindlist(lapply(list.files(file.path(dir_out, "bweb_2022"), full.names = TRUE), read_parquet))
gravar(d, "secoes_2022.parquet")

# ---- seções 2018: detalhe por seção (tem horário de recebimento do BU) --------

if (!feito("secoes_2018.parquet")) {
  message("detalhe_votacao_secao 2018")
  zip <- "detalhe_votacao_secao_2018.zip"
  d <- com_csv(zip, membro(zip, "_BR\\.csv$"), \(ds) ds |>
    filter(CD_CARGO == "1") |>
    select(NR_TURNO, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, QT_APTOS,
           QT_COMPARECIMENTO, QT_ABSTENCOES, DT_RECEBIMENTO_BU_HOR_TSE, DT_PRIM_TOT_PARCIAL_HOR_TSE) |>
    collect())
  d <- d[, .(turno = as.integer(NR_TURNO), uf = SG_UF, mun = as.integer(CD_MUNICIPIO),
             zona = as.integer(NR_ZONA), secao = as.integer(NR_SECAO),
             local = as.integer(NR_LOCAL_VOTACAO), aptos = as.integer(QT_APTOS),
             comparecimento = as.integer(QT_COMPARECIMENTO), abstencoes = as.integer(QT_ABSTENCOES),
             dt_recebido = dt_br(DT_RECEBIMENTO_BU_HOR_TSE),
             dt_totalizado = dt_br(DT_PRIM_TOT_PARCIAL_HOR_TSE))]
  gravar(d, "secoes_2018.parquet")
}

# ---- eleitorado por seção e local de votação, 2018 / 2022 / 2024 / 2026 --------------

for (ano in c(2018, 2022, 2024, 2026)) {
  if (feito(sprintf("locais_%d.parquet", ano))) next
  message("eleitorado_local_votacao ", ano)
  zip <- sprintf("eleitorado_local_votacao_%d.zip", ano)
  arqs <- grep("\\.csv$", zip::zip_list(file.path(dir_raw, zip))$filename, value = TRUE)
  m <- if (length(arqs) == 1) arqs else grep("_BRASIL\\.csv$", arqs, value = TRUE)  # 2026: por UF + BRASIL
  d <- com_fread(zip, m, c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA", "NR_SECAO",
                           "CD_TIPO_SECAO_AGREGADA", "NR_SECAO_PRINCIPAL", "NR_LOCAL_VOTACAO",
                           "NM_LOCAL_VOTACAO", "CD_TIPO_LOCAL", "NR_CEP", "NR_LATITUDE",
                           "NR_LONGITUDE", "QT_ELEITOR_SECAO", "NR_LOCAL_VOTACAO_ORIGINAL"))[NR_TURNO == "1"]
  d[, `:=`(mun = as.integer(CD_MUNICIPIO), zona = as.integer(NR_ZONA), secao = as.integer(NR_SECAO),
           local = as.integer(NR_LOCAL_VOTACAO), local_original = as.integer(NR_LOCAL_VOTACAO_ORIGINAL),
           eleitores = as.integer(QT_ELEITOR_SECAO), cep = suppressWarnings(as.integer(NR_CEP)))]
  # coordenadas: só dentro do retângulo do Brasil; exterior (ZZ) sem coordenadas
  lat <- suppressWarnings(num(d$NR_LATITUDE)); lon <- suppressWarnings(num(d$NR_LONGITUDE))
  ok  <- !is.na(lat) & !is.na(lon) & lat > -34 & lat < 6 & lon > -75 & lon < -28 & d$SG_UF != "ZZ"
  d[, `:=`(lat = fifelse(ok, lat, NA_real_), lon = fifelse(ok, lon, NA_real_))]

  # seção: local atual e local original (= número no dia da eleição em arquivos gerados depois dela)
  gravar(d[, .(uf = SG_UF, mun, zona, secao, local, local_original,
               tipo_agregada = as.integer(CD_TIPO_SECAO_AGREGADA),
               secao_principal = as.integer(NR_SECAO_PRINCIPAL), eleitores, cep, lat, lon)],
         sprintf("eleitorado_secao_%d.parquet", ano))

  # local: nome/CEP/coordenadas da 1ª seção (são iguais dentro do local), eleitorado somado
  gravar(d[, .(nm_local = NM_LOCAL_VOTACAO[1], tipo_local = as.integer(CD_TIPO_LOCAL[1]),
               cep = cep[1], lat = lat[1], lon = lon[1], eleitores = sum(eleitores)),
           by = .(uf = SG_UF, mun, zona, local)],
         sprintf("locais_%d.parquet", ano))
  gravar(unique(d[, .(uf = SG_UF, mun, nm_mun = NM_MUNICIPIO)]), sprintf("municipios_%d.parquet", ano))
}
