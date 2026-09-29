# Gera site/dados/apuracao.json a partir do replay de 2022 (método principal), para desenvolver a página.
# Uso: Rscript estudos/demo_site_json.R [turno = 1] [ate_pct = 100]   (ate_pct corta a apuração nesse ponto)
suppressPackageStartupMessages({ library(arrow); library(data.table); library(jsonlite) })
a <- commandArgs(TRUE)
tn <- if (length(a) >= 1) as.integer(a[1]) else 1L
ate <- if (length(a) >= 2) as.numeric(a[2]) else 100
s <- as.data.table(read_parquet(sprintf("dados/replay/replay_metodos_recon_2022_t%d.parquet", tn)))[metodo == "P_ua_prop"]
s <- s[pestn <= ate][order(hora)]
w <- dcast(s, hora + pestn ~ bloco, value.var = c("apurado", "proj", "lo", "hi", "p_2turno"))
w[pestn < 2, grep("^(proj|lo|hi|p_2turno)_", names(w), value = TRUE) := NA]     # projeção só a partir de 2%
r <- function(x) round(100 * x, 2)
pontos <- lapply(seq_len(nrow(w)), \(i) with(w[i], list(
  t = format(hora, "%H:%M", tz = "America/Sao_Paulo"), x = round(pestn, 2),
  apurado = list(PT = r(apurado_PT), PL = r(apurado_PL), OUTROS = r(apurado_OUTROS)),
  projecao = if (!is.na(proj_PT)) list(PT = r(proj_PT), PL = r(proj_PL), OUTROS = r(proj_OUTROS)),
  faixa = if (!is.na(lo_PT)) list(PT = c(r(lo_PT), r(hi_PT)), PL = c(r(lo_PL), r(hi_PL)), OUTROS = c(r(lo_OUTROS), r(hi_OUTROS))),
  p_2turno = if (!is.na(p_2turno_PT)) round(p_2turno_PT, 3))))
u <- w[.N]
out <- list(eleicao = "Presidente 2022 (replay)", turno = tn, ambiente = "replay", atualizado = format(u$hora, "%d/%m/%Y %H:%M", tz = "America/Sao_Paulo"),
            pct_eleitorado_apurado = round(u$pestn, 2), preliminar = u$pestn < 5,
            candidatos = list(list(bloco = "PT", nome = "Lula", partido = "PT"), list(bloco = "PL", nome = "Bolsonaro", partido = "PL"),
                              list(bloco = "OUTROS", nome = "Outros", partido = "")),
            pontos = pontos)
dir.create("site/dados", recursive = TRUE, showWarnings = FALSE)
write_json(out, "site/dados/apuracao.json", auto_unbox = TRUE, null = "null", digits = NA)
message(sprintf("site/dados/apuracao.json: %d pontos, até %.1f%% apurado", length(pontos), u$pestn))
