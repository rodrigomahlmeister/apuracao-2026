# Arquivo lido pela página (site/dados/apuracao.json): metadados + um ponto por ciclo com o % do eleitorado
# apurado, o % apurado de cada bloco e, a partir de 2% apurado, a projeção, a faixa de 90% e P(2º turno).
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(yaml) })

# serie: data.table longa com hora (POSIXct), pestn (0-100), bloco, apurado, proj, lo, hi, p_2turno (proporções)
# traj: trajetória projetada do ciclo atual (x, PT, PL, OU em proporção), ou NULL
escrever_pagina <- function(serie, eleicao, turno, ambiente, traj = NULL, arq = "site/dados/apuracao.json",
                            candidatos = read_yaml("config/blocos.yaml")$candidatos, tz = "America/Sao_Paulo") {
  w <- dcast(serie, hora + pestn ~ bloco, value.var = c("apurado", "proj", "lo", "hi", "p_2turno"))[order(hora)]
  r <- function(v) round(100 * v, 2)
  pontos <- lapply(seq_len(nrow(w)), \(i) with(w[i], list(
    t = format(hora, "%H:%M", tz = tz), x = round(pestn, 2),
    apurado = list(PT = r(apurado_PT), PL = r(apurado_PL), OUTROS = r(apurado_OUTROS)),
    projecao = if (!is.na(proj_PT)) list(PT = r(proj_PT), PL = r(proj_PL), OUTROS = r(proj_OUTROS)),
    faixa = if (!is.na(lo_PT)) list(PT = r(c(lo_PT, hi_PT)), PL = r(c(lo_PL, hi_PL)), OUTROS = r(c(lo_OUTROS, hi_OUTROS))),
    p_2turno = if (!is.na(p_2turno_PT)) round(p_2turno_PT, 3))))
  u <- w[.N]
  out <- list(eleicao = eleicao, turno = turno, ambiente = ambiente,
              atualizado = format(u$hora, "%d/%m/%Y %H:%M", tz = tz), pct_eleitorado_apurado = round(u$pestn, 2),
              candidatos = candidatos, pontos = pontos,
              trajetoria = if (!is.null(traj) && nrow(traj)) lapply(seq_len(nrow(traj)), \(i) with(traj[i],
                list(x = round(x, 2), PT = r(PT), PL = r(PL), OUTROS = r(OU)))))
  dir.create(dirname(arq), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(arq, ".tmp")                                     # troca atômica: a página nunca lê arquivo pela metade
  write_json(out, tmp, auto_unbox = TRUE, null = "null", digits = NA)
  file.rename(tmp, arq)
  invisible(arq)
}
