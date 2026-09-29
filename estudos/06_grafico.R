# Gráfico da apuração: por bloco, linha cheia = % dos válidos apurado; pontilhada = projeção do resultado
# final com faixa sombreada. Eixo x = andamento (% do eleitorado nas seções totalizadas, por padrão).
suppressPackageStartupMessages({ library(ggplot2); library(ggrepel); library(yaml); library(data.table) })

# serie: data.table com x (0-100), bloco (PT/PL/OUTROS), apurado, proj, lo, hi (proporções 0-1)
# final: data.table(bloco, real) para o replay (linha fina com o resultado real); NULL ao vivo
grafico_apuracao <- function(serie, final = NULL, titulo = "", subtitulo = "", xlab = "% do eleitorado apto nas seções totalizadas",
                             cfg = read_yaml("config/blocos.yaml"), rotulos = NULL) {
  cores <- unlist(cfg$cores); rot <- unlist(cfg$rotulos); if (!is.null(rotulos)) rot[names(rotulos)] <- unlist(rotulos)
  s <- copy(serie)[, bloco := factor(bloco, levels = names(cores))]
  ult <- s[, .SD[which.max(x)], by = bloco]
  lab <- rbind(ult[, .(x, y = apurado, bloco, txt = sprintf("%.1f", 100 * apurado))],
               ult[!is.na(proj), .(x, y = proj, bloco, txt = sprintf("proj. %.1f", 100 * proj))])
  g <- ggplot(s, aes(x = x, colour = bloco, fill = bloco)) +
    geom_hline(yintercept = 50, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_ribbon(aes(ymin = 100 * lo, ymax = 100 * hi), alpha = 0.12, colour = NA, na.rm = TRUE) +
    geom_line(aes(y = 100 * apurado), linewidth = 0.9) +
    geom_line(aes(y = 100 * proj), linetype = "dotted", linewidth = 0.9, na.rm = TRUE)
  if (!is.null(final))
    g <- g + geom_hline(data = copy(final)[, bloco := factor(bloco, levels = names(cores))],
                        aes(yintercept = 100 * real, colour = bloco), linewidth = 0.3, alpha = 0.8)
  g + geom_text_repel(data = lab, aes(x = x, y = 100 * y, label = txt), size = 3, direction = "y",
                      hjust = 0, nudge_x = 2, segment.size = 0.2, show.legend = FALSE) +
    scale_colour_manual(values = cores, labels = rot[names(cores)], name = NULL) +
    scale_fill_manual(values = cores, guide = "none") +
    scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 20)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(x = xlab, y = "% dos votos válidos", title = titulo, subtitle = subtitulo) +
    theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank())
}
