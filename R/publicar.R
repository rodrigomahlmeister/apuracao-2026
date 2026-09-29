# Publica a pasta site/ no Cloudflare (Workers com arquivos estáticos; configuração em wrangler.jsonc).
# Só os arquivos alterados são enviados (normalmente só site/dados/apuracao.json). ~20 s.
# Uso direto: Rscript R/publicar.R      | a partir do R: publicar_site(esperar = FALSE)
publicar_site <- function(esperar = TRUE, log = "saida/publicar.log") {
  node <- file.path(Sys.getenv("USERPROFILE"), "scoop", "apps", "nodejs-lts", "current")
  if (dir.exists(node)) Sys.setenv(PATH = paste(node, Sys.getenv("PATH"), sep = .Platform$path.sep))
  dir.create(dirname(log), showWarnings = FALSE, recursive = TRUE)
  system2("npx", c("--yes", "wrangler@4", "deploy"), stdout = log, stderr = log, wait = esperar)
}

if (sys.nframe() == 0) {
  st <- publicar_site(esperar = TRUE)
  cat(tail(readLines("saida/publicar.log", warn = FALSE), 4), sep = "\n")
  quit(status = st)
}
