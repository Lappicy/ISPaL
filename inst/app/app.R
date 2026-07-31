app_dir <- system.file("app", package = "ISPaLShiny")
if (!nzchar(app_dir) || !dir.exists(app_dir)) {
  stop("Não foi possível localizar os arquivos do painel ISPaL.", call. = FALSE)
}

ISPaLShiny:::build_ispal_app(app_dir)
