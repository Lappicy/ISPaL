app_dir <- system.file("app", package = "ISPaL")
if (!nzchar(app_dir) || !dir.exists(app_dir)) {
  stop("Não foi possível localizar os arquivos do painel ISPaL.", call. = FALSE)
}

ISPaL:::build_ispal_app(app_dir)
