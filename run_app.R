ispal_shiny_dir <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(source_file) && nzchar(source_file)) {
    normalizePath(dirname(source_file), mustWork = TRUE)
  } else {
    normalizePath(getwd(), mustWork = TRUE)
  }
})

runISPaL <- function(launch.browser = interactive(), ...) {
  app_dir <- ispal_shiny_dir
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop(
      "Não foi possível localizar app.R. Baixe o repositório ISPaL completo ",
        "e execute run_app.R sem separar seus arquivos.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir = app_dir, launch.browser = launch.browser, ...)
}
