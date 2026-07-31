package_dir <- system.file(package = "ISPaLShiny")

if (nzchar(package_dir)) {
  options(ispal.app.dir = system.file("app", package = "ISPaLShiny"))
  package_namespace <- asNamespace("ISPaLShiny")
  package_objects <- ls(package_namespace, all.names = FALSE)
  invisible(lapply(package_objects, function(name) {
    assign(name, get(name, envir = package_namespace), envir = .GlobalEnv)
  }))
} else {
  app_dir <- normalizePath(file.path("..", ".."), mustWork = TRUE)
  options(ispal.app.dir = app_dir)

  source_order <- c(
    "utils.R", "io_utils.R", "data_validation.R", "forecasting_backend.R",
    "export_functions.R", "plotting_functions.R", "mod_data_input.R"
  )
  invisible(lapply(file.path(app_dir, "R", source_order), source, local = FALSE))
}

example_data_for_tests <- load_example_data()
