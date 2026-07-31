local_app_dir <- normalizePath(file.path("..", ".."), mustWork = FALSE)
use_local_source <- file.exists(file.path(local_app_dir, "DESCRIPTION")) &&
  dir.exists(file.path(local_app_dir, "R")) &&
  dir.exists(file.path(local_app_dir, "inst", "extdata"))

if (use_local_source) {
  app_dir <- normalizePath(local_app_dir, mustWork = TRUE)
  options(ispal.app.dir = app_dir)

  source_order <- c(
    "utils.R", "io_utils.R", "data_validation.R", "forecasting_backend.R",
    "export_functions.R", "plotting_functions.R", "mod_data_input.R"
  )
  invisible(lapply(file.path(app_dir, "R", source_order), source, local = FALSE))
} else {
  package_dir <- system.file(package = "ISPaL")
  if (!nzchar(package_dir)) stop("O pacote ISPaL não foi encontrado para os testes.")

  options(ispal.app.dir = system.file("app", package = "ISPaL"))
  package_namespace <- asNamespace("ISPaL")
  package_objects <- ls(package_namespace, all.names = FALSE)
  invisible(lapply(package_objects, function(name) {
    assign(name, get(name, envir = package_namespace), envir = .GlobalEnv)
  }))
}

example_data_for_tests <- load_example_data()
