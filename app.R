app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!dir.exists(file.path(app_dir, "R"))) {
  stop("Inicie o aplicativo a partir da raiz do pacote ISPaL ou use run_app.R.")
}

source_order <- c(
  "utils.R",
  "io_utils.R",
  "data_validation.R",
  "forecasting_backend.R",
  "export_functions.R",
  "plotting_functions.R",
  "mod_data_input.R",
  "mod_configuration.R",
  "mod_results.R",
  "mod_plots.R",
  "mod_about.R",
  "app_factory.R"
)
invisible(lapply(file.path(app_dir, "R", source_order), source, local = FALSE))

build_ispal_app(app_dir)
