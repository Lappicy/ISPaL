app_dir <- normalizePath(file.path("..", ".."), mustWork = TRUE)
options(ispal.app.dir = app_dir)

source_order <- c(
  "utils.R", "io_utils.R", "data_validation.R", "forecasting_backend.R",
  "export_functions.R", "plotting_functions.R", "mod_data_input.R"
)
invisible(lapply(file.path(app_dir, "R", source_order), source, local = FALSE))

example_data_for_tests <- load_example_data()
