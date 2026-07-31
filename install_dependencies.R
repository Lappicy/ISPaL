packages <- c(
  "shiny", "bslib", "shinyWidgets", "DT", "readxl", "readr",
  "openxlsx", "ggplot2", "ggridges", "patchwork", "shinycssloaders",
  "dplyr", "lubridate", "MASS", "testthat", "jsonlite", "rjson",
  "forecast", "pls", "glmnet", "foreign"
)

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("Todas as dependências já estão instaladas.")
}
