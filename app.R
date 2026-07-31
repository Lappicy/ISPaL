required_packages <- c(
  "shiny", "bslib", "shinyWidgets", "DT", "readxl", "readr",
  "openxlsx", "ggplot2", "ggridges", "patchwork", "shinycssloaders",
  "dplyr", "lubridate", "MASS", "scales"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Instale os pacotes necessários antes de iniciar o aplicativo: ",
    paste(missing_packages, collapse = ", ")
  )
}

app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!dir.exists(file.path(app_dir, "R"))) {
  stop("Inicie o aplicativo a partir da pasta ISPaL_Shiny ou use run_app.R.")
}
options(ispal.app.dir = app_dir)
options(sass.cache = file.path(tempdir(), "ispal-sass-cache"))

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
  "mod_about.R"
)
invisible(lapply(file.path(app_dir, "R", source_order), source, local = FALSE))

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2D6A73",
  secondary = "#667085",
  success = "#2D7D5B",
  info = "#337A9B",
  warning = "#C47A20",
  danger = "#B74242",
  bg = "#F4F7F8",
  fg = "#172235",
  base_font = bslib::font_google("Source Sans 3"),
  heading_font = bslib::font_google("Manrope")
)

ui <- bslib::page_navbar(
  title = shiny::div(
    class = "brand-wrap",
    shiny::strong("ISPaL")
  ),
  id = "main_nav",
  theme = theme,
  fillable = FALSE,
  header = shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      shiny::tags$script(shiny::HTML(
        "Shiny.addCustomMessageHandler('toggle-disabled', function(x) {
           var el = document.getElementById(x.id);
           if (el) {
             el.disabled = !!x.disabled;
             el.classList.toggle('is-running', !!x.disabled);
           }
         });"
      ))
    ),
    shiny::div(
      class = "hero-strip",
      shiny::div(
        class = "container-fluid app-shell",
        shiny::span(class = "hero-kicker", "PAR · PARX · RIDGE"),
        shiny::h1("Previsões periódicas com variáveis exógenas")
      )
    )
  ),
  bslib::nav_panel(
    shiny::span(shiny::icon("database"), "Dados"),
    shiny::div(class = "container-fluid app-shell page-section", mod_data_input_ui("data"))
  ),
  bslib::nav_panel(
    shiny::span(shiny::icon("sliders"), "Configuração"),
    shiny::div(
      class = "container-fluid app-shell page-section",
      mod_configuration_ui("configuration")
    )
  ),
  bslib::nav_panel(
    shiny::span(shiny::icon("table"), "Resultados"),
    shiny::div(class = "container-fluid app-shell page-section", mod_results_ui("results"))
  ),
  bslib::nav_panel(
    shiny::span(shiny::icon("chart-area"), "Gráficos"),
    shiny::div(class = "container-fluid app-shell page-section", mod_plots_ui("plots"))
  ),
  bslib::nav_panel(
    shiny::span(shiny::icon("circle-info"), "Ajuda · Como citar"),
    shiny::div(class = "container-fluid app-shell page-section", mod_about_ui("about"))
  ),
  footer = shiny::div(
    class = "app-footer",
    "ISPaL Shiny - contato lappicy@gmail.com"
  )
)

server <- function(input, output, session) {
  data_module <- mod_data_input_server("data")
  configuration_module <- mod_configuration_server(
    "configuration",
    data_module$data
  )
  mod_results_server("results", configuration_module$results)
  mod_plots_server(
    "plots",
    configuration_module$results,
    data_module$data
  )
}

shiny::shinyApp(ui, server)
