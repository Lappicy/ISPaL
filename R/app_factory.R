ispal_required_packages <- function() {
  c(
    "shiny", "bslib", "shinyWidgets", "DT", "readxl", "readr",
    "openxlsx", "ggplot2", "ggridges", "patchwork", "shinycssloaders",
    "dplyr", "lubridate", "MASS"
  )
}

check_ispal_dependencies <- function() {
  missing_packages <- ispal_required_packages()[
    !vapply(ispal_required_packages(), requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages)) {
    stop(
      "Instale os pacotes necessários antes de iniciar o aplicativo: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
}

ispal_styles_dir <- function(app_dir) {
  local_styles <- file.path(app_dir, "www")
  if (dir.exists(local_styles)) return(local_styles)

  installed_styles <- system.file("www", package = "ISPaL")
  if (nzchar(installed_styles) && dir.exists(installed_styles)) return(installed_styles)

  stop("Não foi possível localizar o arquivo de estilos da aplicação.", call. = FALSE)
}

build_ispal_app <- function(app_dir = ispal_app_dir()) {
  check_ispal_dependencies()

  app_dir <- normalizePath(app_dir, mustWork = TRUE)
  options(ispal.app.dir = app_dir)
  options(sass.cache = file.path(tempdir(), "ispal-sass-cache"))
  shiny::addResourcePath("ispal-assets", ispal_styles_dir(app_dir))

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
        shiny::tags$link(
          rel = "stylesheet", type = "text/css",
          href = "ispal-assets/styles.css"
        ),
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
}
