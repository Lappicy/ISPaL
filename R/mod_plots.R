mod_plots_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tagList(
      bslib::card(
        bslib::card_header("Fonte dos resultados"),
        bslib::layout_columns(
          col_widths = c(7, 5),
          shiny::div(
            class = "source-mode-control",
            shiny::radioButtons(
              ns("source_mode"),
              NULL,
              choices = c(
                "Resultado calculado no aplicativo" = "calculated",
                "Carregar tabela pronta" = "uploaded"
              ),
              selected = "calculated",
              inline = TRUE
            )
          ),
          shiny::div(
            class = "source-table-control",
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] === 'calculated'", ns("source_mode")),
              shiny::selectInput(
                ns("calculated_table"),
                "Tabela usada",
                choices = c("Errors" = "Error.table", "All Errors" = "All.error.table"),
                selected = "Error.table"
              )
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] === 'uploaded'", ns("source_mode")),
              shiny::fileInput(
                ns("result_file"),
                "Tabela de resultados",
                accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")
              ),
              shiny::uiOutput(ns("result_sheet_ui")),
              bslib::accordion(
                open = FALSE,
                bslib::accordion_panel(
                  "Opções de leitura",
                  shiny::selectInput(
                    ns("result_delimiter"), "Separador",
                    choices = c(
                      "Detectar" = "auto", ";" = ";", "," = ",",
                      "Tabulação" = "\\t", "|" = "|"
                    )
                  ),
                  shiny::selectInput(
                    ns("result_decimal"), "Separador decimal",
                    choices = c("Ponto" = ".", "Vírgula" = ",")
                  ),
                  shiny::checkboxInput(ns("result_header"), "Possui cabeçalho", TRUE),
                  shiny::selectInput(
                    ns("result_encoding"), "Codificação",
                    choices = c("UTF-8", "Latin1", "Windows-1252")
                  )
                )
              ),
              shiny::uiOutput(ns("mapping_ui"))
            )
          )
        )
      ),
      bslib::card(
        bslib::card_header("Gráfico e filtros"),
        shiny::uiOutput(ns("plot_controls"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(9, 3),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Visualização"),
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("main_plot"), height = "850px"),
          type = 4,
          color = "#2D6A73"
        )
      ),
      bslib::card(
        bslib::card_header("Exportar figura"),
        shiny::numericInput(ns("width_mm"), "Largura (mm)", 250, min = 80, max = 1000),
        shiny::numericInput(ns("height_mm"), "Altura (mm)", 300, min = 80, max = 1500),
        shiny::numericInput(ns("dpi"), "Resolução (dpi)", 300, min = 72, max = 1200),
        shiny::downloadButton(
          ns("download_png"), "Baixar PNG",
          class = "btn-primary w-100 mb-2"
        ),
        shiny::downloadButton(
          ns("download_pdf"), "Baixar PDF",
          class = "btn-outline-primary w-100"
        ),
        shiny::hr(),
        shiny::p(
          class = "form-text",
          "Os gráficos são reconstruídos a partir das tabelas; nenhuma imagem pronta é utilizada."
        )
      )
    )
  )
}

mod_plots_server <- function(id, results, data_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$result_sheet_ui <- shiny::renderUI({
      file <- input$result_file
      if (is.null(file) || !is_excel_file(file$name)) return(NULL)
      sheets <- excel_sheets_safe(file$datapath)
      shiny::selectInput(ns("result_sheet"), "Planilha", choices = sheets)
    })

    uploaded_raw <- shiny::reactive({
      file <- input$result_file
      if (is.null(file)) return(NULL)
      read_tabular_file(
        file$datapath, file$name,
        sheet = input$result_sheet,
        delimiter = input$result_delimiter %||% "auto",
        decimal_mark = input$result_decimal %||% ".",
        header = input$result_header %||% TRUE,
        encoding = input$result_encoding %||% "UTF-8"
      )
    })

    output$mapping_ui <- shiny::renderUI({
      data <- uploaded_raw()
      if (is.null(data)) return(NULL)
      choices <- c("— não usar —" = "", names(data))
      find_choice <- function(candidates) {
        intersect(candidates, names(data))[1] %||% ""
      }
      shiny::tagList(
        shiny::h6("Identificação das colunas"),
        shiny::selectInput(
          ns("map_model"), "Modelo",
          choices = names(data),
          selected = find_choice(c("Model", "Modelo"))
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectInput(
            ns("map_series"), "Série / variável",
            choices = choices,
            selected = find_choice(c("Series", "Serie", "PostoUHE", "K"))
          ),
          shiny::selectInput(
            ns("map_lag"), "Lead",
            choices = choices,
            selected = find_choice(c("Lag", "Lead"))
          ),
          shiny::selectInput(
            ns("map_month"), "Mês",
            choices = choices,
            selected = find_choice(c("Month", "Mes"))
          )
        )
      )
    })

    plot_data <- shiny::reactive({
      if (identical(input$source_mode, "uploaded")) {
        data <- uploaded_raw()
        if (is.null(data)) return(NULL)
        mapping <- compact_list(list(
          Model = input$map_model,
          Series = if (nzchar(input$map_series %||% "")) input$map_series else NULL,
          Lag = if (nzchar(input$map_lag %||% "")) input$map_lag else NULL,
          Month = if (nzchar(input$map_month %||% "")) input$map_month else NULL
        ))
        return(normalize_result_columns(data, mapping))
      }
      result <- results()
      if (is.null(result)) return(NULL)
      normalize_result_columns(result[[input$calculated_table]])
    })

    output$plot_controls <- shiny::renderUI({
      data <- plot_data()
      state <- data_state()
      diagnostic_available <- isTRUE(state$ready) && !is.null(state$var_x)
      plot_choices <- c(
        "Distribuições + boxplots" = "distribution",
        "Modelo vencedor" = "winner"
      )
      if (diagnostic_available) {
        plot_choices <- c(
          plot_choices,
          "Correlação entre X" = "correlation",
          "CCF entre Y e X" = "ccf"
        )
      }
      if (is.null(data) && !diagnostic_available) {
        return(shiny::div(class = "empty-state", "Execute os modelos ou carregue uma tabela."))
      }
      metrics <- available_metrics(data)
      models <- if (!is.null(data) && "Model" %in% names(data)) unique(data$Model) else character()
      series <- if (!is.null(data) && "Series" %in% names(data)) unique(data$Series) else character()
      lags <- if (!is.null(data) && "Lag" %in% names(data)) sort(unique(data$Lag)) else numeric()
      months <- if (!is.null(data) && "Month" %in% names(data)) sort(unique(data$Month)) else numeric()

      shiny::tagList(
        shiny::selectInput(ns("plot_type"), "Tipo de gráfico", choices = plot_choices),
        shiny::conditionalPanel(
          condition = sprintf(
            "['distribution','winner'].includes(input['%s'])",
            ns("plot_type")
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::div(
              class = "plot-filter-block",
              shiny::selectizeInput(
                ns("metrics"), "Métricas — arraste para ordenar",
                choices = stats::setNames(metrics, unname(metric_labels[metrics])),
                selected = utils::head(metrics, 6),
                multiple = TRUE,
                options = list(
                  plugins = list("drag_drop", "remove_button"),
                  persist = TRUE
                )
              )
            ),
            shiny::div(
              class = "plot-filter-block",
              shiny::selectizeInput(
                ns("models"), "Modelos",
                choices = models,
                selected = {
                  main_models <- intersect(c("PAR", "PARX", "RIDGE"), models)
                  if (length(main_models)) main_models else models
                },
                multiple = TRUE
              )
            ),
            shiny::div(
              class = "plot-filter-block",
              if (length(series)) shiny::selectizeInput(
                ns("series"), "Variáveis Y",
                choices = series, selected = series, multiple = TRUE
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            if (length(lags)) shiny::selectizeInput(
              ns("plot_lags"), "Leads",
              choices = lags, selected = lags, multiple = TRUE
            ),
            if (length(months)) shiny::selectizeInput(
              ns("plot_months"), "Meses",
              choices = stats::setNames(months, month_labels_pt[as.character(months)]),
              selected = months, multiple = TRUE
            )
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'distribution'", ns("plot_type")),
          shiny::sliderInput(ns("plot_columns"), "Colunas de painéis", min = 1, max = 3, value = 2)
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'winner'", ns("plot_type")),
          shiny::radioButtons(
            ns("aggregate_by"), "Agregar por",
            choices = c("Mês" = "Month", "Lead" = "Lag"),
            inline = TRUE
          )
        ),
        if (diagnostic_available) shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'correlation'", ns("plot_type")),
          shiny::selectizeInput(
            ns("cor_x"), "Covariáveis",
            choices = setdiff(names(state$var_x), "Date"),
            selected = setdiff(names(state$var_x), "Date"),
            multiple = TRUE
          )
        ),
        if (diagnostic_available) shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'ccf'", ns("plot_type")),
          shiny::selectizeInput(
            ns("ccf_y"), "Variáveis Y",
            choices = setdiff(names(state$var_y), "Date"),
            selected = setdiff(names(state$var_y), "Date"),
            multiple = TRUE
          ),
          shiny::selectizeInput(
            ns("ccf_x"), "Covariáveis X",
            choices = setdiff(names(state$var_x), "Date"),
            selected = utils::head(setdiff(names(state$var_x), "Date"), 3),
            multiple = TRUE
          ),
          shiny::sliderInput(ns("ccf_lag"), "Lag máximo", min = 1, max = 24, value = 12)
        )
      )
    })

    filtered_plot_data <- shiny::reactive({
      data <- plot_data()
      if (is.null(data)) return(NULL)
      if (length(input$models) && "Model" %in% names(data)) {
        data <- data[data$Model %in% input$models, , drop = FALSE]
      }
      if (length(input$series) && "Series" %in% names(data)) {
        data <- data[data$Series %in% input$series, , drop = FALSE]
      }
      if (length(input$plot_lags) && "Lag" %in% names(data)) {
        data <- data[data$Lag %in% as.numeric(input$plot_lags), , drop = FALSE]
      }
      if (length(input$plot_months) && "Month" %in% names(data)) {
        data <- data[data$Month %in% as.numeric(input$plot_months), , drop = FALSE]
      }
      data
    })

    result_x_names <- shiny::reactive({
      if (identical(input$source_mode, "calculated")) {
        result <- results()
        if (!is.null(result) && length(result$metadata$x_names)) {
          return(result$metadata$x_names)
        }
        state <- data_state()
        return(if (is.null(state$var_x)) character() else setdiff(names(state$var_x), "Date"))
      }
      data <- plot_data()
      if (is.null(data)) return(character())
      name_columns <- grep("^X[0-9]+_Name$", names(data), value = TRUE)
      values <- if (length(name_columns)) {
        unique(unlist(data[name_columns], use.names = FALSE))
      } else {
        character()
      }
      values <- values[!is.na(values) & nzchar(values)]
      if (!length(values) && "SelectedX" %in% names(data)) {
        values <- trimws(unlist(strsplit(
          paste(data$SelectedX[!is.na(data$SelectedX)], collapse = ";"),
          ";",
          fixed = TRUE
        )))
        values <- unique(values[nzchar(values)])
      }
      values
    })

    metric_variable_context <- shiny::reactive({
      data <- filtered_plot_data()
      y_names <- if (!is.null(data) && "Series" %in% names(data)) {
        unique(as.character(data$Series))
      } else {
        character()
      }
      format_variable_context(
        y_names,
        result_x_names(),
        y_fallback = "não identificada",
        x_fallback = if (identical(input$source_mode, "uploaded")) {
          "não identificada"
        } else {
          "não utilizada"
        }
      )
    })

    plot_object <- shiny::reactive({
      type <- input$plot_type %||% "distribution"
      state <- data_state()
      if (type == "correlation") {
        return(build_correlation_plot(
          state$var_x,
          input$cor_x,
          y_names = character()
        ))
      }
      if (type == "ccf") {
        return(build_ccf_plot(
          state$var_y, state$var_x,
          input$ccf_y, input$ccf_x,
          lag_max = input$ccf_lag %||% 12
        ))
      }
      data <- filtered_plot_data()
      shiny::req(data, nrow(data) > 0, length(input$metrics) > 0)
      if (type == "winner") {
        build_winner_plot(
          data,
          input$metrics,
          input$aggregate_by %||% "Month",
          variable_context = metric_variable_context()
        )
      } else {
        build_metric_distribution_plot(
          data,
          input$metrics,
          columns = input$plot_columns %||% 2,
          variable_context = metric_variable_context()
        )
      }
    })

    output$main_plot <- shiny::renderPlot({
      plot_object()
    }, res = 110)

    output$download_png <- shiny::downloadHandler(
      filename = function() paste0("ISPaL_grafico_", Sys.Date(), ".png"),
      content = function(file) {
        save_metric_plot(
          plot_object(), file,
          input$width_mm, input$height_mm, input$dpi, "png"
        )
      }
    )
    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0("ISPaL_grafico_", Sys.Date(), ".pdf"),
      content = function(file) {
        save_metric_plot(
          plot_object(), file,
          input$width_mm, input$height_mm, input$dpi, "pdf"
        )
      }
    )

    list(data = plot_data, plot = plot_object)
  })
}
