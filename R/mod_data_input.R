file_import_controls <- function(ns, prefix, label) {
  bslib::card(
    class = "h-100",
    bslib::card_header(label),
    shiny::fileInput(
      ns(paste0(prefix, "_file")),
      "Arquivo",
      accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")
    ),
    shiny::uiOutput(ns(paste0(prefix, "_sheet_ui"))),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Opções de leitura",
        shiny::selectInput(
          ns(paste0(prefix, "_delimiter")),
          "Separador",
          choices = c(
            "Detectar automaticamente" = "auto",
            "Ponto e vírgula (;)" = ";",
            "Vírgula (,)" = ",",
            "Tabulação" = "\\t",
            "Barra vertical (|)" = "|"
          )
        ),
        shiny::selectInput(
          ns(paste0(prefix, "_decimal")),
          "Separador decimal",
          choices = c("Ponto (.)" = ".", "Vírgula (,)" = ",")
        ),
        shiny::checkboxInput(
          ns(paste0(prefix, "_header")),
          "O arquivo possui cabeçalho",
          value = TRUE
        ),
        shiny::selectInput(
          ns(paste0(prefix, "_encoding")),
          "Codificação",
          choices = c("UTF-8", "Latin1", "Windows-1252")
        )
      )
    )
  )
}

mod_data_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Como os seus dados estão organizados?"),
      shiny::div(
        class = "data-mode-options",
        shiny::radioButtons(
          ns("mode"),
          NULL,
          choices = c(
            "Uma tabela com Y e X" = "single",
            "Duas tabelas separadas" = "two"
          ),
          selected = "two",
          inline = TRUE
        )
      ),
      shiny::p(
        class = "text-secondary mb-0",
        "Y representa as séries a prever; X representa as covariáveis auxiliares."
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'single'", ns("mode")),
      bslib::layout_columns(
        col_widths = 12,
        file_import_controls(ns, "single", "Tabela única")
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'two'", ns("mode")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        file_import_controls(ns, "y", "Tabela das variáveis Y"),
        file_import_controls(ns, "x", "Tabela das covariáveis X")
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Prévia dos dados"),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "Tabela Y / única",
            DT::DTOutput(ns("preview_y"))
          ),
          bslib::nav_panel(
            "Tabela X",
            DT::DTOutput(ns("preview_x"))
          )
        )
      ),
      bslib::card(
        bslib::card_header("Diagnóstico"),
        shiny::uiOutput(ns("diagnostic_ui"))
      )
    )
  )
}

mod_data_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_valid_choice <- function(value, choices) {
      length(value) == 1L && !is.na(value) && value %in% choices
    }

    file_sheet_ui <- function(prefix) {
      shiny::renderUI({
        file <- input[[paste0(prefix, "_file")]]
        if (is.null(file) || !is_excel_file(file$name)) return(NULL)
        sheets <- excel_sheets_safe(file$datapath)
        shiny::selectInput(
          ns(paste0(prefix, "_sheet")),
          "Planilha",
          choices = sheets,
          selected = sheets[1]
        )
      })
    }
    output$single_sheet_ui <- file_sheet_ui("single")
    output$y_sheet_ui <- file_sheet_ui("y")
    output$x_sheet_ui <- file_sheet_ui("x")

    read_uploaded <- function(prefix) {
      shiny::reactive({
        file <- input[[paste0(prefix, "_file")]]
        if (is.null(file)) return(NULL)
        read_tabular_file(
          path = file$datapath,
          original_name = file$name,
          sheet = input[[paste0(prefix, "_sheet")]],
          delimiter = input[[paste0(prefix, "_delimiter")]] %||% "auto",
          decimal_mark = input[[paste0(prefix, "_decimal")]] %||% ".",
          header = input[[paste0(prefix, "_header")]] %||% TRUE,
          encoding = input[[paste0(prefix, "_encoding")]] %||% "UTF-8"
        )
      })
    }
    uploaded_single <- read_uploaded("single")
    uploaded_y <- read_uploaded("y")
    uploaded_x <- read_uploaded("x")

    raw_single <- shiny::reactive({
      uploaded_single()
    })
    raw_y <- shiny::reactive({
      uploaded_y()
    })
    raw_x <- shiny::reactive({
      uploaded_x()
    })

    date_format_input <- function(id, label = "Formato da data") {
      shiny::selectInput(
        ns(id),
        label,
        choices = c(
          "Detectar automaticamente" = "auto",
          "YYYY-MM-DD",
          "DD/MM/YYYY",
          "MM/DD/YYYY",
          "YYYY-MM",
          "DD-MM-YYYY",
          "YYYY/MM/DD"
        ),
        selected = input[[id]] %||% "auto"
      )
    }

    mapping_controls <- function() {
      if (identical(input$mode, "single")) {
        data <- raw_single()
        if (is.null(data)) {
          return(shiny::div(class = "empty-state", "Importe uma tabela primeiro."))
        }
        automatic <- auto_single_mapping(data)
        choices <- names(data)
        shiny::tagList(
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::selectInput(
              ns("single_date"), "Coluna de data",
              choices = choices,
              selected = if (is_valid_choice(input$single_date, choices)) {
                input$single_date
              } else {
                automatic$date
              }
            ),
            shiny::selectizeInput(
              ns("single_y"), "Variáveis Y",
              choices = choices,
              multiple = TRUE,
              selected = {
                current <- intersect(input$single_y %||% character(), choices)
                if (length(current)) current else automatic$y
              }
            ),
            shiny::selectizeInput(
              ns("single_x"), "Covariáveis X (máximo 10)",
              choices = choices,
              multiple = TRUE,
              selected = {
                current <- intersect(input$single_x %||% character(), choices)
                if (length(current)) current else automatic$x
              },
              options = list(maxItems = 10)
            )
          ),
          date_format_input("single_date_format")
        )
      } else {
        y <- raw_y()
        x <- raw_x()
        if (is.null(y) && is.null(x)) {
          return(shiny::div(class = "empty-state", "Importe as tabelas primeiro."))
        }
        automatic <- auto_two_table_mapping(y, x)
        shiny::tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              class = "mapping-block",
              shiny::h5("Tabela Y"),
              shiny::selectInput(
                ns("y_date"), "Coluna de data",
                choices = names(y %||% data.frame()),
                selected = if (is_valid_choice(input$y_date, names(y %||% data.frame()))) {
                  input$y_date
                } else {
                  automatic$y_date
                }
              ),
              shiny::selectizeInput(
                ns("y_vars"), "Variáveis Y",
                choices = names(y %||% data.frame()),
                multiple = TRUE,
                selected = {
                  current <- intersect(input$y_vars %||% character(), names(y %||% data.frame()))
                  if (length(current)) current else automatic$y
                }
              ),
              date_format_input("y_date_format")
            ),
            if (is.null(x)) {
              shiny::div(
                class = "mapping-block empty-state",
                shiny::h5("Tabela X"),
                "Nenhuma tabela X foi importada."
              )
            } else {
              shiny::div(
                class = "mapping-block",
                shiny::h5("Tabela X"),
                shiny::selectInput(
                  ns("x_date"), "Coluna de data",
                  choices = names(x),
                  selected = if (is_valid_choice(input$x_date, names(x))) {
                    input$x_date
                  } else {
                    automatic$x_date
                  }
                ),
                shiny::selectizeInput(
                  ns("x_vars"), "Covariáveis X (máximo 10)",
                  choices = names(x),
                  multiple = TRUE,
                  selected = {
                    current <- intersect(input$x_vars %||% character(), names(x))
                    if (length(current)) current else automatic$x
                  },
                  options = list(maxItems = 10)
                ),
                date_format_input("x_date_format")
              )
            }
          )
        )
      }
    }

    shiny::observeEvent(input$edit_mapping, {
      shiny::showModal(shiny::modalDialog(
        title = "Alterar identificação das colunas",
        mapping_controls(),
        footer = shiny::modalButton("Concluir"),
        size = "l",
        easyClose = TRUE
      ))
    })

    data_state <- shiny::reactive({
      tryCatch({
        if (identical(input$mode, "single")) {
          data <- raw_single()
          if (is.null(data)) return(list(ready = FALSE, message = "Importe uma tabela."))
          automatic <- auto_single_mapping(data)
          date_col <- if (is_valid_choice(input$single_date, names(data))) {
            input$single_date
          } else {
            automatic$date
          }
          y_vars <- intersect(input$single_y %||% character(), names(data))
          if (!length(y_vars)) y_vars <- automatic$y
          x_vars <- intersect(input$single_x %||% character(), names(data))
          if (!length(input$single_x) && is.null(input$single_x)) x_vars <- automatic$x
          overlap <- intersect(y_vars, x_vars)
          if (length(overlap)) {
            stop("Uma coluna não pode ser simultaneamente Y e X: ",
                 paste(overlap, collapse = ", "), ".")
          }
          if (length(x_vars) > 10L) stop("Selecione no máximo 10 covariáveis X.")
          var_y <- prepare_selected_table(
            data, date_col, y_vars,
            input$single_date_format %||% "auto"
          )
          var_x <- if (length(x_vars)) {
            prepare_selected_table(
              data, date_col, x_vars,
              input$single_date_format %||% "auto"
            )
          } else NULL
        } else {
          y <- raw_y()
          if (is.null(y)) return(list(ready = FALSE, message = "Importe a tabela Y."))
          x <- raw_x()
          automatic <- auto_two_table_mapping(y, x)
          y_date <- if (is_valid_choice(input$y_date, names(y))) {
            input$y_date
          } else {
            automatic$y_date
          }
          y_vars <- intersect(input$y_vars %||% character(), names(y))
          if (!length(y_vars)) y_vars <- automatic$y
          var_y <- prepare_selected_table(
            y, y_date, y_vars,
            input$y_date_format %||% "auto"
          )
          x_date <- if (!is.null(x) && is_valid_choice(input$x_date, names(x))) {
            input$x_date
          } else {
            automatic$x_date
          }
          x_vars <- intersect(input$x_vars %||% character(), names(x %||% data.frame()))
          if (!length(input$x_vars) && is.null(input$x_vars)) x_vars <- automatic$x
          if (length(x_vars) > 10L) stop("Selecione no máximo 10 covariáveis X.")
          var_x <- if (!is.null(x) && length(x_vars)) {
            prepare_selected_table(
              x, x_date, x_vars,
              input$x_date_format %||% "auto"
            )
          } else NULL
        }

        y_validation <- validate_monthly_data(var_y, "Y")
        x_validation <- if (is.null(var_x)) {
          validation_result()
        } else {
          validate_monthly_data(var_x, "X", max_variables = 10L)
        }
        errors <- c(y_validation$errors, x_validation$errors)
        warnings <- c(y_validation$warnings, x_validation$warnings)
        list(
          ready = length(errors) == 0L,
          message = if (length(errors)) paste(errors, collapse = "\n") else NULL,
          errors = errors,
          warnings = warnings,
          var_y = var_y,
          var_x = var_x,
          date_y = if (identical(input$mode, "single")) date_col else y_date,
          date_x = if (identical(input$mode, "single")) {
            if (length(x_vars)) date_col else NULL
          } else if (length(x_vars)) {
            x_date
          } else {
            NULL
          },
          mode = input$mode
        )
      }, error = function(e) {
        list(ready = FALSE, message = conditionMessage(e), errors = conditionMessage(e))
      })
    })

    output$preview_y <- DT::renderDT({
      data <- if (identical(input$mode, "single")) raw_single() else raw_y()
      shiny::req(data)
      DT::datatable(
        utils::head(data, 100),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 8)
      )
    })
    output$preview_x <- DT::renderDT({
      data <- if (identical(input$mode, "single")) raw_single() else raw_x()
      shiny::req(data)
      DT::datatable(
        utils::head(data, 100),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 8)
      )
    })

    output$diagnostic_ui <- shiny::renderUI({
      state <- data_state()
      has_data <- if (identical(input$mode, "single")) {
        !is.null(raw_single())
      } else {
        !is.null(raw_y())
      }
      edit_button <- if (has_data) shiny::actionButton(
        ns("edit_mapping"),
        "Alterar identificação",
        icon = shiny::icon("pen-to-square"),
        class = "btn-outline-primary btn-sm mb-3"
      )
      if (!isTRUE(state$ready)) {
        return(shiny::tagList(
          edit_button,
          shiny::div(
            class = "validation-box validation-error",
            shiny::icon("circle-exclamation"),
            shiny::div(
              shiny::strong("Dados ainda não estão prontos"),
              shiny::p(class = "mb-0", state$message %||% "Revise a identificação.")
            )
          )
        ))
      }
      y <- state$var_y
      x <- state$var_x
      warning_ui <- if (length(state$warnings)) {
        shiny::div(
          class = "validation-box validation-warning",
          shiny::icon("triangle-exclamation"),
          shiny::div(lapply(state$warnings, shiny::p))
        )
      }
      shiny::tagList(
        edit_button,
        shiny::div(
          class = "validation-box validation-success",
          shiny::icon("circle-check"),
          shiny::div(
            shiny::strong("Colunas identificadas")
          )
        ),
        warning_ui,
        shiny::tags$dl(
          shiny::tags$dt("Coluna de data Y"),
          shiny::tags$dd(state$date_y %||% "—"),
          shiny::tags$dt("Variáveis Y"),
          shiny::tags$dd(
            class = "diagnostic-section-break",
            paste(setdiff(names(y), "Date"), collapse = ", ")
          ),
          shiny::tags$dt("Coluna de data X"),
          shiny::tags$dd(state$date_x %||% "—"),
          shiny::tags$dt("Variáveis X"),
          shiny::tags$dd(if (is.null(x)) "—" else paste(setdiff(names(x), "Date"), collapse = ", "))
        )
      )
    })

    list(
      data = data_state,
      raw_y = raw_y,
      raw_x = raw_x
    )
  })
}
