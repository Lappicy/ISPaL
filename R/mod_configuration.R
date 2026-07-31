mod_configuration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Modelos"),
        shinyWidgets::prettyCheckboxGroup(
          ns("models"),
          NULL,
          choices = c("PAR", "PARX", "RIDGE"),
          selected = c("PAR", "PARX", "RIDGE"),
          status = "primary",
          animation = "smooth"
        ),
        shiny::p(
          class = "form-text",
          "PAR pode ser executado sem covariáveis X. PARX e RIDGE exigem covariáveis."
        )
      ),
      bslib::card(
        bslib::card_header("Horizonte e meses previstos"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectizeInput(
            ns("lags"),
            "Leads (meses)",
            choices = 1:24,
            selected = 1:6,
            multiple = TRUE
          ),
          shiny::selectizeInput(
            ns("months"),
            "Meses",
            choices = stats::setNames(1:12, paste0(1:12, " — ", month_labels_pt)),
            selected = 1:12,
            multiple = TRUE
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header("Períodos"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::div(
          class = "period-block",
          shiny::h5("Calibração"),
          shiny::dateInput(
            ns("cal_start"), "Data inicial",
            value = as.Date("1949-01-01"), format = "dd/mm/yyyy"
          ),
          shiny::dateInput(
            ns("cal_end"), "Data final",
            value = as.Date("1990-12-31"), format = "dd/mm/yyyy"
          )
        ),
        shiny::div(
          class = "period-block",
          shiny::h5("Validação (seleção PARX)"),
          shiny::dateInput(
            ns("val_start"), "Data inicial",
            value = as.Date("1991-01-01"), format = "dd/mm/yyyy"
          ),
          shiny::dateInput(
            ns("val_end"), "Data final",
            value = as.Date("2010-12-31"), format = "dd/mm/yyyy"
          )
        ),
        shiny::div(
          class = "period-block",
          shiny::h5("Teste"),
          shiny::dateInput(
            ns("test_start"), "Data inicial",
            value = as.Date("2011-01-01"), format = "dd/mm/yyyy"
          ),
          shiny::dateInput(
            ns("test_end"), "Data final",
            value = as.Date("2021-12-31"), format = "dd/mm/yyyy"
          )
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header("Resumo e validação"),
        shiny::uiOutput(ns("configuration_summary"))
      ),
      bslib::card(
        class = "run-card",
        bslib::card_header("Executar"),
        shiny::actionButton(
          ns("run"),
          "Executar modelos",
          icon = shiny::icon("play"),
          class = "btn-success btn-lg w-100"
        ),
        shiny::uiOutput(ns("run_status"))
      )
    )
  )
}

mod_configuration_server <- function(id, data_state) {
  shiny::moduleServer(id, function(input, output, session) {
    results <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(NULL)
    last_models <- shiny::reactiveVal(c("PAR", "PARX", "RIDGE"))

    shiny::observeEvent(input$models, {
      if (!length(input$models)) {
        shiny::updateCheckboxGroupInput(
          session, "models", selected = last_models()
        )
        shiny::showNotification("Ao menos um modelo deve permanecer selecionado.", type = "warning")
      } else {
        last_models(input$models)
      }
    }, ignoreInit = TRUE)

    configuration <- shiny::reactive({
      list(
        models = input$models %||% character(),
        lags = sort(as.integer(input$lags %||% integer())),
        months = sort(as.integer(input$months %||% integer())),
        calibration = as.Date(c(input$cal_start, input$cal_end)),
        validation = as.Date(c(input$val_start, input$val_end)),
        test = as.Date(c(input$test_start, input$test_end))
      )
    })

    run_validation <- shiny::reactive({
      state <- data_state()
      if (!isTRUE(state$ready)) {
        return(validation_result(state$message %||% "Prepare os dados na aba Dados."))
      }
      cfg <- configuration()
      validate_run_configuration(
        state$var_y, state$var_x,
        cfg$models, cfg$lags, cfg$months,
        cfg$calibration, cfg$validation, cfg$test
      )
    })

    output$configuration_summary <- shiny::renderUI({
      state <- data_state()
      check <- run_validation()
      cfg <- configuration()
      if (!isTRUE(state$ready)) {
        return(shiny::div(
          class = "validation-box validation-error",
          shiny::icon("database"),
          shiny::div("Prepare e valide os dados antes de executar.")
        ))
      }
      message_class <- if (check$ok) "validation-success" else "validation-error"
      shiny::tagList(
        shiny::div(
          class = paste("validation-box", message_class),
          shiny::icon(if (check$ok) "circle-check" else "circle-exclamation"),
          shiny::div(
            shiny::strong(if (check$ok) "Configuração pronta" else "Revise a configuração"),
            if (length(check$errors)) shiny::tags$ul(lapply(check$errors, shiny::tags$li))
          )
        ),
        if (length(check$warnings)) shiny::div(
          class = "validation-box validation-warning",
          shiny::icon("triangle-exclamation"),
          shiny::tags$ul(lapply(check$warnings, shiny::tags$li))
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::div(
            shiny::strong("Dados"),
            shiny::p(paste("Y:", paste(setdiff(names(state$var_y), "Date"), collapse = ", "))),
            shiny::p(paste(
              "X:",
              if (is.null(state$var_x)) "nenhuma" else
                paste(setdiff(names(state$var_x), "Date"), collapse = ", ")
            ))
          ),
          shiny::div(
            shiny::strong("Execução"),
            shiny::p(paste("Modelos:", paste(cfg$models, collapse = ", "))),
            shiny::p(paste(length(cfg$lags), "lead(s) ·", length(cfg$months), "mês(es)")),
            shiny::div(
              class = "period-summary",
              shiny::div(
                shiny::strong("Calibração: "),
                format(cfg$calibration[1]), " a ", format(cfg$calibration[2])
              ),
              shiny::div(
                shiny::strong("Validação: "),
                format(cfg$validation[1]), " a ", format(cfg$validation[2])
              ),
              shiny::div(
                shiny::strong("Teste: "),
                format(cfg$test[1]), " a ", format(cfg$test[2])
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$run, {
      state <- data_state()
      check <- run_validation()
      if (!check$ok) {
        shiny::showNotification(
          paste(check$errors, collapse = "\n"),
          type = "error",
          duration = 10
        )
        return()
      }
      cfg <- configuration()
      status(list(type = "running", message = "Execução iniciada."))
      session$sendCustomMessage("toggle-disabled", list(id = session$ns("run"), disabled = TRUE))

      output_value <- tryCatch(
        shiny::withProgress(message = "Executando modelos", value = 0, {
          forecast_ispal(
            var_y = state$var_y,
            var_x = state$var_x,
            models = cfg$models,
            forecast_lag = cfg$lags,
            forecast_months = cfg$months,
            period_calibration = cfg$calibration,
            period_validation = cfg$validation,
            period_test = cfg$test,
            progress = function(current, total, detail) {
              shiny::setProgress(
                value = current / total,
                detail = paste(current, "de", total, "—", detail)
              )
            }
          )
        }),
        error = function(e) e
      )
      session$sendCustomMessage("toggle-disabled", list(id = session$ns("run"), disabled = FALSE))

      if (inherits(output_value, "error")) {
        status(list(type = "error", message = conditionMessage(output_value)))
        shiny::showNotification(
          paste("A execução foi interrompida:", conditionMessage(output_value)),
          type = "error",
          duration = NULL
        )
      } else {
        results(output_value)
        status(list(
          type = "success",
          message = paste(
            format(
              nrow(output_value$Forecast.table),
              big.mark = ".",
              decimal.mark = ","
            ),
            "previsões produzidas."
          )
        ))
        if (length(output_value$warnings)) {
          shiny::showNotification(
            paste(output_value$warnings, collapse = "\n"),
            type = "warning",
            duration = 12
          )
        }
        shiny::showNotification("Modelos concluídos com sucesso.", type = "message")
      }
    })

    output$run_status <- shiny::renderUI({
      current <- status()
      if (is.null(current)) {
        return(shiny::p(class = "form-text mt-3", "Os resultados serão mantidos durante esta sessão."))
      }
      classes <- c(
        running = "status-running",
        error = "status-error",
        success = "status-success"
      )
      shiny::div(
        class = paste("run-status mt-3", classes[[current$type]]),
        current$message
      )
    })

    list(
      results = shiny::reactive(results()),
      configuration = configuration,
      validation = run_validation
    )
  })
}
