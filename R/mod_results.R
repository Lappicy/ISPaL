result_table_card <- function(ns, key, label) {
  bslib::nav_panel(
    label,
    shiny::div(
      class = "result-toolbar",
      shiny::span(class = "result-count", shiny::textOutput(ns(paste0(key, "_count")), inline = TRUE))
    ),
    DT::DTOutput(ns(paste0(key, "_table")))
  )
}

mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "download-all-row",
      shiny::downloadButton(
        ns("download_all"),
        "Baixar tabelas em excel",
        icon = shiny::icon("file-excel"),
        class = "btn-success"
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::navset_card_tab(
        result_table_card(ns, "forecast", "Forecast"),
        result_table_card(ns, "errors", "Errors"),
        result_table_card(ns, "all_errors", "All Errors"),
        result_table_card(ns, "lambda", "Lambda")
      )
    )
  )
}

mod_results_server <- function(id, results) {
  shiny::moduleServer(id, function(input, output, session) {
    table_map <- c(
      forecast = "Forecast.table",
      errors = "Error.table",
      all_errors = "All.error.table",
      lambda = "Lambda.table"
    )

    for (key in names(table_map)) {
      local({
        current_key <- key
        result_name <- unname(table_map[current_key])
        data_reactive <- shiny::reactive({
          result <- results()
          if (is.null(result)) return(data.frame())
          result[[result_name]] %||% data.frame()
        })
        output[[paste0(current_key, "_table")]] <- DT::renderDT({
          data <- data_reactive()
          if (!nrow(data)) {
            return(DT::datatable(
              data.frame(Mensagem = "Execute os modelos para visualizar esta tabela."),
              rownames = FALSE,
              options = list(dom = "t")
            ))
          }
          DT::datatable(
            data,
            rownames = FALSE,
            selection = "none",
            options = list(
              dom = "tip",
              pageLength = 25,
              scrollX = TRUE,
              deferRender = TRUE
            )
          )
        }, server = FALSE)

        output[[paste0(current_key, "_count")]] <- shiny::renderText({
          data <- data_reactive()
          paste0(
            format(nrow(data), big.mark = ".", decimal.mark = ","),
            " linhas"
          )
        })

      })
    }

    output$download_all <- shiny::downloadHandler(
      filename = function() paste0("ISPaL_resultados_", Sys.Date(), ".xlsx"),
      content = function(file) {
        result <- results()
        shiny::req(result)
        write_results_workbook(result, file)
      }
    )
  })
}
