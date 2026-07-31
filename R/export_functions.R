write_csv_semicolon <- function(data, path) {
  utils::write.table(
    data,
    file = path,
    sep = ";",
    dec = ",",
    row.names = FALSE,
    col.names = TRUE,
    fileEncoding = "UTF-8",
    qmethod = "double"
  )
  invisible(path)
}

write_xlsx_table <- function(data, path, sheet = "Dados") {
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, sheet)
  openxlsx::writeDataTable(
    workbook, sheet, data,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )
  if (ncol(data)) {
    openxlsx::freezePane(workbook, sheet, firstRow = TRUE)
    openxlsx::setColWidths(workbook, sheet, cols = seq_len(ncol(data)), widths = "auto")
  }
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

write_results_workbook <- function(results, path) {
  workbook <- openxlsx::createWorkbook()
  table_names <- c("Forecast", "Errors", "All_Errors", "Lambda")
  result_names <- c("Forecast.table", "Error.table", "All.error.table", "Lambda.table")
  for (i in seq_along(result_names)) {
    data <- results[[result_names[i]]] %||% data.frame()
    openxlsx::addWorksheet(workbook, table_names[i])
    openxlsx::writeData(
      workbook, table_names[i], data,
      withFilter = nrow(data) > 0L
    )
    if (ncol(data)) {
      openxlsx::freezePane(workbook, table_names[i], firstRow = TRUE)
      openxlsx::setColWidths(
        workbook, table_names[i],
        cols = seq_len(ncol(data)), widths = "auto"
      )
    }
  }
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

save_metric_plot <- function(plot, path, width_mm = 250, height_mm = 300,
                             dpi = 300, device = c("png", "pdf")) {
  device <- match.arg(device)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = if (device == "png") dpi else 300,
    bg = "white",
    device = device,
    limitsize = FALSE
  )
  invisible(path)
}

