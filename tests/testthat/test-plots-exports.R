small_result <- local({
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", c("Subsystem_N", "Subsystem_NE")
  )
  x <- prepare_selected_table(
    example_data_for_tests$x, "Date",
    setdiff(names(example_data_for_tests$x), "Date")
  )
  forecast_ispal(
    y, x,
    forecast_lag = 1:2,
    forecast_months = 1:2
  )
})

test_that("gráfico principal é gerado a partir do resultado calculado", {
  plot <- build_metric_distribution_plot(
    small_result$Error.table,
    c("NSE", "Beta.NSE", "KGE", "Beta.KGE")
  )
  expect_s3_class(plot, "patchwork")
})

test_that("tabela pronta com coluna Modelo é normalizada", {
  ready <- small_result$Error.table
  names(ready)[names(ready) == "Model"] <- "Modelo"
  names(ready)[names(ready) == "NSE"] <- "NSE.orig"
  names(ready)[names(ready) == "KGE"] <- "KGE.orig"
  names(ready)[names(ready) == "r"] <- "r.KGE"
  normalized <- normalize_result_columns(ready)
  expect_true(all(c("Model", "NSE", "KGE", "r") %in% names(normalized)))
  plot <- build_metric_distribution_plot(normalized, c("NSE", "KGE"))
  expect_s3_class(plot, "patchwork")
})

test_that("gráfico de vencedores é gerado", {
  winners <- metric_winner_table(
    small_result$Error.table,
    c("NSE", "KGE"),
    "Month"
  )
  expect_true(all(c("Month", "Metric", "Model", "Quantity") %in% names(winners)))
  expect_s3_class(
    build_winner_plot(small_result$Error.table, c("NSE"), "Lag"),
    "ggplot"
  )
})

test_that("CSV e XLSX são exportados", {
  csv <- tempfile(fileext = ".csv")
  xlsx <- tempfile(fileext = ".xlsx")
  all_xlsx <- tempfile(fileext = ".xlsx")
  write_csv_semicolon(small_result$Error.table, csv)
  write_xlsx_table(small_result$Error.table, xlsx)
  write_results_workbook(small_result, all_xlsx)
  expect_gt(file.info(csv)$size, 0)
  expect_gt(file.info(xlsx)$size, 0)
  expect_gt(file.info(all_xlsx)$size, 0)
  expect_equal(
    openxlsx::getSheetNames(all_xlsx),
    c("Forecast", "Errors", "All_Errors", "Lambda")
  )
})

test_that("PNG e PDF são exportados", {
  plot <- build_metric_distribution_plot(
    small_result$Error.table,
    c("NSE", "KGE")
  )
  png <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  save_metric_plot(plot, png, 180, 150, 100, "png")
  save_metric_plot(plot, pdf, 180, 150, 300, "pdf")
  expect_gt(file.info(png)$size, 0)
  expect_gt(file.info(pdf)$size, 0)
})

test_that("CCF aceita várias séries Y simultaneamente", {
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", c("Subsystem_N", "Subsystem_NE")
  )
  x <- prepare_selected_table(
    example_data_for_tests$x, "Date", c("U1", "NINO3")
  )
  plot <- build_ccf_plot(
    y, x,
    c("Subsystem_N", "Subsystem_NE"),
    c("U1", "NINO3"),
    lag_max = 6
  )
  expect_s3_class(plot, "ggplot")
})
