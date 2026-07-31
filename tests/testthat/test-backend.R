test_that("backend reproduz o exemplo para os três modelos", {
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", "Subsystem_N"
  )
  x <- prepare_selected_table(
    example_data_for_tests$x, "Date",
    setdiff(names(example_data_for_tests$x), "Date")
  )
  result <- forecast_ispal(
    y, x,
    models = c("PAR", "PARX", "RIDGE"),
    forecast_lag = 1,
    forecast_months = 1,
    period_calibration = as.Date(c("1949-01-01", "1990-12-31")),
    period_validation = as.Date(c("1991-01-01", "2010-12-31")),
    period_test = as.Date(c("2011-01-01", "2021-12-31"))
  )

  expect_named(
    result[1:4],
    c("Forecast.table", "Error.table", "All.error.table", "Lambda.table")
  )
  expect_equal(nrow(result$Forecast.table), 10)
  expect_equal(sort(result$Error.table$Model), sort(c("PAR", "PARX", "RIDGE")))
  expect_equal(result$Lambda.table$Lambda, 4.4, tolerance = 1e-8)
  expect_equal(
    result$Error.table$KGE[result$Error.table$Model == "PAR"],
    0.6301993744,
    tolerance = 1e-8
  )
  expect_equal(
    result$Error.table$KGE[result$Error.table$Model == "RIDGE"],
    0.6198014867,
    tolerance = 1e-8
  )
})

test_that("múltiplas Y são processadas sem loop externo", {
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", c("Subsystem_N", "Subsystem_NE")
  )
  result <- forecast_ispal(
    y, NULL,
    models = "PAR",
    forecast_lag = 1,
    forecast_months = 1,
    period_calibration = as.Date(c("1949-01-01", "1990-12-31")),
    period_validation = as.Date(c("1991-01-01", "2010-12-31")),
    period_test = as.Date(c("2011-01-01", "2021-12-31"))
  )
  expect_equal(sort(unique(result$Forecast.table$Series)),
               sort(c("Subsystem_N", "Subsystem_NE")))
  expect_equal(unique(result$Error.table$Model), "PAR")
  expect_equal(nrow(result$Lambda.table), 0)
})

test_that("PARX, RIDGE e combinações isoladas retornam tabelas coerentes", {
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", "Subsystem_N"
  )
  x <- prepare_selected_table(
    example_data_for_tests$x, "Date",
    setdiff(names(example_data_for_tests$x), "Date")
  )
  parx <- forecast_ispal(
    y, x, models = "PARX", forecast_lag = 1, forecast_months = 1
  )
  ridge <- forecast_ispal(
    y, x, models = "RIDGE", forecast_lag = 1, forecast_months = 1
  )
  expect_equal(unique(parx$Error.table$Model), "PARX")
  expect_true(all(c("PARX0", "PARX_U1") %in% parx$All.error.table$Model))
  expect_equal(unique(ridge$Error.table$Model), "RIDGE")
  expect_equal(nrow(ridge$Lambda.table), 1)
})

test_that("o backend aceita dez covariáveis", {
  set.seed(123)
  dates <- seq(as.Date("1980-01-01"), by = "month", length.out = 41 * 12)
  x <- replicate(10, stats::arima.sim(list(ar = 0.4), n = length(dates)))
  colnames(x) <- paste0("X", 1:10)
  y_values <- 100 + 0.5 * x[, 1] - 0.3 * x[, 2] + rnorm(length(dates))
  y <- data.frame(Date = dates, Y = y_values)
  x <- data.frame(Date = dates, x, check.names = FALSE)
  result <- forecast_ispal(
    y, x,
    models = c("PARX", "RIDGE"),
    forecast_lag = 1,
    forecast_months = 1,
    period_calibration = as.Date(c("1980-01-01", "1999-12-01")),
    period_validation = as.Date(c("2000-01-01", "2009-12-01")),
    period_test = as.Date(c("2010-01-01", "2020-12-01"))
  )
  expect_equal(length(result$metadata$x_names), 10)
  expect_true("CoefBX10" %in% names(result$Error.table))
  expect_true(nrow(result$Forecast.table) > 0)
})

test_that("as seis métricas são retornadas na ordem definida", {
  metrics <- all_metrics(1:12 + 0.2, 1:12)
  expect_named(
    metrics,
    c("NSE", "Beta.NSE", "KGE", "Alpha.KGE", "Beta.KGE", "r")
  )
})
