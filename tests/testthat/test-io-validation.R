test_that("CSV e TXT são lidos com detecção de separador", {
  csv <- tempfile(fileext = ".csv")
  txt <- tempfile(fileext = ".txt")
  writeLines(c("Date;Y;X", "2020-01-01;1,2;4,5", "2020-02-01;2,3;5,6"), csv)
  writeLines(c("Date\tY", "2020-01-01\t1.2", "2020-02-01\t2.3"), txt)

  csv_data <- read_tabular_file(csv, "dados.csv", decimal_mark = ",")
  txt_data <- read_tabular_file(txt, "dados.txt")

  expect_equal(names(csv_data), c("Date", "Y", "X"))
  expect_equal(csv_data$Y, c(1.2, 2.3))
  expect_equal(names(txt_data), c("Date", "Y"))
})

test_that("XLSX é lido e preserva as colunas", {
  path <- tempfile(fileext = ".xlsx")
  input <- data.frame(Date = c("2020-01-01", "2020-02-01"), Y = c(1, 2))
  openxlsx::write.xlsx(list(Dados = input), path)
  output <- read_tabular_file(path, "dados.xlsx", sheet = "Dados")
  expect_equal(names(output), names(input))
  expect_equal(nrow(output), 2)
})

test_that("formatos de data comuns e datas Excel são convertidos", {
  expect_equal(
    parse_date_column(c("2020-01-01", "2020-02-01"), "YYYY-MM-DD"),
    as.Date(c("2020-01-01", "2020-02-01"))
  )
  expect_equal(
    parse_date_column(c("01/01/2020", "01/02/2020"), "DD/MM/YYYY"),
    as.Date(c("2020-01-01", "2020-02-01"))
  )
  expect_equal(
    parse_date_column(c("2020-01", "2020-02"), "YYYY-MM"),
    as.Date(c("2020-01-01", "2020-02-01"))
  )
  expect_equal(
    parse_date_column(c(43831, 43862)),
    as.Date(c("2020-01-01", "2020-02-01"))
  )
})

test_that("uma tabela permite múltiplas Y e X separadas", {
  input <- data.frame(
    Data = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    Y1 = 1:12, Y2 = 13:24, X1 = 25:36
  )
  y <- prepare_selected_table(input, "Data", c("Y1", "Y2"))
  x <- prepare_selected_table(input, "Data", "X1")
  expect_equal(names(y), c("Date", "Y1", "Y2"))
  expect_equal(names(x), c("Date", "X1"))
})

test_that("colunas são identificadas automaticamente", {
  two <- auto_two_table_mapping(
    example_data_for_tests$y,
    example_data_for_tests$x
  )
  expect_equal(two$y_date, "Date")
  expect_equal(two$x_date, "Date")
  expect_equal(
    two$y,
    c("Subsystem_N", "Subsystem_NE", "Subsystem_S", "Subsystem_SE")
  )
  expect_equal(two$x, c("U1", "NINO3", "Nino3"))

  single <- data.frame(
    Data = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    Y_flow = 1:12,
    X_climate = 13:24
  )
  mapping <- auto_single_mapping(single)
  expect_equal(mapping$date, "Data")
  expect_equal(mapping$y, "Y_flow")
  expect_equal(mapping$x, "X_climate")
})

test_that("módulo prepara duas tabelas sem abrir a alteração", {
  y_file <- tempfile(fileext = ".csv")
  x_file <- tempfile(fileext = ".csv")
  readr::write_csv(example_data_for_tests$y, y_file)
  readr::write_csv(example_data_for_tests$x, x_file)

  shiny::testServer(mod_data_input_server, {
    session$setInputs(
      mode = "two",
      y_file = data.frame(
        name = "y.csv", size = file.info(y_file)$size,
        type = "text/csv", datapath = y_file
      ),
      x_file = data.frame(
        name = "x.csv", size = file.info(x_file)$size,
        type = "text/csv", datapath = x_file
      ),
      y_delimiter = "auto", x_delimiter = "auto",
      y_decimal = ".", x_decimal = ".",
      y_header = TRUE, x_header = TRUE,
      y_encoding = "UTF-8", x_encoding = "UTF-8"
    )
    state <- session$returned$data()
    expect_true(state$ready)
    expect_equal(state$date_y, "Date")
    expect_equal(state$date_x, "Date")
    expect_equal(length(setdiff(names(state$var_y), "Date")), 4)
    expect_equal(length(setdiff(names(state$var_x), "Date")), 3)
    expect_match(output$diagnostic_ui$html, "Alterar identificação", fixed = TRUE)
  })
})

test_that("validação identifica duplicatas, lacunas e valores ausentes", {
  invalid <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-01-01", "2020-03-01")),
    Y = c(1, NA, 3)
  )
  check <- validate_monthly_data(invalid, "Y")
  expect_false(check$ok)
  expect_true(any(grepl("duplicadas", check$errors)))
  expect_true(any(grepl("ausentes", check$errors)))
})

test_that("zero modelos e períodos sobrepostos são rejeitados", {
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", "Subsystem_N"
  )
  check <- validate_run_configuration(
    y, NULL, character(), 1, 1,
    c(1949, 1990), c(1990, 2010), c(2011, 2021)
  )
  expect_false(check$ok)
  expect_true(any(grepl("modelo", check$errors, ignore.case = TRUE)))
  expect_true(any(grepl("sobrepor", check$errors, ignore.case = TRUE)))
})

test_that("períodos aceitam datas mensais exatas", {
  y <- prepare_selected_table(
    example_data_for_tests$y, "Date", "Subsystem_N"
  )
  check <- validate_run_configuration(
    y, NULL, "PAR", 1, 1,
    as.Date(c("1949-03-01", "1990-10-01")),
    as.Date(c("1991-02-01", "2010-09-01")),
    as.Date(c("2011-04-01", "2021-08-01"))
  )
  expect_true(check$ok)
})
