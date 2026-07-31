file_extension <- function(path_or_name) {
  tolower(tools::file_ext(path_or_name %||% ""))
}

is_excel_file <- function(path_or_name) {
  file_extension(path_or_name) %in% c("xlsx", "xls")
}

guess_delimiter <- function(path, encoding = "UTF-8") {
  line <- readLines(path, n = 1L, warn = FALSE, encoding = encoding)
  if (!length(line)) return(",")
  candidates <- c("\t", ";", ",", "|")
  counts <- vapply(
    candidates,
    function(delim) lengths(regmatches(line, gregexpr(delim, line, fixed = TRUE))),
    integer(1)
  )
  if (max(counts) == 0L) "," else candidates[which.max(counts)]
}

excel_sheets_safe <- function(path) {
  if (is.null(path) || !file.exists(path)) return(character())
  tryCatch(readxl::excel_sheets(path), error = function(e) character())
}

read_tabular_file <- function(path, original_name = path, sheet = NULL,
                              delimiter = "auto", decimal_mark = ".",
                              header = TRUE, encoding = "UTF-8") {
  stopifnot(file.exists(path))
  ext <- file_extension(original_name)

  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    selected_sheet <- sheet %||% sheets[[1]]
    out <- readxl::read_excel(
      path,
      sheet = selected_sheet,
      col_names = isTRUE(header),
      .name_repair = "minimal"
    )
    return(as.data.frame(out, check.names = FALSE))
  }

  if (!ext %in% c("csv", "txt", "tsv", "dat")) {
    stop("Formato não reconhecido. Use CSV, TXT, TSV, XLS ou XLSX.")
  }

  delim <- delimiter
  if (identical(delim, "auto")) {
    delim <- guess_delimiter(path, encoding)
  }
  if (identical(delim, "\\t")) delim <- "\t"

  out <- readr::read_delim(
    file = path,
    delim = delim,
    col_names = isTRUE(header),
    locale = readr::locale(decimal_mark = decimal_mark, encoding = encoding),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
  as.data.frame(out, check.names = FALSE)
}

parse_date_column <- function(x, format = "auto") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))

  if (is.numeric(x)) {
    if (all(is.na(x) | x > 10000)) {
      return(as.Date(x, origin = "1899-12-30"))
    }
    stop("A coluna numérica de data não parece usar o sistema de datas do Excel.")
  }

  values <- trimws(as.character(x))
  values[values == ""] <- NA_character_

  parse_one <- function(fmt) {
    if (fmt == "YYYY-MM") {
      return(as.Date(paste0(values, "-01"), format = "%Y-%m-%d"))
    }
    format_map <- c(
      "YYYY-MM-DD" = "%Y-%m-%d",
      "DD/MM/YYYY" = "%d/%m/%Y",
      "MM/DD/YYYY" = "%m/%d/%Y",
      "DD-MM-YYYY" = "%d-%m-%Y",
      "YYYY/MM/DD" = "%Y/%m/%d"
    )
    as.Date(values, format = unname(format_map[[fmt]]))
  }

  if (!identical(format, "auto")) {
    parsed <- parse_one(format)
    if (any(is.na(parsed) & !is.na(values))) {
      stop("Algumas datas não puderam ser interpretadas no formato selecionado.")
    }
    return(parsed)
  }

  formats <- c(
    "YYYY-MM-DD", "DD/MM/YYYY", "MM/DD/YYYY",
    "YYYY-MM", "DD-MM-YYYY", "YYYY/MM/DD"
  )
  attempts <- lapply(formats, parse_one)
  scores <- vapply(
    attempts,
    function(z) sum(!is.na(z) | is.na(values)),
    integer(1)
  )
  parsed <- attempts[[which.max(scores)]]

  if (any(is.na(parsed) & !is.na(values))) {
    suppressWarnings({
      parsed_lubridate <- as.Date(lubridate::parse_date_time(
        values,
        orders = c("ymd", "dmy", "mdy", "ym"),
        quiet = TRUE
      ))
    })
    if (sum(!is.na(parsed_lubridate)) > sum(!is.na(parsed))) {
      parsed <- parsed_lubridate
    }
  }

  if (any(is.na(parsed) & !is.na(values))) {
    stop(paste0(
      "Não foi possível interpretar todas as datas. Exemplos problemáticos: ",
      paste(utils::head(unique(values[is.na(parsed) & !is.na(values)]), 4), collapse = ", ")
    ))
  }
  parsed
}

prepare_selected_table <- function(data, date_col, value_cols,
                                   date_format = "auto", date_name = "Date") {
  if (is.null(data) || !nrow(data)) stop("A tabela está vazia.")
  if (!date_col %in% names(data)) stop("Selecione uma coluna de data válida.")
  if (!length(value_cols)) stop("Selecione ao menos uma variável.")
  if (any(!value_cols %in% names(data))) stop("Uma variável selecionada não existe na tabela.")
  if (date_col %in% value_cols) stop("A coluna de data não pode ser usada como variável.")

  out <- data[, c(date_col, value_cols), drop = FALSE]
  names(out)[1] <- date_name
  out[[date_name]] <- parse_date_column(out[[date_name]], date_format)
  out <- out[order(out[[date_name]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

normalized_column_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(gsub("[^[:alnum:]]+", "", x))
}

detect_date_column <- function(data) {
  if (is.null(data) || !ncol(data)) return(NULL)
  normalized <- normalized_column_name(names(data))
  preferred <- c("date", "data", "datetime", "tempo", "time", "mes", "month")
  named_match <- which(normalized %in% preferred)
  candidates <- c(named_match, setdiff(seq_along(data), named_match))

  for (index in candidates) {
    parsed <- tryCatch(
      parse_date_column(data[[index]], "auto"),
      error = function(e) NULL
    )
    if (is.null(parsed) || anyNA(parsed) || length(unique(parsed)) < 2L) next
    years <- suppressWarnings(as.integer(format(parsed, "%Y")))
    if (all(years >= 1800L & years <= 2200L)) return(names(data)[index])
  }
  names(data)[1]
}

auto_single_mapping <- function(data, max_x = 10L) {
  date_col <- detect_date_column(data)
  values <- setdiff(names(data), date_col)
  normalized <- normalized_column_name(values)
  y_matches <- values[grepl("^(y|target|response|dependent)", normalized)]
  x_matches <- values[grepl("^(x|cov|clim|exog|predictor)", normalized)]

  if (!length(y_matches) || length(intersect(y_matches, x_matches))) {
    y_matches <- utils::head(values, 1L)
  }
  x_matches <- setdiff(x_matches, y_matches)
  if (!length(x_matches)) x_matches <- setdiff(values, y_matches)

  list(
    date = date_col,
    y = y_matches,
    x = utils::head(x_matches, max_x),
    date_format = "auto"
  )
}

auto_two_table_mapping <- function(y_data, x_data = NULL, max_x = 10L) {
  y_date <- detect_date_column(y_data)
  x_date <- detect_date_column(x_data)
  list(
    y_date = y_date,
    y = setdiff(names(y_data %||% data.frame()), y_date),
    y_date_format = "auto",
    x_date = x_date,
    x = utils::head(setdiff(names(x_data %||% data.frame()), x_date), max_x),
    x_date_format = "auto"
  )
}

data_summary <- function(data, date_col = NULL, selected = character()) {
  if (is.null(data)) return(NULL)
  dates <- NULL
  if (!is.null(date_col) && date_col %in% names(data)) {
    dates <- tryCatch(parse_date_column(data[[date_col]], "auto"), error = function(e) NULL)
  }
  list(
    rows = nrow(data),
    columns = ncol(data),
    missing = sum(is.na(data)),
    date_range = if (is.null(dates)) "Não interpretada" else format_range(dates),
    selected = selected
  )
}

load_example_data <- function() {
  data_dir <- ispal_extdata_dir("data")
  streamflow_path <- file.path(data_dir, "StreamflowEnergy.rda")
  climate_path <- file.path(data_dir, "ClimaticInfo.rda")

  if (!file.exists(streamflow_path) || !file.exists(climate_path)) {
    stop(
      "Dados de exemplo não encontrados em inst/extdata/data. ",
      "Verifique se a pasta ISPaL_Shiny foi baixada completa."
    )
  }

  env <- new.env(parent = emptyenv())
  load(streamflow_path, envir = env)
  load(climate_path, envir = env)
  list(
    y = env$StreamflowEnergy,
    x = env$ClimaticInfo
  )
}
