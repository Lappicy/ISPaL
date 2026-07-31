`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

ispal_app_dir <- function() {
  getOption("ispal.app.dir", normalizePath(".", mustWork = FALSE))
}

ispal_extdata_dir <- function(...) {
  normalizePath(file.path(ispal_app_dir(), "inst", "extdata", ...), mustWork = FALSE)
}

month_labels_pt <- c(
  "1" = "Jan", "2" = "Fev", "3" = "Mar", "4" = "Abr",
  "5" = "Mai", "6" = "Jun", "7" = "Jul", "8" = "Ago",
  "9" = "Set", "10" = "Out", "11" = "Nov", "12" = "Dez"
)

metric_labels <- c(
  "NSE" = "NSE",
  "Beta.NSE" = "β₍NSE₎",
  "KGE" = "KGE",
  "Alpha.KGE" = "α₍KGE₎",
  "Beta.KGE" = "β₍KGE₎",
  "r" = "r"
)

recognized_metrics <- names(metric_labels)

ideal_metric_values <- function() {
  c(
    "NSE" = 1,
    "Beta.NSE" = 0,
    "KGE" = 1,
    "Alpha.KGE" = 1,
    "Beta.KGE" = 1,
    "r" = 1
  )
}

metric_plot_label <- function(metric) {
  switch(
    metric,
    "NSE" = expression(NSE),
    "Beta.NSE" = expression(beta[NSE]),
    "KGE" = expression(KGE),
    "Alpha.KGE" = expression(alpha[KGE]),
    "Beta.KGE" = expression(beta[KGE]),
    "r" = expression(r),
    metric
  )
}

metric_plot_parse_label <- function(metric) {
  switch(
    metric,
    "NSE" = "NSE",
    "Beta.NSE" = "beta[NSE]",
    "KGE" = "KGE",
    "Alpha.KGE" = "alpha[KGE]",
    "Beta.KGE" = "beta[KGE]",
    "r" = "r",
    metric
  )
}

format_variable_context <- function(y_names = character(), x_names = character(),
                                    y_fallback = "não utilizada",
                                    x_fallback = "não utilizada") {
  y_text <- if (length(y_names)) paste(unique(y_names), collapse = ", ") else y_fallback
  x_text <- if (length(x_names)) paste(unique(x_names), collapse = ", ") else x_fallback
  paste0("Y: ", y_text, " | X: ", x_text)
}

model_palette <- function(models = character()) {
  base <- c(
    "PAR" = "#C7E84A",
    "PARX" = "#47A9A3",
    "RIDGE" = "#6E4E9B",
    "PARX0" = "#7B8794"
  )
  extra <- setdiff(models, names(base))
  if (length(extra)) {
    extra_cols <- grDevices::hcl.colors(length(extra), "Dark 3")
    names(extra_cols) <- extra
    base <- c(base, extra_cols)
  }
  base[intersect(names(base), models)]
}

safe_name <- function(x) {
  x <- gsub("[^[:alnum:]_]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(ifelse(nzchar(x), x, "variavel"))
}

as_character_message <- function(x) {
  paste(as.character(x), collapse = "\n")
}

compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

format_range <- function(x) {
  if (!length(x) || all(is.na(x))) return("—")
  paste(format(min(x, na.rm = TRUE)), format(max(x, na.rm = TRUE)), sep = " a ")
}

normalize_period_dates <- function(period) {
  if (length(period) != 2L || anyNA(period)) {
    stop("Cada período precisa de uma data inicial e uma data final.")
  }
  if (is.numeric(period) && all(period >= 1000 & period <= 9999)) {
    dates <- as.Date(c(
      sprintf("%04d-01-01", as.integer(period[1])),
      sprintf("%04d-12-01", as.integer(period[2]))
    ))
  } else if (is.character(period) && all(grepl("^\\d{4}$", period))) {
    dates <- as.Date(c(
      paste0(period[1], "-01-01"),
      paste0(period[2], "-12-01")
    ))
  } else {
    dates <- as.Date(period)
  }
  if (anyNA(dates)) stop("Não foi possível interpretar as datas do período.")
  as.Date(format(dates, "%Y-%m-01"))
}

empty_result_tables <- function() {
  list(
    Forecast.table = data.frame(),
    Error.table = data.frame(),
    All.error.table = data.frame(),
    Lambda.table = data.frame()
  )
}
