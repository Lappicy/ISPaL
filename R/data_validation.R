validation_result <- function(errors = character(), warnings = character(), info = list()) {
  list(
    ok = length(errors) == 0L,
    errors = unique(errors),
    warnings = unique(warnings),
    info = info
  )
}

monthly_sequence <- function(dates) {
  if (!length(dates)) return(as.Date(character()))
  seq(
    as.Date(format(min(dates), "%Y-%m-01")),
    as.Date(format(max(dates), "%Y-%m-01")),
    by = "month"
  )
}

validate_monthly_data <- function(data, role = c("Y", "X"), max_variables = Inf) {
  role <- match.arg(role)
  errors <- warnings <- character()
  if (is.null(data) || !nrow(data)) {
    return(validation_result(paste0("A tabela ", role, " está vazia.")))
  }
  if (!"Date" %in% names(data)) {
    return(validation_result(paste0("A tabela ", role, " não possui a coluna Date.")))
  }
  if (!inherits(data$Date, "Date")) {
    errors <- c(errors, paste0("As datas da tabela ", role, " não foram convertidas corretamente."))
  }
  if (anyNA(data$Date)) {
    errors <- c(errors, paste0("Existem datas ausentes ou inválidas na tabela ", role, "."))
  }
  if (anyDuplicated(data$Date)) {
    duplicated_dates <- unique(data$Date[duplicated(data$Date)])
    errors <- c(
      errors,
      paste0(
        "Existem datas duplicadas na tabela ", role, ": ",
        paste(utils::head(duplicated_dates, 5), collapse = ", "), "."
      )
    )
  }

  value_cols <- setdiff(names(data), "Date")
  if (!length(value_cols)) {
    errors <- c(errors, paste0("Nenhuma variável foi selecionada para a tabela ", role, "."))
  }
  if (length(value_cols) > max_variables) {
    errors <- c(
      errors,
      paste0("A tabela ", role, " possui ", length(value_cols),
             " variáveis selecionadas; o máximo permitido é ", max_variables, ".")
    )
  }
  non_numeric <- value_cols[!vapply(data[value_cols], is.numeric, logical(1))]
  if (length(non_numeric)) {
    errors <- c(
      errors,
      paste0("Variáveis não numéricas em ", role, ": ", paste(non_numeric, collapse = ", "), ".")
    )
  }
  if (length(value_cols) && anyNA(data[value_cols])) {
    columns_na <- value_cols[vapply(data[value_cols], anyNA, logical(1))]
    errors <- c(
      errors,
      paste0("Existem valores ausentes nas variáveis ", role, ": ",
             paste(columns_na, collapse = ", "), ".")
    )
  }

  dates <- sort(unique(data$Date))
  if (length(dates) > 1L) {
    normalized <- as.Date(format(dates, "%Y-%m-01"))
    expected <- monthly_sequence(normalized)
    missing_months <- setdiff(expected, normalized)
    if (length(missing_months)) {
      errors <- c(
        errors,
        paste0(
          "A série ", role, " não é mensal e contínua. Meses ausentes: ",
          paste(utils::head(missing_months, 8), collapse = ", "),
          if (length(missing_months) > 8) "..." else "."
        )
      )
    }
    if (any(format(dates, "%d") != "01")) {
      warnings <- c(
        warnings,
        paste0("As datas de ", role, " não estão todas no primeiro dia do mês; ",
               "a frequência mensal será identificada pelo ano e mês.")
      )
    }
  }

  validation_result(
    errors,
    warnings,
    info = list(
      rows = nrow(data),
      variables = value_cols,
      start = min(data$Date, na.rm = TRUE),
      end = max(data$Date, na.rm = TRUE)
    )
  )
}

validate_periods <- function(period_calibration, period_validation, period_test,
                             y_data, x_data = NULL, models = "PAR") {
  errors <- warnings <- character()
  periods <- tryCatch(
    list(
      calibração = normalize_period_dates(period_calibration),
      `validação (seleção PARX)` = normalize_period_dates(period_validation),
      teste = normalize_period_dates(period_test)
    ),
    error = function(e) e
  )
  if (inherits(periods, "error")) {
    return(validation_result(conditionMessage(periods)))
  }

  for (nm in names(periods)) {
    p <- periods[[nm]]
    if (length(p) != 2L || anyNA(p)) {
      errors <- c(errors, paste0("O período de ", nm, " precisa de data inicial e final."))
    } else if (p[1] > p[2]) {
      errors <- c(errors, paste0("No período de ", nm, ", a data inicial é posterior à final."))
    }
  }

  if (!length(errors)) {
    selection <- periods[["validação (seleção PARX)"]]
    if (periods$calibração[2] >= selection[1]) {
      errors <- c(errors, "Os períodos de calibração e validação não podem se sobrepor.")
    }
    if (selection[2] >= periods$teste[1]) {
      errors <- c(errors, "Os períodos de validação e teste não podem se sobrepor.")
    }

    required_start <- periods$calibração[1]
    required_end <- periods$teste[2]
    y_dates <- as.Date(format(y_data$Date, "%Y-%m-01"))
    if (min(y_dates) > required_start || max(y_dates) < required_end) {
      errors <- c(
        errors,
        paste0(
          "A tabela Y não cobre integralmente as datas solicitadas (",
          format(required_start), " a ", format(required_end), ")."
        )
      )
    }

    if (any(c("PARX", "RIDGE") %in% models)) {
      if (is.null(x_data)) {
        errors <- c(errors, "PARX e RIDGE exigem ao menos uma covariável X.")
      } else {
        x_dates <- as.Date(format(x_data$Date, "%Y-%m-01"))
        if (min(x_dates) > required_start || max(x_dates) < required_end) {
          errors <- c(
            errors,
            paste0(
              "A tabela X não cobre integralmente as datas solicitadas (",
              format(required_start), " a ", format(required_end), ")."
            )
          )
        }
      }
    }

    month_count <- function(period) length(seq(period[1], period[2], by = "month"))
    n_cal <- month_count(periods$calibração)
    n_val <- month_count(selection)
    n_test <- month_count(periods$teste)
    if (n_cal < 60L) warnings <- c(warnings, "O período de calibração possui menos de cinco anos.")
    if (n_val < 36L) {
      warnings <- c(warnings, "O período de validação para seleção PARX possui menos de três anos.")
    }
    if (n_test < 36L) warnings <- c(warnings, "O período de teste possui menos de três anos.")
  }

  validation_result(errors, warnings, periods)
}

validate_run_configuration <- function(y_data, x_data, models, lags, months,
                                       period_calibration, period_validation,
                                       period_test, max_x = 10L) {
  errors <- warnings <- character()
  if (!length(models)) errors <- c(errors, "Selecione ao menos um modelo.")
  invalid_models <- setdiff(models, c("PAR", "PARX", "RIDGE"))
  if (length(invalid_models)) {
    errors <- c(errors, paste0("Modelos inválidos: ", paste(invalid_models, collapse = ", "), "."))
  }
  if (!length(lags) || any(!lags %in% 1:24)) {
    errors <- c(errors, "Selecione ao menos um lead válido entre 1 e 24 meses.")
  }
  if (!length(months) || any(!months %in% 1:12)) {
    errors <- c(errors, "Selecione ao menos um mês válido.")
  }

  y_check <- validate_monthly_data(y_data, "Y")
  errors <- c(errors, y_check$errors)
  warnings <- c(warnings, y_check$warnings)

  if (!is.null(x_data)) {
    x_check <- validate_monthly_data(x_data, "X", max_variables = max_x)
    errors <- c(errors, x_check$errors)
    warnings <- c(warnings, x_check$warnings)
  } else if (any(c("PARX", "RIDGE") %in% models)) {
    errors <- c(errors, "Selecione covariáveis X para executar PARX ou RIDGE.")
  }

  period_check <- validate_periods(
    period_calibration, period_validation, period_test,
    y_data, x_data, models
  )
  errors <- c(errors, period_check$errors)
  warnings <- c(warnings, period_check$warnings)
  validation_result(errors, warnings)
}
