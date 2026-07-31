shift_months <- function(date, n) {
  lt <- as.POSIXlt(as.Date(date))
  total <- (lt$year + 1900L) * 12L + lt$mon - as.integer(n)
  year <- total %/% 12L
  month <- total %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", year, month))
}

nse_metrics <- function(sim, obs) {
  sim <- as.numeric(sim)
  obs <- as.numeric(obs)
  if (length(sim) < 2L || anyNA(sim) || anyNA(obs)) {
    return(data.frame(
      NSE = NA_real_,
      Beta.NSE = NA_real_
    ))
  }
  sd_obs <- stats::sd(obs)
  beta <- if (sd_obs == 0) NA_real_ else (mean(sim) - mean(obs)) / sd_obs
  denominator <- sum((obs - mean(obs))^2)
  nse <- if (denominator == 0) NA_real_ else {
    1 - sum((sim - obs)^2) / denominator
  }
  data.frame(
    NSE = nse,
    Beta.NSE = beta
  )
}

kge_metrics <- function(sim, obs) {
  sim <- as.numeric(sim)
  obs <- as.numeric(obs)
  if (length(sim) < 2L || anyNA(sim) || anyNA(obs)) {
    return(data.frame(
      KGE = NA_real_,
      Alpha.KGE = NA_real_,
      Beta.KGE = NA_real_,
      r = NA_real_
    ))
  }
  sd_obs <- stats::sd(obs)
  sd_sim <- stats::sd(sim)
  alpha <- if (sd_obs == 0) NA_real_ else sd_sim / sd_obs
  beta <- if (mean(obs) == 0) NA_real_ else mean(sim) / mean(obs)
  correlation <- if (sd_sim == 0 || sd_obs == 0) 0 else stats::cor(sim, obs)
  kge <- 1 - sqrt((correlation - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  data.frame(
    KGE = kge,
    Alpha.KGE = alpha,
    Beta.KGE = beta,
    r = correlation
  )
}

all_metrics <- function(sim, obs) {
  cbind(nse_metrics(sim, obs), kge_metrics(sim, obs))
}

make_period_pairs <- function(y_data, x_data, y_name, x_names,
                              period, month, lag) {
  period <- normalize_period_dates(period)
  all_target_dates <- seq(period[1], period[2], by = "month")
  target_dates <- all_target_dates[
    as.integer(format(all_target_dates, "%m")) == as.integer(month)
  ]
  predictor_dates <- shift_months(target_dates, lag)
  allowed <- predictor_dates >= period[1] & predictor_dates <= period[2]
  target_dates <- target_dates[allowed]
  predictor_dates <- predictor_dates[allowed]

  target_index <- match(target_dates, as.Date(format(y_data$Date, "%Y-%m-01")))
  predictor_y_index <- match(predictor_dates, as.Date(format(y_data$Date, "%Y-%m-01")))

  out <- data.frame(
    Date = target_dates,
    y = y_data[[y_name]][target_index],
    y_lag = y_data[[y_name]][predictor_y_index],
    check.names = FALSE
  )

  if (length(x_names)) {
    predictor_x_index <- match(
      predictor_dates,
      as.Date(format(x_data$Date, "%Y-%m-01"))
    )
    x_values <- x_data[predictor_x_index, x_names, drop = FALSE]
    names(x_values) <- paste0("x", seq_along(x_names))
    out <- cbind(out, x_values)
  }

  out[stats::complete.cases(out), , drop = FALSE]
}

fit_ols_model <- function(data, predictors) {
  if (!nrow(data)) stop("Não há observações para ajustar o modelo.")
  matrix_x <- cbind("(Intercept)" = 1, as.matrix(data[, predictors, drop = FALSE]))
  fit <- stats::lm.fit(matrix_x, data$y)
  coefficients <- fit$coefficients
  coefficients[is.na(coefficients)] <- 0
  names(coefficients) <- colnames(matrix_x)
  list(
    coefficients = coefficients,
    predictors = predictors,
    predict = function(newdata) {
      new_x <- cbind("(Intercept)" = 1, as.matrix(newdata[, predictors, drop = FALSE]))
      as.numeric(new_x %*% coefficients[colnames(new_x)])
    }
  )
}

ridge_lambda_grid <- function() {
  c(seq(0, 99.95, by = 0.05), seq(100, 1000, by = 1))
}

fit_ridge_model <- function(data, predictors, lambda_grid = ridge_lambda_grid()) {
  if (!length(predictors)) stop("RIDGE exige ao menos um preditor.")
  formula <- stats::reformulate(predictors, response = "y")
  ridge_path <- MASS::lm.ridge(formula, data = data, lambda = lambda_grid)
  best_index <- which.min(ridge_path$GCV)
  lambda <- lambda_grid[best_index]
  ridge_fit <- MASS::lm.ridge(formula, data = data, lambda = lambda)
  coefficients <- stats::coef(ridge_fit)
  if (length(coefficients) && (is.na(names(coefficients)[1]) || names(coefficients)[1] == "")) {
    names(coefficients)[1] <- "(Intercept)"
  }
  list(
    coefficients = coefficients,
    predictors = predictors,
    lambda = lambda,
    predict = function(newdata) {
      matrix_x <- cbind(
        "(Intercept)" = 1,
        as.matrix(newdata[, predictors, drop = FALSE])
      )
      as.numeric(matrix_x %*% coefficients[colnames(matrix_x)])
    }
  )
}

enumerate_subsets <- function(n) {
  if (n == 0L) return(list(integer()))
  lapply(0:(2^n - 1L), function(mask) {
    which(as.logical(intToBits(mask)[seq_len(n)]))
  })
}

select_parx_model <- function(calibration_data, test_data, x_predictors) {
  subsets <- enumerate_subsets(length(x_predictors))
  scores <- rep(-Inf, length(subsets))
  fits <- vector("list", length(subsets))

  for (i in seq_along(subsets)) {
    predictors <- c("y_lag", x_predictors[subsets[[i]]])
    if (nrow(calibration_data) <= length(predictors) + 1L) next
    fit <- tryCatch(fit_ols_model(calibration_data, predictors), error = function(e) NULL)
    if (is.null(fit)) next
    simulation <- tryCatch(fit$predict(test_data), error = function(e) NULL)
    if (is.null(simulation)) next
    score <- kge_metrics(simulation, test_data$y)$KGE
    if (is.finite(score)) scores[i] <- score
    fits[[i]] <- fit
  }

  if (!any(is.finite(scores))) {
    stop("Nenhuma combinação PARX pôde ser ajustada para este período.")
  }
  best <- which.max(scores)
  list(
    fit = fits[[best]],
    selected = subsets[[best]],
    score = scores[[best]]
  )
}

coefficient_record <- function(coefficients, x_names, max_x = 10L) {
  out <- list(
    CoefB0 = unname(coefficients["(Intercept)"] %||% NA_real_),
    CoefBvazao = unname(coefficients["y_lag"] %||% NA_real_)
  )
  for (i in seq_len(max_x)) {
    out[[paste0("X", i, "_Name")]] <- if (i <= length(x_names)) x_names[i] else NA_character_
    out[[paste0("CoefBX", i)]] <- if (i <= length(x_names)) {
      unname(coefficients[paste0("x", i)] %||% 0)
    } else {
      NA_real_
    }
  }
  as.data.frame(out, check.names = FALSE)
}

error_record <- function(series, k, lag, month, model, fit,
                         simulation, observation, x_names,
                         selected_x = character(), max_x = 10L) {
  cbind(
    data.frame(
      Series = series,
      K = k,
      Lag = lag,
      Month = month,
      Model = model,
      SelectedX = paste(selected_x, collapse = "; "),
      stringsAsFactors = FALSE
    ),
    coefficient_record(fit$coefficients, x_names, max_x),
    all_metrics(simulation, observation)
  )
}

forecast_ispal <- function(var_y, var_x = NULL,
                           models = c("PAR", "PARX", "RIDGE"),
                           forecast_lag = 1:6,
                           forecast_months = 1:12,
                           period_calibration = as.Date(c("1949-01-01", "1990-12-31")),
                           period_validation = as.Date(c("1991-01-01", "2010-12-31")),
                           period_test = as.Date(c("2011-01-01", "2021-12-31")),
                           max_x = 10L,
                           progress = NULL) {
  models <- unique(toupper(models))
  y_names <- setdiff(names(var_y), "Date")
  x_names <- if (is.null(var_x)) character() else setdiff(names(var_x), "Date")
  if (length(x_names) > max_x) stop("Foram selecionadas mais de 10 covariáveis.")

  forecast_rows <- error_rows <- all_error_rows <- lambda_rows <- list()
  warnings <- character()
  total <- length(y_names) * length(forecast_lag) * length(forecast_months)
  step <- 0L

  for (k in seq_along(y_names)) {
    series <- y_names[k]
    for (lag in forecast_lag) {
      for (month in forecast_months) {
        step <- step + 1L
        detail <- paste0(
          series, " · lead ", lag, " · ", month_labels_pt[as.character(month)]
        )
        if (is.function(progress)) progress(step, total, detail)

        par_train_period <- c(
          normalize_period_dates(period_calibration)[1],
          normalize_period_dates(period_validation)[2]
        )
        test_data <- make_period_pairs(
          var_y, var_x, series, x_names,
          period_test, month, lag
        )
        if (nrow(test_data) < 2L) {
          warnings <- c(warnings, paste0("Poucos dados de teste para ", detail, "."))
          next
        }

        base_forecast <- data.frame(
          Series = series,
          K = k,
          Date = test_data$Date,
          Lag = lag,
          Month = month,
          Obs = test_data$y,
          stringsAsFactors = FALSE
        )
        main_error <- all_error <- list()

        if ("PAR" %in% models) {
          par_train <- make_period_pairs(
            var_y, var_x, series, x_names,
            par_train_period, month, lag
          )
          par_fit <- fit_ols_model(par_train, "y_lag")
          par_sim <- par_fit$predict(test_data)
          base_forecast$SimPAR <- par_sim
          rec <- error_record(
            series, k, lag, month, "PAR", par_fit,
            par_sim, test_data$y, x_names, max_x = max_x
          )
          main_error <- c(main_error, list(rec))
          all_error <- c(all_error, list(rec))
        }

        if ("PARX" %in% models) {
          parx_calibration <- make_period_pairs(
            var_y, var_x, series, x_names,
            period_calibration, month, lag
          )
          parx_test <- make_period_pairs(
            var_y, var_x, series, x_names,
            period_validation, month, lag
          )
          x_predictors <- paste0("x", seq_along(x_names))
          selection <- select_parx_model(parx_calibration, parx_test, x_predictors)
          parx_sim <- selection$fit$predict(test_data)
          selected_names <- x_names[selection$selected]
          base_forecast$SimPARX <- parx_sim
          rec <- error_record(
            series, k, lag, month, "PARX", selection$fit,
            parx_sim, test_data$y, x_names, selected_names, max_x
          )
          main_error <- c(main_error, list(rec))
          all_error <- c(all_error, list(rec))

          parx0 <- fit_ols_model(parx_calibration, "y_lag")
          base_forecast$SimPARX0 <- parx0$predict(test_data)
          all_error <- c(all_error, list(error_record(
            series, k, lag, month, "PARX0", parx0,
            base_forecast$SimPARX0, test_data$y, x_names,
            selected_x = character(), max_x = max_x
          )))

          for (i in seq_along(x_predictors)) {
            forced_fit <- fit_ols_model(
              parx_calibration,
              c("y_lag", x_predictors[i])
            )
            forced_name <- paste0("PARX_", safe_name(x_names[i]))
            simulation_name <- paste0("Sim", forced_name)
            base_forecast[[simulation_name]] <- forced_fit$predict(test_data)
            all_error <- c(all_error, list(error_record(
              series, k, lag, month, forced_name, forced_fit,
              base_forecast[[simulation_name]], test_data$y, x_names,
              selected_x = x_names[i], max_x = max_x
            )))
          }
        }

        if ("RIDGE" %in% models) {
          ridge_train <- make_period_pairs(
            var_y, var_x, series, x_names,
            par_train_period, month, lag
          )
          ridge_predictors <- c("y_lag", paste0("x", seq_along(x_names)))
          ridge_fit <- fit_ridge_model(ridge_train, ridge_predictors)
          ridge_sim <- ridge_fit$predict(test_data)
          base_forecast$SimRIDGE <- ridge_sim
          rec <- error_record(
            series, k, lag, month, "RIDGE", ridge_fit,
            ridge_sim, test_data$y, x_names, x_names, max_x
          )
          main_error <- c(main_error, list(rec))
          all_error <- c(all_error, list(rec))
          lambda_rows <- c(lambda_rows, list(data.frame(
            Series = series,
            K = k,
            Lag = lag,
            Month = month,
            Lambda = ridge_fit$lambda,
            stringsAsFactors = FALSE
          )))
        }

        forecast_rows <- c(forecast_rows, list(base_forecast))
        error_rows <- c(error_rows, main_error)
        all_error_rows <- c(all_error_rows, all_error)
      }
    }
  }

  bind_or_empty <- function(rows) {
    if (!length(rows)) data.frame() else dplyr::bind_rows(rows)
  }
  list(
    Forecast.table = bind_or_empty(forecast_rows),
    Error.table = bind_or_empty(error_rows),
    All.error.table = bind_or_empty(all_error_rows),
    Lambda.table = bind_or_empty(lambda_rows),
    warnings = unique(warnings),
    metadata = list(
      y_names = y_names,
      x_names = x_names,
      models = models,
      forecast_lag = forecast_lag,
      forecast_months = forecast_months,
      periods = list(
        calibration = period_calibration,
        validation = period_validation,
        test = period_test
      )
    )
  )
}
