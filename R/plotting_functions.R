normalize_result_columns <- function(data, mapping = list()) {
  if (is.null(data)) return(NULL)
  out <- as.data.frame(data, check.names = FALSE)
  aliases <- list(
    Model = c("Model", "Modelo", "model", "modelo"),
    Series = c("Series", "Série", "Serie", "Variable", "Variavel", "PostoUHE"),
    K = c("K", "k"),
    Lag = c("Lag", "Lead", "lag", "lead"),
    Month = c("Month", "Mes", "Mês", "month", "mes")
  )
  for (target in names(aliases)) {
    explicit <- mapping[[target]]
    candidate <- if (!is.null(explicit) && explicit %in% names(out)) {
      explicit
    } else {
      intersect(aliases[[target]], names(out))[1]
    }
    if (!is.na(candidate) && length(candidate) && candidate != target) {
      names(out)[names(out) == candidate] <- target
    }
  }
  metric_aliases <- list(
    NSE = c("NSE", "NSE.orig"),
    Beta.NSE = c("Beta.NSE"),
    KGE = c("KGE", "KGE.orig"),
    Alpha.KGE = c("Alpha.KGE"),
    Beta.KGE = c("Beta.KGE"),
    r = c("r", "r.KGE", "r.NSE")
  )
  for (target in names(metric_aliases)) {
    if (target %in% names(out)) next
    candidate <- intersect(metric_aliases[[target]], names(out))[1]
    if (length(candidate) && !is.na(candidate)) {
      names(out)[names(out) == candidate] <- target
    }
  }
  out
}

available_metrics <- function(data) {
  if (is.null(data)) return(character())
  metrics <- intersect(recognized_metrics, names(data))
  metrics[vapply(data[metrics], is.numeric, logical(1))]
}

plot_axis_limits <- function(values, ideal) {
  values <- values[is.finite(values)]
  if (!length(values)) return(c(ideal - 1, ideal + 1))
  quantiles <- stats::quantile(values, c(0.01, 0.99), na.rm = TRUE, names = FALSE)
  spread <- diff(quantiles)
  if (!is.finite(spread) || spread == 0) spread <- max(abs(quantiles), 1) * 0.2
  lower <- min(quantiles[1] - 0.12 * spread, ideal)
  upper <- max(quantiles[2] + 0.12 * spread, ideal)
  if (lower == upper) c(lower - 0.5, upper + 0.5) else c(lower, upper)
}

metric_panel <- function(data, metric, model_order = NULL) {
  ideal <- ideal_metric_values()[[metric]]
  label <- metric_plot_label(metric)
  plot_data <- data[
    is.finite(data[[metric]]) & !is.na(data$Model),
    c("Model", metric),
    drop = FALSE
  ]
  names(plot_data)[2] <- "Value"
  if (!nrow(plot_data)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = paste("Sem dados para", label)) +
        ggplot2::theme_void()
    )
  }
  if (is.null(model_order)) {
    preferred <- c("RIDGE", "PARX", "PAR")
    model_order <- c(intersect(preferred, unique(plot_data$Model)),
                     setdiff(unique(plot_data$Model), preferred))
  }
  plot_data$Model <- factor(plot_data$Model, levels = rev(model_order))
  palette <- model_palette(levels(plot_data$Model))
  limits <- plot_axis_limits(plot_data$Value, ideal)

  ridge <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Value, y = Model, fill = Model)
  ) +
    ggridges::geom_density_ridges(
      rel_min_height = 0.01,
      alpha = 0.84,
      scale = 1.7,
      color = "#18202A",
      linewidth = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::geom_vline(
      xintercept = ideal,
      color = "#D83A3A",
      linetype = "dashed",
      linewidth = 0.7
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::coord_cartesian(xlim = limits, expand = FALSE) +
    ggplot2::labs(title = label, x = NULL, y = NULL) +
    ggridges::theme_ridges(font_size = 11, grid = TRUE) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, face = "bold", color = "#132238", size = 14
      ),
      axis.text.y = ggplot2::element_text(color = "#25344A", face = "bold"),
      legend.position = "none",
      plot.margin = grid::unit(c(5, 7, 0, 5), "pt")
    )

  box <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Value, y = Model, fill = Model)
  ) +
    ggplot2::geom_boxplot(
      alpha = 0.88,
      width = 0.58,
      outlier.alpha = 0.45,
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = ideal,
      color = "#D83A3A",
      linetype = "dashed",
      linewidth = 0.55
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::coord_cartesian(xlim = limits, expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = grid::unit(c(0, 7, 5, 5), "pt")
    )

  patchwork::wrap_plots(
    list(ridge, box),
    ncol = 1,
    heights = c(3.5, 1.2)
  )
}

build_metric_distribution_plot <- function(data, metrics,
                                           models = NULL,
                                           series = NULL,
                                           lags = NULL,
                                           months = NULL,
                                           columns = 2L,
                                           variable_context = NULL) {
  data <- normalize_result_columns(data)
  stopifnot("Model" %in% names(data))
  filtered <- data
  if (length(models)) filtered <- filtered[filtered$Model %in% models, , drop = FALSE]
  if (length(series) && "Series" %in% names(filtered)) {
    filtered <- filtered[filtered$Series %in% series, , drop = FALSE]
  }
  if (length(lags) && "Lag" %in% names(filtered)) {
    filtered <- filtered[filtered$Lag %in% lags, , drop = FALSE]
  }
  if (length(months) && "Month" %in% names(filtered)) {
    filtered <- filtered[filtered$Month %in% months, , drop = FALSE]
  }
  metrics <- intersect(metrics, available_metrics(filtered))
  if (!length(metrics)) stop("Nenhuma métrica válida foi selecionada.")
  panels <- lapply(metrics, function(metric) metric_panel(filtered, metric))
  patchwork::wrap_plots(panels, ncol = max(1L, as.integer(columns))) +
    patchwork::plot_annotation(
      title = "Distribuição das métricas por modelo",
      subtitle = paste(
        "A linha vermelha tracejada indica o valor ideal de cada métrica",
        variable_context %||% "",
        sep = if (nzchar(variable_context %||% "")) "\n" else ""
      ),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold", size = 18, color = "#132238"
        ),
        plot.subtitle = ggplot2::element_text(size = 11, color = "#526173")
      )
    )
}

metric_winner_table <- function(data, metrics, aggregate_by = c("Month", "Lag")) {
  aggregate_by <- match.arg(aggregate_by)
  data <- normalize_result_columns(data)
  group_candidates <- intersect(c("Series", "K", "Lag", "Month"), names(data))
  if (!all(c("Model", aggregate_by) %in% names(data))) {
    stop("A tabela não possui as colunas necessárias para calcular modelos vencedores.")
  }
  metrics <- intersect(metrics, available_metrics(data))
  if (!length(metrics)) stop("Nenhuma métrica válida foi selecionada.")
  id_cols <- unique(c(group_candidates, aggregate_by))

  rows <- lapply(metrics, function(metric) {
    ideal <- ideal_metric_values()[[metric]]
    subset <- data[, unique(c(id_cols, "Model", metric)), drop = FALSE]
    names(subset)[names(subset) == metric] <- "Value"
    subset <- subset[is.finite(subset$Value), , drop = FALSE]
    subset$Distance <- abs(subset$Value - ideal)
    grouping <- interaction(subset[id_cols], drop = TRUE, lex.order = TRUE)
    winners <- subset[ave(subset$Distance, grouping, FUN = function(x) x == min(x)) == 1, ]
    winners$Metric <- metric
    winners
  })
  winners <- dplyr::bind_rows(rows)
  counts <- winners |>
    dplyr::group_by(.data[[aggregate_by]], .data$Metric, .data$Model) |>
    dplyr::summarise(Quantity = dplyr::n(), .groups = "drop")
  counts
}

build_winner_plot <- function(data, metrics, aggregate_by = c("Month", "Lag"),
                              variable_context = NULL) {
  aggregate_by <- match.arg(aggregate_by)
  counts <- metric_winner_table(data, metrics, aggregate_by)
  counts$Metric <- factor(
    counts$Metric,
    levels = metrics,
    labels = vapply(metrics, metric_plot_parse_label, character(1))
  )
  if (aggregate_by == "Month") {
    counts[[aggregate_by]] <- factor(
      counts[[aggregate_by]],
      levels = 1:12,
      labels = unname(month_labels_pt)
    )
  } else {
    counts[[aggregate_by]] <- factor(counts[[aggregate_by]])
  }
  palette <- model_palette(unique(counts$Model))
  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = .data[[aggregate_by]], y = .data$Quantity, fill = .data$Model)
  ) +
    ggplot2::geom_col(position = "stack", width = 0.78) +
    ggplot2::facet_wrap(
      ~Metric, scales = "free_y", ncol = 2,
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      title = "Modelo mais próximo do valor ideal",
      subtitle = paste(
        paste("Contagem agregada por", if (aggregate_by == "Month") "mês" else "lead"),
        variable_context %||% "",
        sep = if (nzchar(variable_context %||% "")) "\n" else ""
      ),
      x = if (aggregate_by == "Month") "Mês" else "Lead",
      y = "Quantidade",
      fill = "Modelo"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#132238"),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

build_correlation_plot <- function(x_data, selected_x = NULL,
                                   y_names = character()) {
  if (is.null(x_data)) stop("Não há tabela X disponível.")
  selected_x <- selected_x %||% setdiff(names(x_data), "Date")
  selected_x <- intersect(selected_x, names(x_data))
  if (length(selected_x) < 2L) stop("Selecione ao menos duas covariáveis.")
  matrix_cor <- stats::cor(x_data[selected_x], use = "pairwise.complete.obs")
  long <- as.data.frame(as.table(matrix_cor), stringsAsFactors = FALSE)
  names(long) <- c("X1", "X2", "Correlation")
  ggplot2::ggplot(long, ggplot2::aes(.data$X1, .data$X2, fill = .data$Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$Correlation)),
      size = 3.5
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#2166AC",
      midpoint = 0, limits = c(-1, 1)
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Correlação entre\ncovariáveis",
      subtitle = format_variable_context(
        y_names, selected_x,
        y_fallback = "não utilizada"
      ),
      x = NULL, y = NULL, fill = "Correlação"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
}

build_ccf_plot <- function(y_data, x_data, y_names, x_names, lag_max = 12L) {
  y_names <- intersect(y_names, names(y_data))
  if (!length(y_names)) stop("Selecione ao menos uma variável Y.")
  x_names <- intersect(x_names, names(x_data))
  if (!length(x_names)) stop("Selecione ao menos uma covariável.")
  merged <- merge(
    y_data[, c("Date", y_names), drop = FALSE],
    x_data[, c("Date", x_names), drop = FALSE],
    by = "Date"
  )
  rows <- unlist(lapply(y_names, function(y_name) {
    lapply(x_names, function(x_name) {
      obj <- stats::ccf(
        merged[[y_name]], merged[[x_name]],
        lag.max = lag_max, plot = FALSE, na.action = na.omit
      )
      data.frame(
        Y = y_name,
        X = x_name,
        Lag = as.numeric(obj$lag),
        Correlation = as.numeric(obj$acf)
      )
    })
  }), recursive = FALSE)
  data <- dplyr::bind_rows(rows)
  threshold <- 2 / sqrt(nrow(merged))
  ggplot2::ggplot(data, ggplot2::aes(.data$Lag, .data$Correlation)) +
    ggplot2::geom_hline(yintercept = 0, color = "#4B5563") +
    ggplot2::geom_hline(
      yintercept = c(-threshold, threshold),
      linetype = "dashed", color = "#2563EB"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$Lag, y = 0, yend = .data$Correlation),
      color = "#1F7A76", linewidth = 0.7
    ) +
    ggplot2::facet_grid(Y ~ X, scales = "free_y") +
    ggplot2::labs(
      title = "Correlação cruzada entre Y e X",
      subtitle = paste(
        "Linhas azuis: intervalo aproximado ±2/√N",
        format_variable_context(y_names, x_names),
        sep = "\n"
      ),
      x = "Lag", y = "Correlação cruzada"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
